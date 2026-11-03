# Local projections for tradable and non-tradable sector credit
# for Chapter 6


library(dplyr)
library(ggplot2)
library(fixest)
library(broom)

data <- readRDS("Data.rds")

# Winsorize GDP growth at the 1st and 99th percentiles
winsorize <- function(x) {
  limits <- quantile(x, probs = c(0.01, 0.99), na.rm = TRUE, type = 1)
  pmin(pmax(x, limits[1]), limits[2])
}

# Match calendar years so that gaps in the panel are not counted as one year
calendar_lag <- function(x, year, h) {
  x[match(year - h, year)]
}

data <- data %>%
  mutate(
    annual_gdp_growth = winsorize(annual_gdp_growth),
    across(cumulative_gdp_h1:cumulative_gdp_h10, winsorize)
  ) %>%
  group_by(id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    across(
      c(D1NT_GDP_w, D1T_GDP_w, annual_gdp_growth),
      list(
        lag1 = ~calendar_lag(.x, year, 1),
        lag2 = ~calendar_lag(.x, year, 2),
        lag3 = ~calendar_lag(.x, year, 3),
        lag4 = ~calendar_lag(.x, year, 4),
        lag5 = ~calendar_lag(.x, year, 5)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  ungroup()

# Panel A: sectoral credit and GDP growth, including five annual lags
regressors <- c(
  "D1NT_GDP_w", "D1T_GDP_w",
  paste0("D1NT_GDP_w_lag", 1:5),
  paste0("D1T_GDP_w_lag", 1:5),
  "annual_gdp_growth",
  paste0("annual_gdp_growth_lag", 1:5)
)

results <- list()

for (h in 1:10) {
  outcome <- paste0("cumulative_gdp_h", h)

  estimation_data <- data %>%
    filter(smp_GDP == 1) %>%
    select(id, year, all_of(c(outcome, regressors))) %>%
    filter(complete.cases(.))

  model_formula <- as.formula(
    paste(outcome, "~", paste(regressors, collapse = " + "), "| id")
  )

  model <- feols(
    model_formula,
    data = estimation_data,
    panel.id = ~id + year,
    vcov = DK(ceiling(1.5 * h))
  )

  results[[h]] <- tidy(model, conf.int = TRUE, conf.level = 0.95) %>%
    filter(term %in% c("D1NT_GDP_w", "D1T_GDP_w")) %>%
    mutate(horizon = h)
}

preview_results <- bind_rows(results) %>%
  mutate(
    credit_category = factor(
      term,
      levels = c("D1NT_GDP_w", "D1T_GDP_w"),
      labels = c("Non-tradable sector credit", "Tradable sector credit")
    )
  )

# Plot estimates and 95% confidence intervals
preview_plot <- ggplot(
  preview_results,
  aes(x = horizon, y = estimate, group = credit_category)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.15,
    linewidth = 0.7,
    color = "black"
  ) +
  geom_line(linewidth = 0.9, color = "#1F6EAA") +
  geom_point(size = 2.8, color = "#1F6EAA") +
  facet_wrap(~credit_category, nrow = 1) +
  scale_x_continuous(breaks = 1:10) +
  labs(
    x = "Years after the annual credit change",
    y = "Cumulative real GDP-growth association (log points)",
    title = "Preview: dynamic GDP paths after sectoral credit changes",
    subtitle = "Points are estimates; bars are 95% confidence intervals"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 29),
    plot.subtitle = element_text(size = 22, margin = margin(b = 16)),
    axis.title = element_text(size = 21),
    strip.text = element_text(size = 18),
    panel.grid.minor = element_blank(),
    panel.spacing = grid::unit(0.2, "in")
  )

ggsave(
  "local_projection_preview.png",
  plot = preview_plot,
  width = 19.8,
  height = 9,
  units = "in",
  dpi = 100,
  bg = "white"
)
