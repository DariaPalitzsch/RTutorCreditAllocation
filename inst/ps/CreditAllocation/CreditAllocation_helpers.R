# Shared analysis helpers for the Credit Allocation problem set.
# Embedded by RTutor through create.ps(extra.code.file = ...).
# The first visible definition remains in the relevant learning exercise.
# All estimators receive their dataset explicitly.

winsorize <- function(x) {
  limits <- stats::quantile(
    x,
    probs = c(0.01, 0.99),
    na.rm = TRUE,
    type = 1
  )
  pmin(pmax(x, limits[1]), limits[2])
}

calendar_lead <- function(x, year, h) {
  target_row <- match(year + h, year)
  x[target_row]
}

calendar_growth <- function(x, year, h, window = 3) {
  end_value <- calendar_lead(x, year, h)
  start_value <- calendar_lead(x, year, h - window)
  end_value - start_value
}

calendar_lag <- function(x, year, h) {
  target_row <- match(year - h, year)
  x[target_row]
}

estimate_local_projection <- function(
    local_data,
    h,
    panel = c("A", "B"),
    fixed_effects = "country") {
  
  panel <- match.arg(panel)
  outcome_name <- paste0("cumulative_gdp_h", h)
  
  sector_credit <- c("D1NT_GDP_w", "D1T_GDP_w")
  panel_credit <- if (panel == "A") {
    sector_credit
  } else {
    c(sector_credit, "D1HH_GDP")
  }
  
  regressor_names <- c(
    panel_credit,
    unlist(
      lapply(panel_credit, function(x) paste0(x, "_lag", 1:5)),
      use.names = FALSE
    ),
    "annual_gdp_growth",
    paste0("annual_gdp_growth_lag", 1:5)
  )
  
  estimation_data <- dplyr::filter(local_data, smp_GDP == 1)
  estimation_data <- dplyr::select(
    estimation_data,
    country,
    year,
    dplyr::all_of(c(outcome_name, regressor_names))
  )
  estimation_data <- estimation_data[
    stats::complete.cases(estimation_data),
  ]
  
  model_formula <- stats::as.formula(paste(
    outcome_name, "~", paste(regressor_names, collapse = " + "),
    "|", fixed_effects
  ))
  
  local_projection_model <- fixest::feols(
    model_formula,
    data = estimation_data,
    panel.id = ~country + year,
    vcov = fixest::DK(ceiling(1.5 * h)),
    notes = FALSE
  )
  
  results <- broom::tidy(
    local_projection_model,
    conf.int = TRUE,
    conf.level = 0.95
  )
  results <- dplyr::filter(results, term %in% panel_credit)
  
  dplyr::mutate(
    results,
    horizon = h,
    panel = panel,
    observations = stats::nobs(local_projection_model),
    countries = length(fixest::fixef(local_projection_model)$country)
  )
}

estimate_horizon_model <- function(dataset, outcome, h) {
  regression_formula <- stats::as.formula(paste(
    outcome,
    "~ D3T_GDP_w + D3NT_GDP_w + D3HH_GDP_w | country"
  ))
  
  fixest::feols(
    regression_formula,
    data = dataset,
    panel.id = ~country + year,
    vcov = fixest::DK(ceiling(1.5 * (3 + h)))
  )
}

future_crisis_window <- function(x, year, h) {
  future_values <- lapply(seq_len(h), function(step) {
    calendar_lead(x, year, step)
  })

  complete_window <- Reduce(
    `&`, lapply(future_values, function(value) !is.na(value))
  )
  crisis_in_window <- Reduce(
    `|`, lapply(future_values, function(value) value == 1)
  )

  dplyr::if_else(
    complete_window,
    as.integer(crisis_in_window),
    NA_integer_
  )
}

estimate_alternative_split <- function(
    h,
    split = c("mortgage", "small_firm"),
    dataset) {
  
  split <- match.arg(split)
  
  credit_terms <- switch(
    split,
    mortgage = c(
      "D3HighMortgageShare_GDP_w",
      "D3LowMortgageShare_GDP_w"
    ),
    small_firm = c(
      "D3HighSmallFirmShare_GDP_w",
      "D3LowSmallFirmShare_GDP_w"
    )
  )
  
  model_formula <- stats::as.formula(
    paste0(
      "gdp_3yr_h", h, " ~ ",
      paste(credit_terms, collapse = " + "),
      " | country"
    )
  )
  
  fixest::feols(
    model_formula,
    data = dataset,
    subset = ~smp_GDP == 1 & !is.na(D3HH_GDP_w),
    panel.id = ~country + year,
    vcov = fixest::DK(ceiling(1.5 * (3 + h))),
    notes = FALSE
  )
}

