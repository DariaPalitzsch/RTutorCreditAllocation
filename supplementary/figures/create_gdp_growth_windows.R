# GDP-growth windows around a credit expansion
# for Chapter 4

library(ggplot2)

# Example years used to illustrate the three-year windows
credit_endpoint <- 2000L
window_length <- 3L
horizons <- 0:5
windows <- data.frame(
  label = c("Credit change\n(fixed window)",
            paste0("GDP growth: h = ", horizons, "\n(gdp_3yr_h", horizons, ")")),
  row = 7:1,
  start = c(credit_endpoint - window_length,
            credit_endpoint + horizons - window_length),
  end = c(credit_endpoint, credit_endpoint + horizons),
  category = c("Credit", rep("GDP", length(horizons))),
  relation = c("", "Coinciding", "Overlapping", "Overlapping",
               "First fully subsequent", "Later window", "Later window")
)
windows$dates <- paste(windows$start, windows$end, sep = "\u2013")
year_breaks <- seq(min(windows$start), max(windows$end))

timing_plot <- ggplot(windows) +
  geom_vline(xintercept = year_breaks, colour = "#E4E7EA", linewidth = 0.35) +
  geom_vline(xintercept = credit_endpoint, colour = "#68737D", linewidth = 0.6,
             linetype = "dashed") +
  geom_rect(aes(xmin = start, xmax = end, ymin = row - 0.16,
                ymax = row + 0.16, fill = category), colour = NA) +
  geom_text(aes(x = (start + end) / 2, y = row, label = dates),
            colour = "white", size = 4.2, family = "Arial") +
  geom_text(data = subset(windows, category == "GDP"),
            aes(x = (start + end) / 2, y = row - 0.37, label = relation),
            colour = "#343B42", size = 3.8, family = "Arial") +
  annotate("text", x = credit_endpoint, y = 7.43,
           label = paste0("t = ", credit_endpoint),
           size = 4.2, colour = "#343B42", family = "Arial") +
  scale_fill_manual(values = c(Credit = "#35658D", GDP = "#97511C"),
                    guide = "none") +
  scale_x_continuous(breaks = year_breaks,
                     limits = range(year_breaks) + c(-0.18, 0.18),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(breaks = windows$row, labels = windows$label,
                     limits = c(0.32, 7.58), expand = expansion(mult = 0)) +
  labs(title = "Timing GDP growth around a credit expansion",
       subtitle = "The credit window stays fixed; each GDP window covers three years.",
       x = "Year", y = NULL,
       caption = "h indicates how many years the GDP-growth window moves forward.") +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 11.5, colour = "#242A30", hjust = 1,
                               margin = margin(r = 16)),
    axis.text.x = element_text(size = 12, colour = "#242A30",
                               margin = margin(t = 6)),
    axis.ticks.x = element_line(colour = "#78818A", linewidth = 0.4),
    axis.ticks.length = grid::unit(4, "pt"),
    axis.line.x = element_line(colour = "#78818A", linewidth = 0.4),
    axis.title.x = element_text(size = 12, margin = margin(t = 8)),
    plot.title = element_text(size = 19, face = "bold", colour = "#242A30",
                              margin = margin(b = 10)),
    plot.subtitle = element_text(size = 13, colour = "#49525B",
                                 margin = margin(b = 18)),
    plot.caption = element_text(size = 11, colour = "#49525B", hjust = 0,
                                margin = margin(t = 16)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.margin = margin(18, 24, 16, 14),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# Save the figure used in the problem set
ggsave("gdp_growth_windows.png", timing_plot,
       width = 10.8, height = 7.3, units = "in", dpi = 160, bg = "white")
