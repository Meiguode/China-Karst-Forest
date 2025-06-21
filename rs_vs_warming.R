library(ggplot2)
library(dplyr)
library(readr)

custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")

rs_data <- read_csv("yearly_Rs_differences.csv")
mat_data <- read_csv("yearly_MAT_warming.csv")
t5_data  <- read_csv("yearly_T5_warming.csv")

rs_data <- rs_data %>%
  mutate(Rs_percent_change = (DiffRs / AvgRs) * 100)

cw_data <- rs_data %>%
  inner_join(mat_data, by = "Year") %>%
  mutate(Source = "CW")

sw_data <- rs_data %>%
  inner_join(t5_data, by = "Year") %>%
  mutate(Source = "SW")

plot_data <- bind_rows(cw_data, sw_data)

p <- ggplot(plot_data, aes(x = Rounded, y = Rs_percent_change, color = Source)) +
  geom_point(size = 3, alpha = 0.6, stroke = 0) +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +
  scale_color_manual(values = custom_palette[1:2]) +
  labs(
    x = "Warming (°C)",
    y = "ΔRs(%)",
    color = "Warming Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill= "white", color = NA)
  )

ggsave("Rs_vs_Warming.tiff", plot = p, dpi = 600, width = 7, height = 5, units = "in", compression = "lzw")
ggsave("Rs_vs_Warming.pdf",  plot = p, dpi = 600, width = 7, height = 5, units = "in")
