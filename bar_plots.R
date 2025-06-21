library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)

# Custom color palette (just two colors now)
custom_palette <- c("MAT" = "#fcbf49",  # Orange for MAT
                    "Rs" = "#003049")   # Dark blue for Rs

# Read and prepare data
mat_data <- read.csv("yearly_MAT_differences.csv") %>% 
  mutate(Variable = "MAT",
         PercDiff = (DiffMat / lag(AvgMAT)) * 100) %>% 
  filter(!is.na(PercDiff))

rs_data <- read.csv("yearly_Rs_differences.csv") %>% 
  mutate(Variable = "Rs",
         PercDiff = (DiffRs / lag(AvgRs)) * 100) %>% 
  filter(!is.na(PercDiff))

# Combine data
combined_data <- bind_rows(mat_data, rs_data) %>% 
  mutate(Year = as.integer(Year)) # Ensure years are whole numbers

# Create plot
ggplot(combined_data, aes(x = factor(Year), y = PercDiff, fill = Variable)) +
  
  # Grouped bars for MAT and Rs
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  
  # Use two custom colors
  scale_fill_manual(values = custom_palette) +

  # Zero line
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
  
  # Axes labels
  labs(y = "Annual Change (%)",
       x = "Year") +
  
  # Theme adjustments to remove grid and title
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_blank(),
    axis.line = element_line(color = "gray30"),
    axis.text.x = element_text(angle = 0),
    legend.position = "bottom"
  ) +
  
  # X-axis as years
  scale_x_discrete(breaks = unique(combined_data$Year)) +
  
  # Clean legend
  guides(fill = guide_legend(title = NULL, override.aes = list(alpha = 1)))

# Save outputs
ggsave("MAT_Rs_Comparison.tiff", 
       device = "tiff", dpi = 600, 
       width = 10, height = 6,
       bg = "white", compression = "lzw")

ggsave("MAT_Rs_Comparison.pdf",
       device = cairo_pdf,
       width = 10, height = 6,
       bg = "white")







library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)

# Custom color palette (just two colors now)
custom_palette <- c("T5" = "#fcbf49",  # Orange for MAT
                    "Rs" = "#003049")   # Dark blue for Rs

# Read and prepare data
t5_data <- read.csv("yearly_T5_differences.csv") %>% 
  mutate(Variable = "T5",
         PercDiff = (DiffT5 / lag(AvgT5)) * 100) %>% 
  filter(!is.na(PercDiff))

rs_data <- read.csv("yearly_Rs_differences.csv") %>% 
  mutate(Variable = "Rs",
         PercDiff = (DiffRs / lag(AvgRs)) * 100) %>% 
  filter(!is.na(PercDiff))

# Combine data
combined_data <- bind_rows(t5_data, rs_data) %>% 
  mutate(Year = as.integer(Year)) # Ensure years are whole numbers

# Create plot
ggplot(combined_data, aes(x = factor(Year), y = PercDiff, fill = Variable)) +
  
  # Grouped bars for T5 and Rs
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  
  # Use two custom colors
  scale_fill_manual(values = custom_palette) +

  # Zero line
  geom_hline(yintercept = 0, color = "gray50", linewidth = 0.3) +
  
  # Axes labels
  labs(y = "Annual Change (%)",
       x = "Year") +
  
  # Theme adjustments to remove grid and title
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_blank(),
    axis.line = element_line(color = "gray30"),
    axis.text.x = element_text(angle = 0),
    legend.position = "bottom"
  ) +
  
  # X-axis as years
  scale_x_discrete(breaks = unique(combined_data$Year)) +
  
  # Clean legend
  guides(fill = guide_legend(title = NULL, override.aes = list(alpha = 1)))

# Save outputs
ggsave("MAT_Rs_Comparison.tiff", 
       device = "tiff", dpi = 600, 
       width = 10, height = 6,
       bg = "white", compression = "lzw")

ggsave("MAT_Rs_Comparison.pdf",
       device = cairo_pdf,
       width = 10, height = 6,
       bg = "white")
