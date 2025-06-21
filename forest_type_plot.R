df <- read.csv("regression_matrix.csv")

library(ggplot2)
library(dplyr)

df$Forest.type <- as.factor(df$Forest.type)

# Example data prep (adjust column names as needed)
df_summary <- df %>%
  group_by(Forest.type) %>%
  summarize(
    Mean_Rs_change = mean(Rs, na.rm = TRUE),  # Replace with actual % change column
    Warming_years = n_distinct(Year)           # Count years with data per forest
  )

# Plot
ggplot(df_summary, aes(x = reorder(Forest.type, Mean_Rs_change), 
                       y = Mean_Rs_change, 
                       fill = Warming_years)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_c(name = "Warming Years") +  # Use viridis for colorblind-friendly gradient
  labs(x = "Forest Type", y = "% Change in Rs", 
       title = "Soil Respiration Change by Forest Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))













library(ggplot2)
library(dplyr)


df_summary <- df %>%
  group_by(Forest.type) %>%
  summarize(
    Mean_Rs_change = mean(Rs, na.rm = TRUE),
    Warming_years = n_distinct(Year[!is.na(Rs)])  # Count years with non-NA Rs
  ) %>%
  mutate(
    Warming_category = ifelse(Warming_years == 0, "No warming", as.character(Warming_years)),
    Warming_category = factor(Warming_category, 
                             levels = c("No warming", sort(unique(Warming_years[Warming_years > 0])))
  )


custom_palette <- c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")
no_warming_color <- "#999999"  # Gray for "No warming" (change if needed)


warm_years <- sort(unique(df_summary$Warming_years[df_summary$Warming_years > 0]))
color_values <- setNames(
  c(no_warming_color, rep(custom_palette, length.out = length(warm_years))),
  c("No warming", warm_years)
)


ggplot(df_summary, aes(x = reorder(Forest.type, Mean_Rs_change), 
                       y = Mean_Rs_change, 
                       fill = Warming_category)) +
  geom_bar(stat = "identity", width = 0.7, color = "white", linewidth = 0.3) +  # White borders for clarity
  scale_fill_manual(
    name = "Warming Years",
    values = color_values  # Uses your palette + gray
  ) +
  labs(x = "Forest Type", y = "% Change in Rs") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    panel.grid.major = element_blank(),  # No grid lines
    panel.grid.minor = element_blank(),
    legend.position = "right",          # Legend on right
    plot.background = element_rect(fill = "white", color = NA)  # Clean white background
  )
















# LAtest one
library(ggplot2)
library(dplyr)
library(viridis)
library(readr)

# Read your warming data
warming_df <- read_csv("forest_yearly_MAT_differences.csv")

# Summarize warming years
warming_summary <- warming_df %>%
  group_by(Forest.type) %>%
  summarize(Warming_years = sum(!is.na(DiffMAT)), .groups = 'drop') %>%
  mutate(Warming_label = ifelse(Warming_years == 0, "No warming", as.character(Warming_years)))

# Rs summary
df_summary <- df %>%
  group_by(Forest.type) %>%
  summarize(Mean_Rs_change = mean(Rs, na.rm = TRUE), .groups = 'drop')

# Convert to character to allow joining
df_summary$Forest.type <- as.character(df_summary$Forest.type)
warming_summary$Forest.type <- as.character(warming_summary$Forest.type)

# Join
df_summary <- left_join(df_summary, warming_summary, by = "Forest.type")

# Create factor for plotting (ordered: "No warming", then 1–N)
df_summary$Warming_label <- factor(
  df_summary$Warming_label,
  levels = c("No warming", sort(unique(df_summary$Warming_label[df_summary$Warming_label != "No warming"])))
)

# Create color palette
num_levels <- length(levels(df_summary$Warming_label)) - 1  # Exclude "No warming"
colors <- c(
  "No warming" = "gray60",
  setNames(viridis(num_levels), levels(df_summary$Warming_label)[-1])
)

# Plot
ggplot(df_summary, aes(x = reorder(Forest.type, Mean_Rs_change),
                       y = Mean_Rs_change,
                       fill = Warming_label)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  scale_fill_manual(
    values = colors,
    name = "Warming Years",
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      order = 1
    )
  ) +
  labs(
    x = "Forest Type", y = "% Change in Rs",
    title = "Mean Rs Change by Forest Type and Warming Duration"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.spacing.y = unit(0.5, "cm")
  )

ggsave("CW_Forest_types.tiff",
  device = "tiff", dpi = 600,
  width = 10, height = 6,
  bg = "white", compression = "lzw")






# SW
library(ggplot2)
library(dplyr)
library(viridis)
library(readr)

# Read your warming data
warming_df <- read_csv("forest_yearly_T5_differences.csv")

# Summarize warming years
warming_summary <- warming_df %>%
  group_by(Forest.type) %>%
  summarize(Warming_years = sum(!is.na(DiffT5)), .groups = 'drop') %>%
  mutate(Warming_label = ifelse(Warming_years == 0, "No warming", as.character(Warming_years)))

# Rs summary
df_summary <- df %>%
  group_by(Forest.type) %>%
  summarize(Mean_Rs_change = mean(Rs, na.rm = TRUE), .groups = 'drop')

# Convert to character to allow joining
df_summary$Forest.type <- as.character(df_summary$Forest.type)
warming_summary$Forest.type <- as.character(warming_summary$Forest.type)

# Join
df_summary <- left_join(df_summary, warming_summary, by = "Forest.type")

# Create factor for plotting (ordered: "No warming", then 1–N)
df_summary$Warming_label <- factor(
  df_summary$Warming_label,
  levels = c("No warming", sort(unique(df_summary$Warming_label[df_summary$Warming_label != "No warming"])))
)

# Create color palette
num_levels <- length(levels(df_summary$Warming_label)) - 1  # Exclude "No warming"
colors <- c(
  "No warming" = "gray60",
  setNames(viridis(num_levels), levels(df_summary$Warming_label)[-1])
)

# Plot
ggplot(df_summary, aes(x = reorder(Forest.type, Mean_Rs_change),
                       y = Mean_Rs_change,
                       fill = Warming_label)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  scale_fill_manual(
    values = colors,
    name = "Soil Warming Years",
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      order = 1
    )
  ) +
  labs(
    x = "Forest Type", y = "% Change in Rs",
    title = "Mean Rs Change by Forest Type and Warming Duration"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.spacing.y = unit(0.5, "cm")
  )

ggsave("SW_Forest_types.tiff",
  device = "tiff", dpi = 600,
  width = 10, height = 6,
  bg = "white", compression = "lzw")

























#Very Last one I used
library(ggplot2)
library(dplyr)
library(viridis)
library(readr)

# === 1. Load warming data ===
warming_df <- read_csv("forest_yearly_T5_differences.csv")

warming_summary <- warming_df %>%
  group_by(Forest.type) %>%
  summarize(Warming_years = sum(!is.na(DiffT5)), .groups = 'drop')

# === 2. Load Rs data ===
rs_df <- read_csv("forest_yearly_Rs_differences.csv")

rs_summary <- rs_df %>%
  group_by(Forest.type) %>%
  summarize(Mean_Rs_change = mean(DiffRs, na.rm = TRUE), .groups = 'drop')

# === 3. Merge both ===
warming_summary$Forest.type <- as.character(warming_summary$Forest.type)
rs_summary$Forest.type <- as.character(rs_summary$Forest.type)

df_summary <- full_join(rs_summary, warming_summary, by = "Forest.type") %>%
  mutate(
    # Treat missing values correctly
    Warming_years = ifelse(is.na(Warming_years), 0, Warming_years),
    Mean_Rs_change = ifelse(is.na(Mean_Rs_change), 0, Mean_Rs_change),  # <- CRUCIAL FIX
    Warming_label = ifelse(Warming_years == 0, "No warming", as.character(Warming_years))
  )

# === 4. Order forest types by Warming_years ascending ===
df_summary <- df_summary %>%
  arrange(Warming_years, Forest.type) %>%
  mutate(Forest.type = factor(Forest.type, levels = unique(Forest.type)))

# === 5. Set Warming_label factor ===
df_summary$Warming_label <- factor(
  df_summary$Warming_label,
  levels = c("No warming", sort(unique(df_summary$Warming_label[df_summary$Warming_label != "No warming"])))
)

# === 6. Build color palette ===
num_levels <- length(levels(df_summary$Warming_label)) - 1
colors <- c(
  "No warming" = "gray60",
  setNames(viridis(num_levels), levels(df_summary$Warming_label)[-1])
)

# === 7. Plot ===
ggplot(df_summary, aes(x = Forest.type, y = Mean_Rs_change, fill = Warming_label)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  scale_fill_manual(
    values = colors,
    name = "Soil Warming Years",
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    x = "Forest Type",
    y = "% Change in Rs"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right",
    legend.spacing.y = unit(0.5, "cm"),
    panel.grid = element_blank(),      # remove grid lines
    plot.title = element_blank()       # remove title
  )






























#LATEST
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)

# === 1. Load Data ===
t5_df <- read_csv("forest_yearly_T5_differences.csv")
mat_df <- read_csv("forest_yearly_MAT_differences.csv")
rs_df  <- read_csv("forest_yearly_Rs_differences.csv")

# === 2. Summarize warming ===
t5_summary <- t5_df %>%
  group_by(Forest.type) %>%
  summarize(Warming_years = sum(DiffT5 > 0, na.rm = TRUE), .groups = 'drop')

mat_summary <- mat_df %>%
  group_by(Forest.type) %>%
  summarize(MAT_warming = sum(DiffMAT > 0, na.rm = TRUE), .groups = 'drop')

rs_summary <- rs_df %>%
  group_by(Forest.type) %>%
  summarize(Mean_Rs_change = mean(DiffRs, na.rm = TRUE), .groups = 'drop')

# === 3. Merge all ===
t5_summary$Forest.type <- as.character(t5_summary$Forest.type)
mat_summary$Forest.type <- as.character(mat_summary$Forest.type)
rs_summary$Forest.type <- as.character(rs_summary$Forest.type)

df_summary <- full_join(t5_summary, mat_summary, by = "Forest.type") %>%
  full_join(rs_summary, by = "Forest.type") %>%
  mutate(
    Warming_years    = ifelse(is.na(Warming_years), 0, Warming_years),
    MAT_warming      = ifelse(is.na(MAT_warming), 0, MAT_warming),
    Mean_Rs_change   = ifelse(is.na(Mean_Rs_change), 0, Mean_Rs_change)
  )

# === 4. Define warming type symbols ===
df_summary <- df_summary %>%
  mutate(
    Warming_Type = case_when(
      Warming_years > 0 & MAT_warming > 0 ~ "Both",
      Warming_years > 0 & MAT_warming == 0 ~ "SW only",
      Warming_years == 0 & MAT_warming > 0 ~ "CW only",
      TRUE ~ "None"
    ),
    Symbol = case_when(
      Warming_Type == "SW only" ~ "♢",
      Warming_Type == "CW only" ~ "☀",
      Warming_Type == "Both"    ~ "♢☀",
      TRUE                      ~ ""
    )
  )

# === 5. Set forest order ===
desired_order <- c(1,3,4,6,7,8,9,11,12,14,16,18,19,20,21,22,23,24,25,26,27,30,31,32,2,5,10,13,15,17,28,29)
df_summary <- df_summary %>%
  mutate(Forest.type = factor(Forest.type, levels = as.character(desired_order)))

# === 6. Apply fixed custom palette ===
# Use gray for "0" and your 5 custom colors for 1–5 warming years
custom_palette <- c(
  "0" = "gray60",
  "1" = "#003049",
  "2" = "#d62828",
  "3" = "#f77f00",
  "4" = "#fcbf49",
  "5" = "#eae2b7"
)

# Warn if any warming years exceed the palette
extra_years <- setdiff(as.character(unique(df_summary$Warming_years)), names(custom_palette))
if (length(extra_years) > 0) {
  warning("Warming years found without color mapping: ", paste(extra_years, collapse = ", "))
}

df_summary <- df_summary %>%
  mutate(Warming_years = as.character(Warming_years))  # for fill scale

# === 7. Create Plot ===

plot <- ggplot(df_summary, aes(x = Forest.type, y = Mean_Rs_change, fill = Warming_years)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_text(
    aes(label = Symbol),
    vjust = ifelse(df_summary$Mean_Rs_change >= 0, -0.5, 1.2),
    size = 5
  ) +
  scale_fill_manual(
    values = custom_palette,
    name = "Warming Years"
  ) +
  labs(
    x = "Forest Type",
    y = "% Change in Rs"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(),
    legend.title = element_text(),
    legend.text = element_text(),
    legend.position = "bottom",  # ✅ Moves Warming Years legend below the plot
    legend.spacing.y = unit(0.5, "cm"),
    panel.grid = element_blank(),
    plot.title = element_blank()
  )

# === 3. Create Separate SW & CW Legend Below Main Legend ===
library(gridExtra)  # Ensure gridExtra is loaded

# ✅ Create the SW & CW legend as a separate plot
sw_cw_legend <- ggplot() +
  annotate("text", x = 0, y = 1, label = "♢ = SW only\n☀ = CW only", size = 5, hjust = 0) +
  theme_void()  # Removes background

# ✅ Reduce height ratio so SW & CW legend moves up
grid.arrange(plot, sw_cw_legend, ncol = 1, heights = c(3.5, 0.5))  # ✅ Moves SW & CW closer

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(gridExtra)
library(grid)

# [Your full plot construction code from earlier here, unchanged]
# (Includes df_summary creation and this line at the end):
# plot <- ggplot(...) + theme(...)

# === 1. Custom SW/CW legend below ===
sw_cw_legend <- ggplot() +
  annotate("text", x = 0, y = 1, label = "♢ = SW only    ☀ = CW only", size = 5, hjust = 0) +
  theme_void()

# === 2. Combine both using arrangeGrob ===
full_plot <- arrangeGrob(
  plot,
  sw_cw_legend,
  ncol = 1,
  heights = c(4, 0.4)  # Adjust this ratio to balance size
)

# === 3. Save full combined plot ===
ggsave("plot_output.tiff", plot = full_plot, device = "tiff", dpi = 600, width = 10, height = 7, units = "in", compression = "lzw")
ggsave("plot_output.pdf", plot = full_plot, device = "pdf", width = 10, height = 7, units = "in")
