# -----------------------------
# Load data
# -----------------------------
df <- read.csv("regression_matrix.csv")

# -----------------------------
# Average MAT per year
# -----------------------------
library(dplyr)

yearly_avg <- df %>%
  group_by(Year) %>%
  summarise(AvgMAT = mean(MAT, na.rm = TRUE)) %>%
  arrange(Year) %>%
  mutate(DiffMAT = c(NA, diff(AvgMAT)))  # year-to-year differences

# -----------------------------
# Save general year differences
# -----------------------------
write.csv(yearly_avg, "yearly_MAT_differences.csv", row.names = FALSE)

# -----------------------------
# Print only positive differences (warming), rounded to 1 sig fig
# -----------------------------
warming <- yearly_avg %>%
  filter(!is.na(DiffMAT) & DiffMAT > 0) %>%
  mutate(Rounded = signif(DiffMAT, digits = 1))

print("Warming by Year (Rounded to 1 significant figure):")
print(warming[, c("Year", "Rounded")])
write.csv(warming, "yearly_MAT_warming.csv", row.names = FALSE)

# -----------------------------
# Repeat for each Forest.type
# -----------------------------
forest_diffs <- df %>%
  group_by(Forest.type, Year) %>%
  summarise(AvgMAT = mean(MAT, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Forest.type, Year) %>%
  group_by(Forest.type) %>%
  mutate(DiffMAT = c(NA, diff(AvgMAT))) %>%
  ungroup()

# Save all differences
write.csv(forest_diffs, "forest_yearly_MAT_differences.csv", row.names = FALSE)

# -----------------------------
# Print warming years per forest type
# -----------------------------
warming_forest <- forest_diffs %>%
  filter(!is.na(DiffMAT) & DiffMAT > 0) %>%
  mutate(Rounded = signif(DiffMAT, digits = 1))

print("Warming by Year per Forest Type (Rounded):")
print(warming_forest[, c("Forest.type", "Year", "Rounded")])
write.csv(warming_forest, "yearly_T5_warming.csv", row.names = FALSE)