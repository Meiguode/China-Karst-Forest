df <- read.csv("regression_matrix.csv")

library(dplyr)
#-----------------MAT---------------------
yearly_avg <- df %>%
  group_by(Year) %>%
  summarise(AvgMAT = mean(MAT, na.rm = TRUE)) %>%
  arrange(Year) %>%
  mutate(DiffMAT = c(NA, diff(AvgMAT)))  # year-to-year differences

write.csv(yearly_avg, "yearly_MAT_differences.csv", row.names = FALSE)

warming <- yearly_avg %>%
  filter(!is.na(DiffMAT) & DiffMAT > 0) %>%
  mutate(Rounded = signif(DiffMAT, digits = 1))

print("Warming by Year (Rounded to 1 significant figure):")
print(warming[, c("Year", "Rounded")])
write.csv(warming, "yearly_MAT_warming.csv", row.names = FALSE)

forest_diffs <- df %>%
  group_by(Forest.type, Year) %>%
  summarise(AvgMAT = mean(MAT, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Forest.type, Year) %>%
  group_by(Forest.type) %>%
  mutate(DiffMAT = c(NA, diff(AvgMAT))) %>%
  ungroup()

write.csv(forest_diffs, "forest_yearly_MAT_differences.csv", row.names = FALSE)

warming_forest <- forest_diffs %>%
  filter(!is.na(DiffMAT) & DiffMAT > 0) %>%
  mutate(Rounded = signif(DiffMAT, digits = 1))

print("Warming by Year per Forest Type (Rounded):")
print(warming_forest[, c("Forest.type", "Year", "Rounded")])
write.csv(warming_forest, "yearly_MAT_warming.csv", row.names = FALSE)

#-------------T5-----------------------
yearly_avg <- df %>%
  group_by(Year) %>%
  summarise(AvgT5 = mean(T5, na.rm = TRUE)) %>%
  arrange(Year) %>%
  mutate(DiffMAT = c(NA, diff(AvgT5)))  # year-to-year differences

write.csv(yearly_avg, "yearly_T5_differences.csv", row.names = FALSE)

warming <- yearly_avg %>%
  filter(!is.na(DiffT5) & DiffT5 > 0) %>%
  mutate(Rounded = signif(DiffT5, digits = 1))

print("Warming by Year (Rounded to 1 significant figure):")
print(warming[, c("Year", "Rounded")])
write.csv(warming, "yearly_T5_warming.csv", row.names = FALSE)

forest_diffs <- df %>%
  group_by(Forest.type, Year) %>%
  summarise(AvgMAT = mean(T5, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Forest.type, Year) %>%
  group_by(Forest.type) %>%
  mutate(DiffT5 = c(NA, diff(AvgT5))) %>%
  ungroup()

write.csv(forest_diffs, "forest_yearly_T5_differences.csv", row.names = FALSE)

warming_forest <- forest_diffs %>%
  filter(!is.na(DiffT5) & DiffT5 > 0) %>%
  mutate(Rounded = signif(DiffT5, digits = 1))

print("Warming by Year per Forest Type (Rounded):")
print(warming_forest[, c("Forest.type", "Year", "Rounded")])
write.csv(warming_forest, "yearly_T5_warming.csv", row.names = FALSE)
