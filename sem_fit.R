library(dplyr)
library(lavaan)
library(semPlot)
library(knitr)
library(tidyr)

regression_matrix <- read.csv("regression_matrix.csv")
cw_warming <- read.csv("yearly_MAT_warming.csv")
sw_warming <- read.csv("yearly_T5_warming.csv")

# Prepare warming data
cw <- cw_warming[, c("Year", "Rounded")]
colnames(cw) <- c("Year", "CW")

sw <- sw_warming[, c("Year", "Rounded")]
colnames(sw) <- c("Year", "SW")

# Merge all data
dat <- regression_matrix %>%
  left_join(cw, by = "Year") %>%
  left_join(sw, by = "Year") %>%
  na.omit()


# Standardize Key Variables
dat <- dat %>%
  mutate(across(c(Rs, CW, SW, MAP, Elevation, T10), scale))


# Model 1: CW Model
model_cw <- '
  Rs ~ CW + MAP + Elevation + T10
  CW ~ MAP + Elevation + T10
  MAP ~~ Elevation
'

fit_cw <- sem(model_cw, data = dat, estimator = "MLM")

# ✅ Model 2: SW Model
model_sw <- '
  Rs ~ SW + MAP + Elevation + T10
  SW ~ MAP + Elevation + T10
  MAP ~~ Elevation
'
fit_sw <- sem(model_sw, data = dat, estimator = "MLM")


# Model 3: Latent Warming Model
model_combined_t10 <- '
  Warming =~ CW + SW
  Rs ~ Warming + MAP + Elevation + T10
  CW ~ MAP + Elevation + T10
  SW ~ MAP + Elevation + T10
  MAP ~~ Elevation
'

fit_combined_t10 <- sem(model_combined_t10, data = dat, estimator = "MLM")

# Print Fit Measures
cat("### ✅ Model Fit Comparison\n")
fit_summary <- rbind(
  cbind(model = "CW", fitmeasures(fit_cw, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"))),
  cbind(model = "SW", fitmeasures(fit_sw, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"))),
  cbind(model = "Latent", fitmeasures(fit_combined_t10, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr")))
)
kable(fit_summary, digits = 3)

aic_table <- AIC(fit_cw, fit_sw, fit_combined_t10)
bic_table <- BIC(fit_cw, fit_sw, fit_combined_t10)
cat("\n### ✅ AIC Comparison\n")
kable(aic_table)
cat("\n### ✅ BIC Comparison\n")
kable(bic_table)

# Extract Standardized Path Coefficients

get_direct_paths <- function(fit_obj, label) {
  standardizedSolution(fit_obj) %>%
    filter(op == "~") %>%
    select(lhs, rhs, est.std) %>%
    mutate(model = label)
}

paths_cw <- get_direct_paths(fit_cw, "CW")
paths_sw <- get_direct_paths(fit_sw, "SW")
paths_latent <- get_direct_paths(fit_combined_t10, "Latent")

all_paths <- bind_rows(paths_cw, paths_sw, paths_latent) %>%
  pivot_wider(names_from = model, values_from = est.std) %>%
  rename(Dependent = lhs, Predictor = rhs)

cat("\n### ✅ Standardized Path Coefficients Across Models\n")
kable(all_paths, digits = 3)

#Effects Summary for Latent Model
cat("\n### ✅ Direct, Indirect, and Total Effects from Latent Warming Model\n")
latent_effects <- parameterEstimates(fit_combined_t10, standardized = TRUE, ci = TRUE) %>%
  filter(lhs == "Rs" & op %in% c("~", ":=")) %>%
  select(lhs, op, rhs, est, ci.lower, ci.upper)

kable(latent_effects, digits = 3)

#Generate SEM Path Diagrams

get_edge_colors <- function(fit, sig_color = "#003049", nonsig_color = "#eae2b7") {
  pe <- parameterEstimates(fit)
  pe <- subset(pe, op %in% c("~", "~~"))
  colors <- ifelse(pe$pvalue < 0.05, sig_color, nonsig_color)
  colors[is.na(colors)] <- nonsig_color  # Replace NA values
  return(colors)
}

# Get edge colors for each model
edge_colors_cw <- get_edge_colors(fit_cw)
edge_colors_sw <- get_edge_colors(fit_sw)
edge_colors_combined <- get_edge_colors(fit_combined_t10)

# Extract variable names for node labels
node_labels_cw <- semPlotModel(fit_cw)@Vars$name
node_labels_sw <- semPlotModel(fit_sw)@Vars$name
node_labels_combined <- semPlotModel(fit_combined_t10)@Vars$name

node_labels_cw[node_labels_cw == "Elevation"] <- "ELN"
node_labels_sw[node_labels_sw == "Elevation"] <- "ELN"
node_labels_combined[node_labels_combined == "Elevation"] <- "ELN"
node_labels_combined[node_labels_combined == "Warming"] <- "TW"

# Replace missing labels with placeholders
node_labels_cw[is.na(node_labels_cw)] <- "Unknown"
node_labels_sw[is.na(node_labels_sw)] <- "Unknown"
node_labels_combined[is.na(node_labels_combined)] <- "Unknown"

#CW Model TIFF Export
tiff("SEM_CW_Model.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
semPaths(fit_cw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_cw, border.color = "black",
         nodeLabels = node_labels_cw, residuals = FALSE)
dev.off()

# SW Model TIFF Export
tiff("SEM_SW_Model.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
semPaths(fit_sw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_sw, border.color = "black",
         nodeLabels = node_labels_sw, residuals = FALSE)
dev.off()

