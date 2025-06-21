# ----------------------------------
# Load Required Libraries
# ----------------------------------
library(dplyr)
library(lavaan)
library(semPlot)
library(knitr)
library(tidyr)

# ----------------------------------
# Load Data Files
# ----------------------------------
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

# ----------------------------------
# Standardize Key Variables
# ----------------------------------
dat <- dat %>%
  mutate(across(c(Rs, CW, SW, MAP, Elevation, T10), scale))

# ==================================
# ✅ Model 1: CW Model
# ==================================
model_cw <- '
  Rs ~ CW + MAP + Elevation + T10
  CW ~ MAP + Elevation + T10
  MAP ~~ Elevation
'

fit_cw <- sem(model_cw, data = dat, estimator = "MLM")

# ==================================
# ✅ Model 2: SW Model
# ==================================
model_sw <- '
  Rs ~ SW + MAP + Elevation + T10
  SW ~ MAP + Elevation + T10
  MAP ~~ Elevation
'

fit_sw <- sem(model_sw, data = dat, estimator = "MLM")

# ==================================
# ✅ Model 3: Latent Warming Model
# ==================================
model_combined_t10 <- '
  Warming =~ CW + SW
  Rs ~ Warming + MAP + Elevation + T10
  CW ~ MAP + Elevation + T10
  SW ~ MAP + Elevation + T10
  MAP ~~ Elevation
'

fit_combined_t10 <- sem(model_combined_t10, data = dat, estimator = "MLM")

# ==================================
# ✅ Print Fit Measures
# ==================================
cat("### ✅ Model Fit Comparison\n")
fit_summary <- rbind(
  cbind(model = "CW", fitmeasures(fit_cw, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"))),
  cbind(model = "SW", fitmeasures(fit_sw, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr"))),
  cbind(model = "Latent", fitmeasures(fit_combined_t10, c("chisq", "df", "pvalue", "rmsea", "cfi", "tli", "srmr")))
)
kable(fit_summary, digits = 3)





# ==================================
# ✅ Interpret and Save SEM Paths
# ==================================

# Helper function to create interpretations
interpret_sem <- function(fit, model_label) {
  pe <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op == "~") %>%
    mutate(
      significance = case_when(
        pvalue < 0.001 ~ "***",
        pvalue < 0.01 ~ "**",
        pvalue < 0.05 ~ "*",
        TRUE ~ ""
      ),
      direction = case_when(
        est > 0 & pvalue < 0.05 ~ "significantly increases",
        est < 0 & pvalue < 0.05 ~ "significantly decreases",
        TRUE ~ "does not significantly affect"
      ),
      description = paste0(
        rhs, " ", direction, " ", lhs,
        ifelse(significance != "", paste0(" (β = ", round(est, 2), ", p = ", round(pvalue, 3), " ", significance, ")"),
               paste0(" (β = ", round(est, 2), ", p = ", round(pvalue, 3), ")"))
      ),
      model = model_label
    ) %>%
    select(model, lhs, rhs, est, pvalue, description)

  return(pe)
}

# Interpret each model
interp_cw <- interpret_sem(fit_cw, "CW model")
interp_sw <- interpret_sem(fit_sw, "SW model")
interp_latent <- interpret_sem(fit_combined_t10, "Latent model")

# Combine all interpretations
all_interpretations <- bind_rows(interp_cw, interp_sw, interp_latent)

# Save to CSV
write.csv(all_interpretations, "sem_interpretations.csv", row.names = FALSE)

# Optional: Print preview
cat("\n### ✅ SEM Interpretations Saved to CSV\n")
print(head(all_interpretations[, c("model", "description")], 10))





# ==================================
# ✅ Compare AIC and BIC
# ==================================
aic_table <- AIC(fit_cw, fit_sw, fit_combined_t10)
bic_table <- BIC(fit_cw, fit_sw, fit_combined_t10)
cat("\n### ✅ AIC Comparison\n")
kable(aic_table)
cat("\n### ✅ BIC Comparison\n")
kable(bic_table)

# ==================================
# ✅ Extract Standardized Path Coefficients
# ==================================
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

# ==================================
# ✅ Effects Summary for Latent Model
# ==================================
cat("\n### ✅ Direct, Indirect, and Total Effects from Latent Warming Model\n")
latent_effects <- parameterEstimates(fit_combined_t10, standardized = TRUE, ci = TRUE) %>%
  filter(lhs == "Rs" & op %in% c("~", ":=")) %>%
  select(lhs, op, rhs, est, ci.lower, ci.upper)

kable(latent_effects, digits = 3)

# ==================================
# ✅ Path Diagrams 
# ==================================
# ==================================
# ✅ Generate SEM Path Diagrams
# ==================================

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
# ==================================
# ✅ Generate SEM Path Diagrams
# ==================================

# Rename Elevation to ELN for plotting purposes
node_labels_cw <- semPlotModel(fit_cw)@Vars$name
node_labels_sw <- semPlotModel(fit_sw)@Vars$name
node_labels_combined <- semPlotModel(fit_combined_t10)@Vars$name

node_labels_cw[node_labels_cw == "Elevation"] <- "ELN"
node_labels_sw[node_labels_sw == "Elevation"] <- "ELN"
node_labels_combined[node_labels_combined == "Elevation"] <- "ELN"

# Replace missing labels
node_labels_cw[is.na(node_labels_cw)] <- "Unknown"
node_labels_sw[is.na(node_labels_sw)] <- "Unknown"
node_labels_combined[is.na(node_labels_combined)] <- "Unknown"

# ==================================
# ✅ Export SEM Paths (Triangular Layout)
# ==================================

### ===== TIFF Export =====
tiff("SEM_Models_Final.tiff", width = 16, height = 8, units = "in", res = 600, compression = "lzw")
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2))

semPaths(fit_cw, what = "std", whatLabels = "std", style = "lisrel", layout = "circle2", rotation = 1,
         sizeMan = 10, sizeLat = 10, edge.label.cex = 1.2, label.cex = 1.2,
         curvePivot = TRUE, nCharNodes = 0, fade = FALSE, edge.color = edge_colors_cw,
         color = list(lat = "white", man = "white"), border.width = 1.5,
         border.color = "black", nodeLabels = node_labels_cw, title = FALSE, legend = FALSE)

semPaths(fit_sw, what = "std", whatLabels = "std", style = "lisrel", layout = "circle2", rotation = 1,
         sizeMan = 10, sizeLat = 10, edge.label.cex = 1.2, label.cex = 1.2,
         curvePivot = TRUE, nCharNodes = 0, fade = FALSE, edge.color = edge_colors_sw,
         color = list(lat = "white", man = "white"), border.width = 1.5,
         border.color = "black", nodeLabels = node_labels_sw, title = FALSE, legend = FALSE)

semPaths(fit_combined_t10, what = "std", whatLabels = "std", style = "lisrel", layout = "circle2", rotation = 1,
         sizeMan = 10, sizeLat = 10, edge.label.cex = 1.2, label.cex = 1.2,
         curvePivot = TRUE, nCharNodes = 0, fade = FALSE, edge.color = edge_colors_combined,
         color = list(lat = "white", man = "white"), border.width = 1.5,
         border.color = "black", nodeLabels = node_labels_combined, title = FALSE, legend = FALSE)

dev.off()

### ===== PDF Export =====
pdf("SEM_Models_Final.pdf", width = 16, height = 8)
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2))

semPaths(fit_cw, what = "std", whatLabels = "std", style = "lisrel", layout = "circle2", rotation = 1,
         sizeMan = 10, sizeLat = 10, edge.label.cex = 1.2, label.cex = 1.2,
         curvePivot = TRUE, nCharNodes = 0, fade = FALSE, edge.color = edge_colors_cw,
         color = list(lat = "white", man = "white"), border.width = 1.5,
         border.color = "black", nodeLabels = node_labels_cw, title = FALSE, legend = FALSE)

semPaths(fit_sw, what = "std", whatLabels = "std", style = "lisrel", layout = "circle2", rotation = 1,
         sizeMan = 10, sizeLat = 10, edge.label.cex = 1.2, label.cex = 1.2,
         curvePivot = TRUE, nCharNodes = 0, fade = FALSE, edge.color = edge_colors_sw,
         color = list(lat = "white", man = "white"), border.width = 1.5,
         border.color = "black", nodeLabels = node_labels_sw, title = FALSE, legend = FALSE)

semPaths(fit_combined_t10, what = "std", whatLabels = "std", style = "lisrel", layout = "circle2", rotation = 1,
         sizeMan = 10, sizeLat = 10, edge.label.cex = 1.2, label.cex = 1.2,
         curvePivot = TRUE, nCharNodes = 0, fade = FALSE, edge.color = edge_colors_combined,
         color = list(lat = "white", man = "white"), border.width = 1.5,
         border.color = "black", nodeLabels = node_labels_combined, title = FALSE, legend = FALSE)

dev.off()





#Horizontal
# ==================================
# ✅ Generate SEM Path Diagrams
# ==================================

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

# Replace missing labels with placeholders
node_labels_cw[is.na(node_labels_cw)] <- "Unknown"
node_labels_sw[is.na(node_labels_sw)] <- "Unknown"
node_labels_combined[is.na(node_labels_combined)] <- "Unknown"

# ==================================
# ✅ Export SEM Paths
# ==================================

### ===== TIFF Export =====
tiff("SEM_Models_Final.tiff", width = 18, height = 6, units = "in", res = 600, compression = "lzw")
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2))

semPaths(fit_cw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_cw, border.color = "black",
         nodeLabels = node_labels_cw)

semPaths(fit_sw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_sw, border.color = "black",
         nodeLabels = node_labels_sw)

semPaths(fit_combined_t10, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_combined, border.color = "black",
         nodeLabels = node_labels_combined)

dev.off()

### ===== PDF Export =====
pdf("SEM_Models_Final.pdf", width = 18, height = 6)
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2))

semPaths(fit_cw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_cw, border.color = "black",
         nodeLabels = node_labels_cw)

semPaths(fit_sw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_sw, border.color = "black",
         nodeLabels = node_labels_sw)

semPaths(fit_combined_t10, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_combined, border.color = "black",
         nodeLabels = node_labels_combined)

dev.off()






#more Horizontal
# ==================================
# ✅ Generate SEM Path Diagrams
# ==================================

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

# ==================================
# ✅ Export SEM Paths
# ==================================

### ===== TIFF Export =====
tiff("SEM_Models_Final.tiff", width = 18, height = 6, units = "in", res = 600, compression = "lzw")
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2))

semPaths(fit_cw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_cw, border.color = "black",
         nodeLabels = node_labels_cw)

semPaths(fit_sw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_sw, border.color = "black",
         nodeLabels = node_labels_sw)

semPaths(fit_combined_t10, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_combined, border.color = "black",
         nodeLabels = node_labels_combined)

dev.off()

### ===== PDF Export =====
pdf("SEM_Models_Final.pdf", width = 18, height = 6)
par(mfrow = c(1, 3), mar = c(2, 2, 2, 2))

semPaths(fit_cw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_cw, border.color = "black",
         nodeLabels = node_labels_cw)

semPaths(fit_sw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_sw, border.color = "black",
         nodeLabels = node_labels_sw)

semPaths(fit_combined_t10, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_combined, border.color = "black",
         nodeLabels = node_labels_combined)

dev.off()







# Save separately
# ==================================
# ✅ Export SEM Paths Separately
# ==================================

### ===== CW Model TIFF Export =====
tiff("SEM_CW_Model.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
semPaths(fit_cw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_cw, border.color = "black",
         nodeLabels = node_labels_cw)
dev.off()

### ===== SW Model TIFF Export =====
tiff("SEM_SW_Model.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
semPaths(fit_sw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_sw, border.color = "black",
         nodeLabels = node_labels_sw)
dev.off()

### ===== Combined Warming Model TIFF Export =====
tiff("SEM_Combined_Warming_Model.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
semPaths(fit_combined_t10, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_combined, border.color = "black",
         nodeLabels = node_labels_combined)
dev.off()

### ===== CW Model PDF Export =====
pdf("SEM_CW_Model.pdf", width = 6, height = 6)
semPaths(fit_cw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_cw, border.color = "black",
         nodeLabels = node_labels_cw)
dev.off()

### ===== SW Model PDF Export =====
pdf("SEM_SW_Model.pdf", width = 6, height = 6)
semPaths(fit_sw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_sw, border.color = "black",
         nodeLabels = node_labels_sw)
dev.off()

### ===== Combined Warming Model PDF Export =====
pdf("SEM_Combined_Warming_Model.pdf", width = 6, height = 6)
semPaths(fit_combined_t10, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_combined, border.color = "black",
         nodeLabels = node_labels_combined)
dev.off()






#without residuals
# ==================================
# ✅ Export SEM Paths Separately
# ==================================

### ===== CW Model TIFF Export =====
tiff("SEM_CW_Model.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
semPaths(fit_cw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_cw, border.color = "black",
         nodeLabels = node_labels_cw, residuals = FALSE)
dev.off()

### ===== SW Model TIFF Export =====
tiff("SEM_SW_Model.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
semPaths(fit_sw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_sw, border.color = "black",
         nodeLabels = node_labels_sw, residuals = FALSE)
dev.off()

### ===== Combined Warming Model TIFF Export =====
tiff("SEM_Combined_Warming_Model.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
semPaths(fit_combined_t10, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_combined, border.color = "black",
         nodeLabels = node_labels_combined, residuals = FALSE)
dev.off()

### ===== CW Model PDF Export =====
pdf("SEM_CW_Model.pdf", width = 6, height = 6)
semPaths(fit_cw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_cw, border.color = "black",
         nodeLabels = node_labels_cw, residuals = FALSE)
dev.off()

### ===== SW Model PDF Export =====
pdf("SEM_SW_Model.pdf", width = 6, height = 6)
semPaths(fit_sw, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_sw, border.color = "black",
         nodeLabels = node_labels_sw, residuals = FALSE)
dev.off()

### ===== Combined Warming Model PDF Export =====
pdf("SEM_Combined_Warming_Model.pdf", width = 6, height = 6)
semPaths(fit_combined_t10, what = "std", layout = "tree", sizeMan = 10, edge.label.cex = 1.2,
         fade = FALSE, edge.color = edge_colors_combined, border.color = "black",
         nodeLabels = node_labels_combined, residuals = FALSE)
dev.off()
