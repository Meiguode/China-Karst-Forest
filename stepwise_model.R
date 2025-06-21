library(MASS)
library(ggplot2)
library(caret)
library(lm.beta)
library(performance)

regression_matrix <- read.csv("regression_matrix.csv", header = TRUE)

dat <- subset(regression_matrix, select = c("Rs", "MAP", "Elevation", "T10", "MAT", "T5", "Forest.type"))

dat$Forest.type <- as.factor(dat$Forest.type)

dat <- na.omit(dat)

full_model <- lm(Rs ~ MAP + Elevation + T10 + MAT + T5 + Forest.type, data = dat)

step_model <- stepAIC(full_model, direction = "both", trace = FALSE)

pred <- predict(step_model)
obs <- dat$Rs
resid <- resid(step_model)

r2 <- caret::R2(pred, obs)
rmse <- caret::RMSE(pred, obs)
metrics <- data.frame(R2 = r2, RMSE = rmse)
write.csv(metrics, "stepwise_model_validation_metrics.csv", row.names = FALSE)

std_model <- lm.beta(step_model)
importance_df <- data.frame(
  Variable = names(std_model$standardized.coefficients)[-1],
  Std_Coefficient = unname(std_model$standardized.coefficients[-1])
)
importance_df <- importance_df[order(abs(importance_df$Std_Coefficient), decreasing = TRUE), ]
write.csv(importance_df, "variable_importance_stepwise.csv", row.names = FALSE)

# Diagnostic plots
p1 <- ggplot(data.frame(obs, pred), aes(x = obs, y = pred)) + 
  geom_point(size = 1.5, color = "#003049") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#d62828") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

tiff("Observed_vs_Predicted.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
print(p1)
dev.off()

qq <- ggplot(data.frame(resid = resid), aes(sample = resid)) +
  stat_qq(color = "#003049", size = 1.5) +
  stat_qq_line(color = "#d62828", linetype = "dashed") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

tiff("QQ_Plot.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
print(qq)
dev.off()

cooksd <- cooks.distance(step_model)
cutoff <- 4 / nrow(dat)
cook_df <- data.frame(Index = seq_along(cooksd), Cook = cooksd)

p3 <- ggplot(cook_df, aes(x = Index, y = Cook)) +
  geom_point(size = 1.5, color = "#003049") +
  geom_hline(yintercept = cutoff, color = "#d62828", linetype = "dashed") +
  theme_minimal(base_size = 14) +
  theme(panel.grid = element_blank())

tiff("Cooks_Distance.tiff", width = 6, height = 6, units = "in", res = 600, compression = "lzw")
print(p3)
dev.off()
