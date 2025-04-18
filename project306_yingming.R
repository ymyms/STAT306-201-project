# Author: Yingming Sha
# Course: STAT 306 201 2024W2
# Last Modified: 2025-04-17

# Load library
library(ggplot2)
library(dplyr)
library(corrplot)
library(car)
library(leaps)
library(GGally)
library(tidyr)
library(tibble)
library(gridExtra) 

# Load data
white=read.csv("winequality-white.csv", sep = ";")

# Correlation
cor(white$alcohol, white$quality)
cor(white$density, white$quality)
cor(white$pH, white$quality)
cor(white$residual.sugar, white$quality)
cor(white$fixed.acidity, white$quality)
cor(white$volatile.acidity, white$quality)
cor(white$citric.acid, white$quality)
cor(white$chlorides, white$quality)
cor(white$free.sulfur.dioxide, white$quality)
cor(white$total.sulfur.dioxide, white$quality)
cor(white$sulphates, white$quality)

# List of variables
vars <- c("alcohol", "density", "pH", "residual.sugar", "fixed.acidity", 
          "volatile.acidity", "citric.acid", "chlorides", 
          "free.sulfur.dioxide", "total.sulfur.dioxide", "sulphates")

# Scatter plots
plots <- lapply(vars, function(var) {
  ggplot(white, aes(x = .data[[var]], y = .data[["quality"]])) + 
    geom_point(alpha = 0.5) +
    labs(title = paste("Scatterplot of", var, "vs Quality")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))  
})

# Arrange the plots in a grid
do.call(grid.arrange, c(plots, ncol = 3))  

# Box plots
ggplot(white, aes(x = factor(quality), y = alcohol)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Alcohol by Quality Level", x = "Quality", y = "Alcohol") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = fixed.acidity)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Fixed Acidity by Quality Level", x = "Quality", y = "Fixed Acidity") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = volatile.acidity)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Volatile Acidity by Quality Level", x = "Quality", y = "Volatile Acidity") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = citric.acid)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Citric Acid by Quality Level", x = "Quality", y = "Citric Acid") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = residual.sugar)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Residual Sugar by Quality Level", x = "Quality", y = "Residual Sugar") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = chlorides)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Chlorides by Quality Level", x = "Quality", y = "Chlorides") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = free.sulfur.dioxide)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Free Sulfur Dioxide by Quality Level", x = "Quality", y = "Free Sulfur Dioxide") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = total.sulfur.dioxide)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Total Sulfur Dioxide by Quality Level", x = "Quality", y = "Total Sulfur Dioxide") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = density)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Density by Quality Level", x = "Quality", y = "Density") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = pH)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "pH by Quality Level", x = "Quality", y = "pH") +
  theme_minimal()

ggplot(white, aes(x = factor(quality), y = sulphates)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Sulphates by Quality Level", x = "Quality", y = "Sulphates") +
  theme_minimal()

# Continue with EDA
summary(white)

# Check missing values
sum(is.na(white))

# Multicollinearity check
lm_full <- lm(quality ~ alcohol + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides +
                free.sulfur.dioxide + total.sulfur.dioxide + density + pH+sulphates, data = white)
vif_values <- vif(lm_full)
print(vif_values)

# After reduce density
lm_without_density <- lm(quality ~ alcohol + fixed.acidity + volatile.acidity + citric.acid + chlorides +residual.sugar+
                           free.sulfur.dioxide + total.sulfur.dioxide + pH+sulphates, data = white)
vif_values2 <- vif(lm_without_density)
print(vif_values2)

# Corrlation plot
cor_matrix <- cor(white[, sapply(white, is.numeric)])
corrplot(cor_matrix, method = "circle", type = "upper", 
         tl.cex = 0.8,
         addCoef.col = "black", 
         diag = FALSE) 

GGally::ggpairs(white[, c("alcohol", "density", "pH", "residual.sugar", "fixed.acidity", "volatile.acidity","citric.acid"
,"chlorides","free.sulfur.dioxide","total.sulfur.dioxide","sulphates")],upper = list(continuous = wrap("cor", size = 4)),
lower = list(continuous = wrap("points", alpha = 0.5, size = 0.7)),
diag = list(continuous = wrap("barDiag", bins = 20)))

cor_df <- as.data.frame(cor_matrix) %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "value")

# Plot heatmap
ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  labs(title = "The Correlation Heatmap of Wine Attributes") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10))

# Remove density since multicollinearity issue
white <- subset(white, select = -density)

# Model with only intercept 
empty_model <- lm(quality ~ 1, data = white)

# Full model (with all predictors)
full_model <- lm(quality ~ ., data = white)

# Perform forward selection
forward_model <- step(empty_model, scope = formula(full_model), direction = "forward")

# Perform backward selection
backward_model <- step(full_model, direction = "backward")

# Stepwise selection: both forward and backward
stepwise_model <- step(empty_model, 
                       scope = formula(full_model), 
                       direction = "both")

# Extract R² and Adjusted R² from each model
model_names <- c("Forward", "Backward", "Stepwise", "Full")
r_squared <- c(summary(forward_model)$r.squared,
               summary(backward_model)$r.squared,
               summary(stepwise_model)$r.squared,
               summary(full_model)$r.squared)

adj_r_squared <- c(summary(forward_model)$adj.r.squared,
                   summary(backward_model)$adj.r.squared,
                   summary(stepwise_model)$adj.r.squared,
                   summary(full_model)$adj.r.squared)

# Create a data for easy viewing
comparison_table <- data.frame(
  Model = model_names,
  R_squared = round(r_squared, 4),
  Adjusted_R_squared = round(adj_r_squared, 4)
)

# View the table
print(comparison_table)

summary(forward_model)
summary(backward_model)
summary(stepwise_model)
# The result shows they all selected the same model

# Select the final model
final_selected <- lm(quality ~ alcohol+volatile.acidity+residual.sugar+free.sulfur.dioxide+fixed.acidity+sulphates+total.sulfur.dioxide+pH+chlorides, data = white)
summary(final_selected)

#Cp
sigma2_full <- summary(full_model)$sigma^2
rss_final <- sum(resid(final_selected)^2)
n <- nrow(white)
k <- length(coef(final_selected)) 
cp_final <- rss_final / sigma2_full - (n - 2 * k)
cp_final

# Extract fitted values
fitted_values <- fitted(final_selected)

# Extract residuals
residuals_values <- residuals(final_selected)

# Residual plot
plot(fitted_values, residuals_values,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted",
     pch = 16, col = rgb(0, 0, 1, 0.3))  
abline(h = 0, col = "red", lty = 2)

# QQ plot
qqnorm(residuals_values, main = "Q-Q Plot of Residuals")
qqline(residuals_values, col = "red")  

# Final vif check
vif_final <- vif(final_selected)
print(vif_final)