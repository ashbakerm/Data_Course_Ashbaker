library(tidyverse)
library(modelr)
library(easystats)
library(ggpubr)
library(caret)
library(GGally)
library(performance)
library(MASS)

# Load the dataset
admissions <- read_csv("../../Data/GradSchool_Admissions.csv") %>%
  mutate(
    admit = factor(admit),
    rank = factor(rank)
  )

glimpse(admissions)
summary(admissions)

# Initial exploration
admissions_subset <- dplyr::select(admissions, gre, gpa, rank, admit)

GGally::ggpairs(admissions_subset, aes(color = admit))

# Visualize distributions
admissions %>% 
  ggplot(aes(x = gre, fill = admit)) + 
  geom_density(alpha = 0.5) + 
  theme_minimal()

admissions %>% 
  ggplot(aes(x = gpa, fill = admit)) + 
  geom_density(alpha = 0.5) + 
  theme_minimal()

admissions %>% 
  ggplot(aes(x = rank, fill = admit)) + 
  geom_bar(position = "fill") + 
  theme_minimal()

# QQ plot for GPA
ggqqplot(admissions$gpa)

# Model fitting
mod1 <- glm(admit ~ gre + gpa + rank, data = admissions, family = binomial)
mod2 <- glm(admit ~ gre * gpa + rank, data = admissions, family = binomial)
mod3 <- glm(admit ~ gre * gpa * rank, data = admissions, family = binomial)
mod4 <- glm(admit ~ gre * gpa * rank + I(gre^2) + I(gpa^2), data = admissions, family = binomial)

# Stepwise model selection from a full model
full_mod <- glm(admit ~ gre * gpa * rank, data = admissions, family = binomial)
step <- stepAIC(full_mod, trace = 0)
mod5 <- glm(formula = step$formula, data = admissions, family = binomial)

# Compare model performance
comps <- compare_performance(mod1, mod2, mod3, mod4, mod5, rank = TRUE)
print(comps)
plot(comps)

# Add a simplified but high-performing model
mod6 <- glm(admit ~ (gre + gpa) * rank, data = admissions, family = binomial)
compare_performance(mod1, mod2, mod3, mod4, mod5, mod6, rank = TRUE)

# Visualize predictions
admissions %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6) %>% 
  ggplot(aes(x = as.numeric(admit), y = pred, color = model)) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Predicted Probability vs Actual Admission",
       x = "Actual Admission",
       y = "Predicted Probability")

# Cross-validation framework
set.seed(123)
train_index <- createDataPartition(admissions$admit, p = 0.8, list = FALSE)
train_data <- admissions[train_index, ]
test_data <- admissions[-train_index, ]

# Refit top models
mod5 <- glm(formula = mod5$formula, data = train_data, family = binomial)
mod6 <- glm(formula = mod6$formula, data = train_data, family = binomial)

# Test predictions
gather_predictions(test_data, mod5, mod6) %>% 
  ggplot(aes(x = as.numeric(admit), y = pred, color = model)) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Test Set Predictions vs Actual Admission",
       subtitle = "Dashed line shows ideal prediction")

# Interpret final model
model_parameters(mod6)
