# ---- Load libraries ----
library(tidyverse)
library(modelr)
library(easystats)
library(ggpubr)
library(caret)
library(GGally)
library(performance)
library(MASS)
library(janitor)

# ---- Data ----
depression_raw <- read_csv("../../Data/MRA_Student Depression Dataset.csv",
                           show_col_types = FALSE)

# ---- Basic Cleaning & Type Conversion ----
depression <- depression_raw %>%
  rename(suicidal_thoughts = `Have you ever had suicidal thoughts ?`) %>%
  clean_names() %>%
  mutate(
    across(c(gender, city, profession, sleep_duration, dietary_habits,
             degree, suicidal_thoughts, family_history_of_mental_illness),
           as.factor),
    depression = factor(depression, levels = c(0, 1),
                        labels = c("No", "Yes"))
  )

# ---- Data Check ----
glimpse(depression)
summary(depression)

# ---- Missing Overview ----
depression %>% 
  summarise(across(everything(), \(x) mean(is.na(x)) * 100)) %>% 
  pivot_longer(everything(),
               names_to  = "variable",
               values_to = "pct_missing") %>% 
  arrange(desc(pct_missing))

# ---- Outcome Balance ----
depression %>% 
  count(depression, name = "n") %>% 
  mutate(prop = n / sum(n))

# ---- Numeric Distributions ----
numeric_vars <- depression %>% 
  dplyr::select(where(is.numeric), -id) %>% 
  names()

depression %>% 
  pivot_longer(cols = all_of(numeric_vars),
               names_to  = "variable",
               values_to = "value") %>% 
  ggplot(aes(x = value, fill = depression)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

# ---- Boxplots by Outcome ----
depression %>% 
  pivot_longer(cols = all_of(numeric_vars),
               names_to  = "variable",
               values_to = "value") %>% 
  ggplot(aes(x = depression, y = value, fill = depression)) +
  geom_boxplot(outlier.alpha = 0.2) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

# ---- Categorical Proportions ----
cat_vars <- depression %>% 
  dplyr::select(where(is.factor), -depression) %>% 
  names()

depression %>% 
  pivot_longer(cols = all_of(cat_vars),
               names_to  = "variable",
               values_to = "value") %>% 
  ggplot(aes(x = value, fill = depression)) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Pairwise Relationships ----
## Filtered: excludes zero values = id and job_satisfaction
pair_vars <- depression %>%
  dplyr::select(where(is.numeric), -id, -job_satisfaction) %>%
  names()

GGally::ggpairs(depression, columns = pair_vars, aes(color = depression, alpha = 0.4))

# ---- Logistic Regression Models ----

mod1 <- glm(
  depression ~ academic_pressure + cgpa + suicidal_thoughts,
  data   = depression,
  family = binomial
)

mod2 <- glm(
  depression ~ academic_pressure * cgpa + suicidal_thoughts,
  data   = depression,
  family = binomial
)

mod3 <- glm(
  depression ~ academic_pressure * cgpa * suicidal_thoughts,
  data   = depression,
  family = binomial
)

mod4 <- glm(
  depression ~ academic_pressure * cgpa * suicidal_thoughts +
    I(academic_pressure^2) + I(cgpa^2),
  data   = depression,
  family = binomial
)

# ---- Stepwise AIC Model ----

full_mod <- glm(depression ~ ., data = depression, family = binomial)
step_mod <- stepAIC(full_mod, trace = 0)
mod5 <- glm(formula = step_mod$formula, data = depression, family = binomial)

# ---- Simple Alt. Model ----

mod6 <- glm(
  depression ~ (academic_pressure + cgpa) * suicidal_thoughts +
    sleep_duration,
  data   = depression,
  family = binomial
)


# ---- Performance Metrics Table ----
comps <- compare_performance(
  mod1, mod2, mod3, mod4, mod5, mod6,
  rank = TRUE
)

print(comps)

# mod5 is the clear winner across all metrics — best fit and most accurate

# ---- Visual Comparison ----
plot(comps) +
  theme_minimal()


# ---- Predictions for Each Model ----
depression_preds <- depression %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6, type = "response")

# ---- Plot Predictions vs Actual ----
depression_preds %>%
  ggplot(aes(x = as.numeric(depression) - 1, y = pred, color = model)) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Predicted Probability vs Actual Depression Status",
    x = "Actual Depression (0 = No, 1 = Yes)",
    y = "Predicted Probability"
  )
# Each dot is a predicted probability for an individual
# The x-axis is their actual depression status (0 or 1)

## The best model (mod5) produces consistently higher predicted 
## probabilities for true positives (depression = 1) than for true negatives


# ---- Split Data into Training and Testing ----
set.seed(123)
train_index <- createDataPartition(depression$depression, p = 0.8, list = FALSE)

train_data <- depression[train_index, ]
test_data  <- depression[-train_index, ]

# ---- Refit Top Models on Training Data ----
mod5_cv <- glm(formula = mod5$formula, data = train_data, family = binomial)
mod6_cv <- glm(formula = mod6$formula, data = train_data, family = binomial)

# ---- Generate Predictions on Test Data ----
test_preds <- test_data %>%
  gather_predictions(mod5_cv, mod6_cv, type = "response")

# ---- Plot Predictions vs Actual (Test Set) ----
test_preds %>%
  ggplot(aes(x = as.numeric(depression) - 1, y = pred, color = model)) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Test Set Predictions vs Actual Depression Status",
    subtitle = "Dashed line shows ideal prediction",
    x = "Actual Depression (0 = No, 1 = Yes)",
    y = "Predicted Probability"
  )


# ---- Summarize Final Model (mod6) ----
summary(mod6)

# ---- Estimate & Interpret Coefficients ----
model_parameters(mod6, exponentiate = TRUE)

# ---- Visualize Coefficients ----
plot(model_parameters(mod6)) +
  theme_minimal() +
  labs(title = "Coefficient Estimates – Final Model (mod6)")

## Individuals with high academic pressure, low sleep duration, and a history of suicidal thoughts are more likely
## to report depression. Even modest increases in CGPA are associated with slightly increased odds of depression
      ## though this may reflect perfectionism or pressure to maintain high performance.

