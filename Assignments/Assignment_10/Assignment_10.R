# =======================================================
#  Packages & Data
# =======================================================

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
depression_raw <- read_csv("../../Data/MRA_Student_Depression_Dataset.csv",
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

# =======================================================
#  Exploratory Analysis
# =======================================================

# ---- Numeric Variable Distributions ----
numeric_vars <- depression %>% 
  select(where(is.numeric)) %>% 
  names()

depression %>% 
  pivot_longer(cols = all_of(numeric_vars),
               names_to  = "variable",
               values_to = "value") %>% 
  ggplot(aes(x = value, fill = depression)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal()

# ---- Categorical Variable Proportions ----
depression %>% 
  select(where(is.factor)) %>% 
  pivot_longer(cols = -depression,
               names_to  = "variable",
               values_to = "value") %>% 
  ggplot(aes(x = value, fill = depression)) +
  geom_bar(position = "fill") +
  facet_wrap(~ variable, scales = "free_x") +
  theme_minimal()

# ---- Pairwise Relationships (Numeric) ----
GGally::ggpairs(depression,
                columns = numeric_vars,
                aes(color = depression, alpha = 0.4))



