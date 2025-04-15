library(tidyverse)
library(janitor)

career_data <- read_csv("../../Data/MRA_education_career_success.csv")

glimpse(career_data)
head(career_data)
colSums(is.na(career_data))

career_data_clean <- career_data %>%
  janitor::clean_names() %>%
  mutate(
    gender = factor(gender),
    field_of_study = factor(field_of_study),
    current_job_level = factor(current_job_level, ordered = TRUE),
    career_satisfaction = factor(career_satisfaction, ordered = TRUE),
    work_life_balance = factor(work_life_balance, ordered = TRUE),
    starting_salary = as.numeric(starting_salary),
    years_to_promotion = as.numeric(years_to_promotion),
    university_ranking = as.factor(university_ranking)
  ) %>%
  filter(!is.na(starting_salary))

glimpse(career_data_clean)
