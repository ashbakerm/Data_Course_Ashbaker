# Packages -------------------------------------------------------------
library(tidyverse)
library(janitor)      
library(GGally)      
library(caret)        
library(easystats)   
set.seed(123)

# -----------------------------------------------------
# Load the CSV
df <- read_csv("../../Data/MRA_education_career_success.csv", show_col_types = FALSE)

# Clean up column names
df <- janitor::clean_names(df)

# Drop the ID column
df$student_id <- NULL

# Convert to factors
df$entrepreneurship  <- factor(df$entrepreneurship,  levels = c("No","Yes"))
df$gender            <- factor(df$gender)
df$field_of_study    <- factor(df$field_of_study)
df$current_job_level <- factor(df$current_job_level)

# Quick check
print(names(df))
dplyr::glimpse(df)

# -----------------------------------------------------
# Exploratory Data Analysis --------------------------------------------
# Pairwise relationships for numeric vars
num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
GGally::ggpairs(df, columns = num_cols, aes(alpha = 0.4))

# Distribution of Starting Salary
ggplot(df, aes(starting_salary)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = scales::label_dollar()) +
  labs(title = "Distribution of Starting Salary",
       x = "Starting Salary",
       y = "Number of Graduates") +        
  theme_minimal()

# Salary by Field of Study
ggplot(df, aes(fct_reorder(field_of_study, starting_salary, .fun = median),
               starting_salary)) +
  geom_boxplot(outlier.alpha = 0.3) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_dollar()) +            
  labs(title = "Starting Salary by Field of Study",
       x = "", y = "Starting Salary") +
  theme_minimal()

# Internships vs Salary (with trend)
ggplot(df, aes(internships_completed, starting_salary)) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  scale_y_continuous(labels = scales::label_dollar()) +              
  labs(title = "Internships Completed vs. Starting Salary",
       x = "Internships Completed", y = "Starting Salary") +
  theme_minimal()

# Years to Promotion by Entrepreneurship
ggplot(df, aes(x = entrepreneurship, y = years_to_promotion, fill = entrepreneurship)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Years to Promotion by Entrepreneurship Status",
       x = "Entrepreneurship", y = "Years to Promotion") +
  theme_minimal()

# Salary vs. University Ranking
ggplot(df, aes(university_ranking, starting_salary)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, color = "steelblue") +
  scale_y_continuous(labels = scales::label_dollar()) +             
  labs(title = "Starting Salary vs. University Ranking",
       x = "University Ranking (lower = better)",
       y = "Starting Salary") +
  theme_minimal()

# Salary by Gender
ggplot(df, aes(x = gender, y = starting_salary, fill = gender)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  labs(title = "Starting Salary by Gender",
       x = "Gender", y = "Starting Salary") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal() +
  theme(legend.position = "none")

# Distribution of Job Offers
ggplot(df, aes(job_offers)) +
  geom_bar(fill = "coral", alpha = 0.8) +
  labs(title = "Distribution of Number of Job Offers",
       x = "Job Offers",
       y = "Number of Graduates") +
  theme_minimal()

# Career Satisfaction by Entrepreneurship
ggplot(df, aes(x = entrepreneurship, y = career_satisfaction, fill = entrepreneurship)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Career Satisfaction by Entrepreneurship Status",
       x = "Entrepreneurship", y = "Career Satisfaction (1–10)") +
  theme_minimal() +
  theme(legend.position = "none")

# -----------------------------------------------------
# Train/Test split -----------------------------------------------------
train_idx <- createDataPartition(df$starting_salary, p = 0.8, list = FALSE)
train     <- df[train_idx, ]
test      <- df[-train_idx, ]

# -----------------------------------------------------
# Linear model for Starting_Salary ------------------------------------
lm_base <- lm(
  starting_salary ~ university_gpa + internships_completed +
    certifications + soft_skills_score +
    networking_score + field_of_study,
  data = train
)
lm_full <- lm(starting_salary ~ . -entrepreneurship, data = train)
lm_step <- MASS::stepAIC(lm_full, trace = 0)

print(compare_performance(lm_base, lm_step, rank = TRUE))
model_parameters(lm_step)

test_rmse <- RMSE(predict(lm_step, newdata = test), test$starting_salary)
cat(sprintf("Test RMSE: %.2f\n", test_rmse))

# Plot residuals & diagnostics
par(mfrow = c(2,2))
plot(lm_step)
par(mfrow = c(1,1))

# -----------------------------------------------------
# Logistic model & threshold tuning for Entrepreneurship -------------
glm_full <- glm(
  entrepreneurship ~ . -starting_salary,
  data   = train,
  family = binomial
)
glm_step <- MASS::stepAIC(glm_full, trace = 0)
model_parameters(glm_step, exponentiate = TRUE)

probs <- predict(glm_step, newdata = test, type = "response")

# Tune threshold 0.1–0.9
thresholds <- seq(0.1, 0.9, by = 0.1)
results <- tibble(threshold = thresholds)

metrics <- map_dfr(thresholds, function(thr) {
  pred <- factor(ifelse(probs >= thr, "Yes", "No"),
                 levels = c("No","Yes"))
  cm   <- confusionMatrix(pred, test$entrepreneurship, positive = "Yes")
  tibble(
    threshold   = thr,
    Accuracy    = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"]
  )
})

print(metrics)

# Plot threshold tuning curves
metrics %>%
  pivot_longer(-threshold) %>%
  ggplot(aes(x = threshold, y = value, color = name)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(title = "Threshold Tuning Metrics",
       x = "Probability Threshold",
       y = "Metric Value",
       color = "") +
  theme_minimal()

# pick best threshold
best_thresh <- metrics$threshold[which.max(metrics$Sensitivity + metrics$Specificity)]
cat(sprintf("Optimal threshold: %.2f\n", best_thresh))

pred_best <- factor(ifelse(probs >= best_thresh, "Yes", "No"),
                    levels = c("No","Yes"))
conf_best <- confusionMatrix(pred_best, test$entrepreneurship, positive = "Yes")
print(conf_best)

# -----------------------------------------------------
# Save artefacts -------------------------------------------------------
saveRDS(lm_step,   "final_salary_lm.rds")
saveRDS(glm_step,  "final_entrepreneur_glm.rds")
write_csv(as_tibble(conf_best$table, .name_repair = "unique"),
          "entrepreneur_confusion_matrix.csv")

# -----------------------------------------------------
# Dataset Source:
# https://www.kaggle.com/datasets/adilshamim8/education-and-career-success
# -----------------------------------------------------
