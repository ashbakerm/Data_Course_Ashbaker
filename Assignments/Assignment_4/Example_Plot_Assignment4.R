
library(ggplot2)
library(readr)

df <- read_csv("education_career_success.csv")

df$Current_Job_Level <- as.factor(df$Current_Job_Level)

ggplot(df, aes(x = University_GPA, y = Current_Job_Level)) +
  geom_point() +
  labs(title = "University GPA vs. Current Job Level",
       x = "University GPA",
       y = "Current Job Level") +
  theme_minimal()



