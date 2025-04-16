library(tidyverse)
library(modelr)
library(ggplot2)
library(broom)
library(dplyr)

salary_raw <- read_csv("FacultySalaries_1995.csv")
glimpse(salary_raw)
colnames(salary_raw)

## 1
#---------------------
# create long salary data -- w/out summarizing
salary_long <- salary_raw %>%
  pivot_longer(
    cols = starts_with("AvgFullProf") | starts_with("AvgAssocProf") | starts_with("AvgAssistProf"),
    names_to = "RankMeasure",
    values_to = "Value"
  ) %>%
  mutate(
    RankMeasure = str_remove(RankMeasure, "^Avg"),
    Tier = na_if(Tier, "NA")
  ) %>%
  separate(RankMeasure, into = c("Rank", "Measure"), sep = "(?<=Prof)(?=Salary|Comp)") %>%
  filter(Measure == "Salary") %>%
  mutate(
    Rank = case_when(
      Rank == "FullProf" ~ "Full",
      Rank == "AssocProf" ~ "Assoc",
      Rank == "AssistProf" ~ "Assist",
      TRUE ~ Rank
    ),
    Rank = factor(Rank, levels = c("Assist", "Assoc", "Full")),
    Tier = factor(Tier, levels = c("I", "IIA", "IIB"))
  ) %>%
  drop_na(Tier)

# Rank (x) v. Salary (y)
ggplot(salary_long, aes(x = Rank, y = Value, fill = Rank)) +
  geom_boxplot(outlier.size = 1.5, width = 0.6) +
  facet_grid(. ~ Tier) +
  scale_fill_manual(values = c("Assist" = "salmon", "Assoc" = "seagreen3", "Full" = "cornflowerblue")) +
  coord_cartesian(ylim = c(200, 1000)) +
  labs(y = "Salary", x = "Rank", fill = "Rank") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.x = element_text(size = 14, face = "bold"),
    legend.position = "right"
  )

## 2
#---------------------
anova_model <- aov(Value ~ State + Tier + Rank, data = salary_long)
summary(anova_model)

anova_table <- tidy(anova_model)
anova_table

## 3
#---------------------
juniper_raw <- read_csv("Juniper_Oils.csv")
glimpse(juniper_raw)
colnames(juniper_raw)

# Define the chemical compound columns
chemicals <- c(
  "alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene",
  "beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene",
  "cuparene","compound 1","alpha-chamigrene","widdrol","cedrol",
  "beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol",
  "alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2",
  "thujopsenal"
)

# Pivot longer to tidy format
juniper_tidy <- juniper_raw %>%
  pivot_longer(
    cols = all_of(chemicals),
    names_to = "Compound",
    values_to = "Concentration"
  )
glimpse(juniper_tidy)

## 4
#---------------------

# YearsSinceBurn (x) v. Concentration (y)
ggplot(juniper_tidy, aes(x = YearsSinceBurn, y = Concentration)) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linewidth = 1) +
  facet_wrap(~ Compound, scales = "free_y") +
  labs(x = "YearsSinceBurn", y = "Concentration") +
  theme_minimal(base_size = 14)

## 5
#---------------------

glm_results <- juniper_tidy %>%
  group_by(Compound) %>%
  do(tidy(glm(Concentration ~ YearsSinceBurn, data = ., family = gaussian())))

significant <- glm_results %>%
  filter(term == "YearsSinceBurn", p.value < 0.05)

significant_output <- significant %>%
  dplyr::select(term = Compound, estimate, std.error, statistic, p.value)

print(significant_output)

getwd()

