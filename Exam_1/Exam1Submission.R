library(tidyverse)
library(ggplot2)

# I. Read the data (20 pts)
covid_df <- read_csv("cleaned_covid_data.csv")

# II. Subset to states beginning with "A" (20 pts)
A_states <- covid_df %>%
  filter(grepl("^A", Province_State))

# III. Plot Deaths over time for A states (20 pts)
ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  labs(title = "COVID-19 Deaths Over Time in 'A' States",
       x = "Date",
       y = "Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# IV. Find peak Case_Fatality_Ratio by state (20 pts)
state_max_fatality_rate <- covid_df %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

# V. Bar plot of maximum fatality rates (20 pts)
state_max_fatality_rate$Province_State <- factor(state_max_fatality_rate$Province_State, 
                                                 levels = state_max_fatality_rate$Province_State)

ggplot(state_max_fatality_rate, aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col() +
  labs(title = "Maximum Case Fatality Ratio by State",
       x = "State",
       y = "Max Fatality Ratio") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# VI. BONUS: Cumulative US deaths over time (10 pts)
us_deaths <- covid_df %>%
  group_by(Last_Update) %>%
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE)) %>%
  mutate(Cumulative_Deaths = cumsum(Total_Deaths))

ggplot(us_deaths, aes(x = as.Date(Last_Update), y = Cumulative_Deaths)) +
  geom_line() +
  labs(x = "Date", y = "Cumulative Deaths", 
       title = "Cumulative COVID-19 Deaths in the US") +
  theme_minimal()
