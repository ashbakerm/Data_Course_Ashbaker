library(gapminder)
library(ggimage)
library(gganimate)
library(patchwork)
library(ggmap)
library(tidyverse)

# Read the data
df <- read.csv('Data/wide_income_rent.csv')

# Convert from wide to long format
df_long <- df %>%
  pivot_longer(cols = -variable, names_to = "State", values_to = "Value") %>%
  pivot_wider(names_from = variable, values_from = Value)

view(df_long)

# Plot rent for each state
ggplot(df_long, aes(x = reorder(State, rent), y = rent, fill = State)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Rent Prices by State",
       x = "State",
       y = "Average Rent") +
  theme_minimal() +
  theme(legend.position = "none")

table2
table2 %>%
  pivot_wider(names_from = type, values_from = count)

table3
table3 %>% 
  separate(rate, c('cases', 'population'))

table4a
table4b
