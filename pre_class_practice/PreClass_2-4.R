data(peng)

library(tidyverse)
library(palmerpenguins)
penguins

##1.1- Find the fatty penguins (body_mass > 5000)

fatty_penguins <- penguins %>% 
  filter(body_mass_g > 5000)
fatty_penguins

##1.2- Count how many are male and how many are female

fatty_male_female_counts <- fatty_penguins %>% 
  count(sex)
fatty_male_female_counts

##1.3- Find the max body mass for male and female

max_body_mass_by_sex <- fatty_penguins %>% 
  group_by(sex) %>% 
  summarize(max_body_mass = max(body_mass_g, na.rm = TRUE))
max_body_mass_by_sex

##2.1- Add a new column to penguins to dataset that says whether they're fat

fat_data_penguins <- penguins %>% 
  mutate(fat = if_else(body_mass_g > 5000, "fat", "not fat"))
fat_data_penguins

size_data_penguins <- penguins %>%
  mutate(size_category = case_when(
    body_mass_g > 5000 ~ "Fat", 
    body_mass_g <= 5000 & body_mass_g > 3000 ~ "Normal",  # Added comma here
    body_mass_g < 3000 ~ "Skinny", 
    TRUE ~ "NA"
  ))
size_data_penguins

penguins_fat_size <- penguins %>%
  mutate(
    fat = if_else(body_mass_g > 5000, "fat", "not fat"),
    size_category = case_when(
      body_mass_g > 5000 ~ "Fat", 
      body_mass_g <= 5000 & body_mass_g > 3000 ~ "Normal",  
      body_mass_g < 3000 ~ "Skinny", 
      TRUE ~ "NA"
    )
  )
penguins_fat_size


ggplot(data = penguins_fat_size)
view(penguins_fat_size)


penguins_fat_size %>%
  ggplot(aes(x = bill_length_mm, y = body_mass_g, color = sex)) +
geom_point()#+

data(peng)

  