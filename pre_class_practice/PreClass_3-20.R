library(tidyverse)
library(janitor)

dat <- read.csv('Data/BioLog_Plate_Data.csv')
View(dat)

## clean names
dat <- clean_names(dat)

## Create a new col = time, pivot longer
dat_long <- dat %>%
  pivot_longer(
    cols = starts_with("hr"), 
    names_to = "time",
    values_to = "value"
  )

## Create a new col = type (soil or water)
dat_long <- dat_long %>%
  mutate(type = if_else(str_detect(sample_id, "soil"), "soil", "water"))

View(dat_long)


dat <- read_xlsx('Data/height.xlsx')

dat_2 <- dat %>% 
  pivot_longer(everything(),
               names_to = 'sex',
               values_to = 'height')




