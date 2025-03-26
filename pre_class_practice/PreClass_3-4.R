library(tidyverse)


table1
table4a
table4b

## make table4a and table4b tidy (like table1)
x <- table4a %>% 
  pivot_longer(-country,
               names_to = 'year',
               values_to = 'cases') 

y <- table4b %>% 
  pivot_longer(-country,
               names_to = 'year',
               values_to = 'population') 

table4_tidy <- full_join(x, y)

table5

## make table5 tidy

paste0(table5$century, table5$year)
paste0(table5$year, table5$century)
paste0(table5$year, table5$century, 'i')

paste(table5$year, table5$century, 'i', sep = 'xxxx')


table5 %>% 
  separate(rate, c('cases', 'population')) %>% 
  mutate(year = paste0(table5$century, table5$year)) %>% 
  select(-century) %>% 
  View()

table5_tidt <- table5 %>% 
  separate(rate, c('cases', 'population'), convert = T) %>% 
  mutate(year = paste0(table5$century, table5$year)) %>% 
  select(-century) 


## entring data to excel (or Google Sheet)
## path: /Users/yu-yaliang/Desktop/BIOL3100/Data_Course_LASTNAME/Exercises/Data_Entry_Case_Study.txt
## path: Exercises/Data_Entry_Case_Study.txt
getwd()

text <- read.delim('Exercises/Data_Entry_Case_Study.txt')


##
library(readxl)

dat <- read_xlsx('Data/messy_bp.xlsx', skip = 3)
View(dat)

dat$Race %>% unique()


bp <- dat %>% 
  select(-starts_with('HR'))

bp <- bp %>% 
  pivot_longer(starts_with('BP'),
               names_to = 'visit',
               values_to = 'bp') %>% 
  mutate(visit = case_when(visit == 'BP...8' ~ 1,
                           visit == 'BP...10' ~ 2,
                           visit == 'BP...12' ~ 3,)) %>% 
  separate(bp, into = c('systolic', 'diatolic')) 

hr <- dat %>% 
  select(-starts_with('BP')) 

hr <- hr %>% 
  pivot_longer(starts_with('HR'),
               names_to = 'visit',
               values_to = 'hr') %>% 
  mutate(visit = case_when(visit == 'HR...9' ~ 1,
                           visit == 'HR...11' ~ 2,
                           visit == 'HR...13' ~ 3,))

dat_join <- full_join(bp, hr)
View(dat_join)

head(dat_join)
colnames(dat_join)

dat_join %>% 
  mutate(Race = case_when(Race == 'Caucasian' ~ 'White', 
                          Race == 'WHITE' ~ 'White', 
                          TRUE ~ Race)) %>% View()

dat_join <- dat_join %>% 
  mutate(Race = case_when(Race == 'Caucasian' | Race == 'WHITE' ~ 'White', 
                          TRUE ~ Race)) %>% View()

## '|' means 'or' -- '&' means 'and' obviously

# Check if dat_join exists and its structure
print(dat_join) # Should not be NULL
colnames(dat_join) # Should include 'systolic', 'diatolic', 'visit', and 'pat_id'

dat_join %>%
  group_by(pat_id) %>%
  summarise(n = n_distinct(Race, Sex, Hispanic, `Month of birth`, `Day birth`, `Year birth`)) %>%
  filter(n > 1)

library(dplyr)

dat_join <- dat_join %>%
  group_by(Race, Sex, Hispanic, `Month of birth`, `Day birth`, `Year birth`) %>%
  mutate(unique_id = cur_group_id()) %>%
  ungroup() %>%
  select(unique_id, everything())

dat_join %>%
  group_by(unique_id) %>%
  summarise(n = n_distinct(Race, Sex, Hispanic, `Month of birth`, `Day birth`, `Year birth`)) %>%
  filter(n > 1)

view(dat_join)

library(ggplot2)

dat_join <- dat_join %>%
  mutate(
    visit = factor(visit, levels = c(1, 2, 3)),  # Ensure proper visit order
    systolic = as.numeric(systolic),
    diatolic = as.numeric(diatolic)
  )

ggplot(dat_join, aes(x = visit, group = unique_id)) + 
  geom_line(aes(y = systolic, color = "Systolic"), size = 1) + 
  geom_line(aes(y = diatolic, color = "Diastolic"), size = 1, linetype = "dashed") +
  labs(title = "Blood Pressure Changes Over Visits",
       x = "Visit Number",
       y = "Blood Pressure (mmHg)",
       color = "Measurement") +
  theme_minimal() +
  scale_color_manual(values = c("Systolic" = "red", "Diatolic" = "blue")) +
  theme(legend.position = "top")  # Move legend to top for better visibility

##

dat <- read.csv('Data/Bird_Measurements.csv')
view(dat)
summary(dat)

dat <- dat %>%
  janitor::clean_names()

summary(dat)

