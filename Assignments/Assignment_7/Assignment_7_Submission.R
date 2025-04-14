library(tidyverse)
library(ggplot2)
library(janitor)

relig_df <- read_csv("Utah_Religions_by_County.csv")
view(relig_df)
#----------------------------------------------------------------------------------
# Clean column names to be consistent and easy to reference in code
relig_df <- relig_df %>%
  clean_names()
view(relig_df)
#----------------------------------------------------------------------------------
# Pivot religious affiliation columns into long format 
# -> each row will  represent a county–religion–proportion observation
tidy_relig <- relig_df %>%
  pivot_longer(
    cols = -(county:non_religious),
    names_to = "religion",
    values_to = "proportion"
  )
view(tidy_relig)

#----------------------------------------------------------------------------------
##Q1 — Does county population correlate with the proportion of a specific religion?
#----------------------------------------------------------------------------------
# Filter for LDS to isolate relationship
lds_data <- tidy_relig %>%
  filter(religion == "lds")
view(lds_data)

# Scatterplot with trend line to visually show correlation
ggplot(lds_data, aes(x = pop_2010, y = proportion)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "County Population vs. LDS Proportion",
       x = "County Population (2010)",
       y = "Proportion LDS")

# Correlation
cor(lds_data$pop_2010, lds_data$proportion)

#----------------------------------------------------------------------------------
##Q2 — Does a religion's proportion correlate with non-religious population?
#----------------------------------------------------------------------------------

# Pull relevant columns (non_religious and selected religions)
relig_vs_non <- relig_df %>%
  select(county, non_religious, lds, catholic, evangelical)
view(relig_vs_non)

# Plot LDS vs. non-religious to explore inverse relationships
ggplot(relig_vs_non, aes(x = lds, y = non_religious)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "LDS vs. Non-Religious Proportion by County",
       x = "LDS Proportion",
       y = "Non-Religious Proportion")

# Correlation
cor(relig_vs_non$lds, relig_vs_non$non_religious)

#----------------------------------------------------------------------------------
# Thought Process: # I wonder what the overall religious groups vs non-religous per county looks like:
                 # Would be cool to show several religions vs. non-religious group proportion per county
#----------------------------------------------------------------------------------

# Define a set of top religions to explore visually
top_religs <- c("lds", "catholic", "evangelical", "non_denomination", "muslim", "assemblies_of_god")

# Filter top religions and create plot to compare patterns
tidy_relig %>%
  filter(religion %in% top_religs) %>%
  ggplot(aes(x = proportion, y = non_religious)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ religion, scales = "free_x") +
  labs(title = "Religious Group Proportion vs. Non-Religious by County",
       x = "Religious Group Proportion",
       y = "Non-Religious Proportion")

