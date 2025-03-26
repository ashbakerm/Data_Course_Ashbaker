library(tidyverse)

dat <- read_csv("Data/BioLog_Plate_Data.csv")

dat_long <- dat %>%
  pivot_longer(cols = starts_with("Hr_"),
               names_to = "Time",
               names_prefix = "Hr_",
               values_to = "Absorbance") %>%
  mutate(Time = as.numeric(Time))

dat_long <- dat_long %>%
  mutate(Sample_Type = case_when(
    str_detect(`Sample ID`, "Soil") ~ "Soil",
    str_detect(`Sample ID`, "Water|Clear_Creek") ~ "Water",
    TRUE ~ NA_character_
  ))

mean_data <- dat_long %>%
  group_by(Time, `Sample ID`, Dilution) %>%
  summarize(Mean_Absorbance = mean(Absorbance, na.rm = TRUE), .groups = "drop")

ggplot(mean_data, aes(x = Time, y = Mean_Absorbance, color = `Sample ID`)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Dilution, labeller = label_both) +
  theme_minimal() +
  labs(title = "Mean Absorbance over Time by Dilution",
       x = "Time (hours)",
       y = "Mean Absorbance",
       color = "Sample ID")

animated_plot <- ggplot(mean_data, aes(x = Time, y = Mean_Absorbance, color = `Sample ID`)) +
  geom_line(size = 1.2) +
  facet_wrap(~ Dilution, labeller = label_both) +
  theme_minimal() +
  labs(title = "Mean Absorbance Over Time | Time: {frame_along} hrs",
       x = "Time (hours)",
       y = "Mean Absorbance",
       color = "Sample ID") +
  transition_reveal(Time)


animate(animated_plot, nframes = 100, fps = 10)

anim_save("mean_absorbance_animation.gif", animated_plot)
