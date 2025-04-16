library(tidyverse)

# 1. Read in the unicef data (10 pts)
unicef_raw <- read_csv("unicef-u5mr.csv")
glimpse(unicef_raw)

# 2. Get it into tidy format (10 pts)
unicef_tidy <- unicef_raw %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "Year",
    names_prefix = "U5MR.",
    values_to = "MortalityRate"
  ) %>%
  mutate(Year = as.integer(Year))

# 3. Plot each country’s U5MR over time (20 points)
plot1 <- ggplot(
  unicef_tidy %>% drop_na(MortalityRate),
  aes(x = Year, y = MortalityRate, group = CountryName)
) +
  geom_line(alpha = 0.6, color = "steelblue") +
  facet_wrap(~ Continent) +
  labs(
    title = "Under-5 Mortality Rate (U5MR) Over Time",
    y = "Deaths per 1000 live births",
    x = "Year"
  ) +
  theme_minimal(base_size = 14)

# 4. Save this plot as LASTNAME_Plot_1.png (5 pts)
ggsave("Ashbaker_Plot_1.png", plot = plot1, width = 10, height = 6, dpi = 300)

# 5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)

mean_u5mr <- unicef_tidy %>%
  group_by(Year, Continent) %>%
  summarize(Mean_U5MR = mean(MortalityRate, na.rm = TRUE), .groups = "drop")

ggplot(mean_u5mr, aes(x = Year, y = Mean_U5MR, color = Continent)) +
  geom_line(linewidth = 1.3) +
  labs(
    x = "Year",
    y = "Mean U5MR",
    title = "Under-5 Mortality Rate (U5MR) Over Time"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank()
  )

# 6. Save that plot as LASTNAME_Plot_2.png (5 pts)
ggsave("Ashbaker_Plot_2.png", width = 8, height = 6, dpi = 300)

# 7. Create three models of U5MR (20 pts)

# Model 1: U5MR ~ Year
mod1 <- lm(MortalityRate ~ Year, data = unicef_tidy)

# Model 2: U5MR ~ Year + Continent
mod2 <- lm(MortalityRate ~ Year + Continent, data = unicef_tidy)

# Model 3: U5MR ~ Year * Continent (includes interaction)
mod3 <- lm(MortalityRate ~ Year * Continent, data = unicef_tidy)

# 8. Compare the three models with respect to their performance

anova(mod1, mod2, mod3)
AIC(mod1, mod2, mod3)
BIC(mod1, mod2, mod3)

# Based on both ANOVA and AIC/BIC results, mod3 (which includes the interaction between Year and Continent) 
# performs best. It has the lowest residual sum of squares (RSS), the lowest AIC and BIC values, 
# and the improvement over mod1 and mod2 is statistically significant (p < 2.2e-16).

# This suggests that the effect of Year on MortalityRate varies across Continents,
# and the interaction is important to include.

# 9. Plot the 3 models’ predictions like so: (10 pts)

new_data <- expand.grid(
  Year = seq(min(unicef_tidy$Year, na.rm = TRUE), max(unicef_tidy$Year, na.rm = TRUE)),
  Continent = unique(unicef_tidy$Continent)
)

new_data <- new_data %>%
  mutate(
    pred_mod1 = predict(mod1, newdata = new_data),
    pred_mod2 = predict(mod2, newdata = new_data),
    pred_mod3 = predict(mod3, newdata = new_data)
  )

predictions_long <- new_data %>%
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = "Model",
    names_prefix = "pred_",
    values_to = "Predicted"
  )

ggplot(predictions_long, aes(x = Year, y = Predicted, color = Continent)) +
  geom_line(size = 1) +
  facet_wrap(~ Model) +
  labs(title = "Model predictions", y = "Predicted U5MR") +
  theme_minimal(base_size = 14)

ggsave("Ashbaker_Plot_3.png", width = 10, height = 6, dpi = 300)


# 10. BONUS - Using your preferred model, predict what the U5MR 
# would be for Ecuador in the year 2020. The real value for Ecuador 
# for 2020 was 13 under-5 deaths per 1000 live births. 
    #How far off was your model prediction???

ecuador_2020 <- tibble(
  CountryName = "Ecuador",
  Continent = "Americas",
  Year = 2020
)

# Use mod3 to predict
ecuador_pred <- predict(mod3, newdata = ecuador_2020)

# Compare to real value
real_value <- 13
difference <- ecuador_pred - real_value

tibble(Model = "mod3", Prediction = ecuador_pred, Reality = real_value, Difference = difference)

# mod3 prediction for Ecuador in 2020 was -10.6, which is obviously 
# not realistic for mortality rate (can’t have negative deaths per 1000)


# Create any model of your choosing that improves upon this 
# “Ecuadorian measure of model correctness.”

mod4 <- lm(log(MortalityRate) ~ Year * Continent, data = unicef_tidy)

ecuador_pred_log <- predict(mod4, newdata = ecuador_2020)
ecuador_pred4 <- exp(ecuador_pred_log)

real_value <- 13
difference4 <- ecuador_pred4 - real_value

tibble(Model = "mod4", Prediction = ecuador_pred4, Reality = real_value, Difference = difference4)

# Summary of Bonus Model (mod4)
    # Prediction: 12.0
    # Reality: 13
    #Difference: -1.00
 
# mod4 was off by just 1.0 death per 1000 live births, suggesting mod4 captures 
# the declining trend much more accurately than a basic linear model.


