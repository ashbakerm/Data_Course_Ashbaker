library(tidyverse)
library(modelr)
library(easystats)
library(broom)

data <- read_csv("/Users/mylesashbaker/Desktop/Data_Course_Ashbaker/Data/mushroom_growth.csv")
glimpse(data)

# Convert categorical variables
data <- data %>%
  mutate(
    Humidity = factor(Humidity),
    Species = factor(Species)
  )

# Exploratory Plots
ggplot(data, aes(x = Light, y = GrowthRate)) +
  geom_point() + geom_smooth(method = "lm") + theme_minimal()

ggplot(data, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() + geom_smooth(method = "lm") + theme_minimal()

ggplot(data, aes(x = Temperature, y = GrowthRate)) +
  geom_point() + geom_smooth(method = "lm") + theme_minimal()

ggplot(data, aes(x = Humidity, y = GrowthRate)) +
  geom_boxplot() + theme_minimal()

ggplot(data, aes(x = Species, y = GrowthRate)) +
  geom_boxplot() + theme_minimal()

# Model Building
mod1 <- lm(GrowthRate ~ Light + Nitrogen + Temperature, data = data)
mod2 <- lm(GrowthRate ~ Light * Nitrogen + Temperature, data = data)
mod3 <- lm(GrowthRate ~ Light * Nitrogen * Temperature, data = data)
mod4 <- lm(GrowthRate ~ Light * Nitrogen * Temperature + Humidity + Species, data = data)

# Model Evaluation; Mean Squared Error
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
mean(mod4$residuals^2)

compare_performance(mod1, mod2, mod3, mod4, rank = TRUE)

# mod4	Light * Nitrogen * Temperature + Humidity + Species	
  # MSE = 5040.02, R^2 = 0.482, Best Performance
newdata <- tibble(
  Light = c(10, 20, 30),
  Nitrogen = c(10, 20, 30),
  Temperature = c(25, 25, 25),
  Humidity = factor(c("Low", "Medium", "High"), levels = levels(data$Humidity)),
  Species = factor("P.ostreotus", levels = levels(data$Species))
)

preds <- predict(mod4, newdata)
pred_df <- bind_cols(newdata, pred = preds, Type = "Hypothetical")

real_df <- data %>%
  mutate(pred = predict(mod4), Type = "Observed")

combined <- bind_rows(real_df, pred_df)

ggplot(combined, aes(x = Light, y = pred, color = Type)) +
  geom_point() +
  geom_point(data = real_df, aes(y = GrowthRate), color = "black") +
  theme_minimal() +
  labs(y = "Growth Rate (predicted or observed)")











