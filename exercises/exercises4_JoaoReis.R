# First name: João Reis
library(tidyverse)
library(nycflights13)
library(dplyr)
library(ggplot2)
library(modelr)
library(cowplot)

# When you get stuck, read the help on the functions that you are using;
# search the help (via `??`) for specific types of functions, or read
# about a library; post on Slack or search online.


# Exercise 1: Use the "planes" dataset from the nycflights13 library.
# Generate a table where you can look at the models of each manufacturer. Specifically,
# make a tibble called manus_models  where there is one row for every manufacturer. Put the name of the manufacturer
# in the first column, and the models of this manufacturer to the next columns called 
# model1, model2, model3, etc. Every  model should only appear once. If a manufacturer has only one model,
# model2, model3 etc should be NA  Arrange the manufacturers alphabetically.
# You should get a tibble with 35 observations and 66 variables

#View(planes)

manus_models <- planes %>%
  distinct(manufacturer, model) %>%
  group_by(manufacturer) %>%
  mutate(model_num = row_number()) %>%
  pivot_wider(names_from = model_num, values_from = model, names_prefix = "model") %>%
  arrange(manufacturer)

#View(manus_models)

#----------------------------------#----------------------------------#----------------------------------#

# Exercise 2: Using the flights and weather data generate a tibble where all values and
# variables are kept from the flights data, and in a new column called temp you include 
# the average temperature on the given day (you can calculate this from the weather dataset)

#View(flights)
#View(weather)

flights_temp <- flights %>%
  left_join(weather, by = c("year", "month", "day")) %>%
  group_by(year, month, day) %>%
  summarize(temp = mean(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(flights, by = c("year", "month", "day"))
  
#View(flights_temp)
#----------------------------------#----------------------------------#----------------------------------#
  
# Exercise 3: Generate a tibble using the airports dataset, which contains data 

#View(airports)
#View(flights)

# a) only on airports which appear as destinations in the flights dataset. Keep all columns from airports
airports_in_flights <- airports %>%
  filter(faa %in% flights$dest)

#View(airports_in_flights)

# b) only on airports which do not appear as destinations in the flights dataset. Keep all columns from airports
airports_notin_flights <- airports %>%
  filter(!(faa %in% flights$dest)) 

#View(airports_notin_flights)
  

#----------------------------------#----------------------------------#----------------------------------#  
    
# Exercise 4: Using the msleep data look at the relationship between body weight
# and sleep time. 

#View(msleep)
  
# First, generate a scatterplot with bodyweight on the x axis
# and sleep_total on y, fit a linear line to the points and show the
# 90% confidence interval as well

ggplot(msleep, aes(x = bodywt, y = sleep_total)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, level = 0.90) +
  xlab("Body weight (kg)") + 
  ylab("Total sleep time (hrs)") +
  ggtitle("Relationship between body weight and sleep time")

# Then, fit a linear model, store it in model1.
model1 <- lm(sleep_total ~ bodywt, data = msleep)

#model1

# Look at the plot of the residuals vs the fitted values. 
(df <- msleep %>%
    add_predictions(model1) %>%
    add_residuals(model1))

ggplot(df, aes(pred, resid)) +
  geom_point() +
  geom_point(aes(pred, resid), color = "red")

# Keep animals only with bodyweight below 100 and run a linear model again.

model2 <- lm(sleep_total ~ bodywt, data = msleep, subset = bodywt < 100)

#model2

#Run a model (use all animals) where you include the logged value of bodyweight on the right hand side

model3 <- lm(sleep_total ~ log(bodywt), data = msleep)

#model3
        
#Run a model where along with log bodyweight you include vore on the right hand side.
#Set the reference category to herbivore in the model

model4 <- lm(sleep_total ~ log(bodywt) + fct_relevel(vore, ref = "herbi"), data = msleep)
# https://stackoverflow.com/questions/3872070/how-to-force-r-to-use-a-specified-factor-level-as-reference-in-a-regression
# https://stackoverflow.com/questions/66177938/relevel-only-works-with-unordered-factors

#model4

#Write a short intelligent discussion on what you learnt about the relationship between
#body weight and sleep time (max 10 sentences). Which model is your favorite from
#models 1-4 to describe the relationship, and why? You can use coefficients, R squares, significances,
#diagnostic plots, whatever you want, in your argument.

summary(model1)
summary(model2)
summary(model3)
summary(model4)

df_model1 <- msleep %>%
  add_predictions(model1) %>%
  add_residuals(model1)

df_model2 <- msleep %>%
  add_predictions(model2) %>%
  add_residuals(model2)

df_model3 <- msleep %>%
  add_predictions(model3) %>%
  add_residuals(model3)

df_model4 <- msleep %>%
  add_predictions(model4) %>%
  add_residuals(model4)

# Create plots
plot1 <- ggplot(df_model1, aes(pred, resid)) +
  geom_point() +
  geom_point(aes(pred, resid), color = "red") +
  ggtitle("Model 1")

plot2 <- ggplot(df_model2, aes(pred, resid)) +
  geom_point() +
  geom_point(aes(pred, resid), color = "red") +
  ggtitle("Model 2")

plot3 <- ggplot(df_model3, aes(pred, resid)) +
  geom_point() +
  geom_point(aes(pred, resid), color = "red") +
  ggtitle("Model 3")

plot4 <- ggplot(df_model4, aes(pred, resid)) +
  geom_point() +
  geom_point(aes(pred, resid), color = "red") +
  ggtitle("Model 4")

# Arrange plots together in one figure
plot_grid(plot1, plot2, plot3, plot4, ncol = 2)

# ANSWER: Based on our previous analysis, it looks like that there is a slightly negative relation between body weight and sleeping time, 
# captured by the scatter plot drawn initially. However, large outliers in terms of body weight leads to a distortion in the truth 
# relationship between these two variables. Model 1 is the linear regression observed in the scatter plot, and it translate the same 
# findings: a negative and statistically significant relationship between body weight and total sleeping time. However, as mentioned 
# previously, the model has a very low predictive power, what can be seen both by its Adjusted R-squared of 0.086. Model 2 improves 
# slightly Model 1, getting rid of those extreme outliers, what turns out to give us a stronger negative coefficient. However, it is still
# a precarious model, with an Adjusted R-squared of 0.1187. We can also observe the lack of quality of both models by the plot of their 
# residuals on their fitted values, with the residuals not being randomly distributed around 0 (as it is desirable).

# Models 3 and 4 significantly improve the previous models, with Adjusted R-squared of 0.315 and 0.3886 respectively. This significant 
# improvement can also be seen in the residuals plot, with both being fairly similar and the distribution of their residuals being the 
# expected and desired one (randomly around 0). However, I prefer Model 4, not only because it seems slightly better in terms of what can 
# explain in this relationship (higher R-squared statistics), but also because it allows for an interesting comparison among different 
# types of eating habit animals (that is likely to influence the relationship bodyweight/sleeping time, as it affects their energy 
# consumption, etc). In fact, such can be seen by the coefficients on carnivore and insectivore, that are significant at 5% confident 
# level (meaning that their relationship differ from the reference category, herbivore). 

