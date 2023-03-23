# First name: João Reis
library(tidyverse)
library(nycflights13)
library(dplyr)

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

View(planes)

manus_models <- planes %>%
  distinct(manufacturer, model) %>%
  group_by(manufacturer) %>%
  mutate(model_num = row_number()) %>%
  select(manufacturer, model_num, model) %>%
  spread(model_num, model) %>%
  rename_with(~ paste0("model", .x), 2:ncol(.)) %>% #chatGPT
  arrange(manufacturer)

View(manus_models)

#----------------------------------#----------------------------------#----------------------------------#

# Exercise 2: Using the flights and weather data generate a tibble where all values and
# variables are kept from the flights data, and in a new column called temp you include 
# the average temperature on the given day (you can calculate this from the weather dataset)

View(flights)
View(weather)

flights_temp <- flights %>%
  left_join(weather, by = c("year", "month", "day")) %>%
  group_by(year, month, day) %>%
  summarize(temp = mean(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(flights, by = c("year", "month", "day"))
  
View(flights_temp)
#----------------------------------#----------------------------------#----------------------------------#
  
# Exercise 3: Generate a tibble using the airports dataset, which contains data 

View(airports)
View(flights)

# a) only on airports which appear as destinations in the flights dataset. Keep all columns from airports
airports_in_flights <- airports %>%
  filter(faa %in% flights$dest)

View(airports_in_flights)

# b) only on airports which do not appear as destinations in the flights dataset. Keep all columns from airports
airports_notin_flights <- airports %>%
  filter(!(faa %in% flights$dest)) 

View(airports_notin_flights)
  

#----------------------------------#----------------------------------#----------------------------------#  
    
# Exercise 4: Using the msleep data look at the relationship between body weight
# and sleep time. 
  
# First, generate a scatterplot with bodyweight on the x axis
# and sleep_total on y, fit a linear line to the points and show the
# 90% confidence interval as well

ggplot #fixme

# Then, fit a linear model, store it in model1.
model1 <- #fixme

# Look at the plot of the residuals vs the fitted values. 


# Keep animals only with bodyweight below 100 and run a linear model again.

model2<-#fixme


#Run a model (use all animals) where you include the logged value of bodyweight on the right hand side

model3<-#fixme

        
#Run a model where along with log bodyweight you include vore on the right hand side.
#Set the reference category to herbivore in the model

        
model4<-#fixme



#Write a short intelligent discussion on what you learnt about the relationship between
#body weight and sleep time (max 10 sentences). Which model is your favorite from
#models 1-4 to describe the relationship, and why? You can use coefficients, R squares, significances,
#diagnostic plots, whatever you want, in your argument.
