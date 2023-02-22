# First name: João Reis

# When you get stuck, read the help on the functions that you are using;
# search the help (via `??`) for specific types of functions, or read
# about a library; post on Slack or search online.

# Exercise 1: Get all the columns from the NYC 2013 flights data 
# that contain data about a time (of arrival, departure, etc) - but
# not about duration (e.g. flight duration). I.e. it is an event that
# happened at a specific time. Keep year, month, day too.
library(nycflights13)
View(flights)
# Note: time_hour also counts, but air_time does not - it is about the 
# duration spent in the air, so it does not happen at a specific time.
flights_time <- flights %>%
  select(year, month, day, dep_time, sched_dep_time, arr_time, sched_arr_time, time_hour)

# Should we include the columns hour and minute?

View(flights_time)

# Exercise 2: Get all the columns that contain text (i.e. character <chr>
# type) and where the contents of the columns are at most 3 characters 
# long. (NA data counts as 0-length.) 
# Look up how to get the length of strings in library stringr.
library(dplyr)
library(stringr)

flights_text <- flights %>%
  select_if(is.character) %>%   # select only character type columns
  select_if(~sum(str_length(na_if(.,"")) <= 3) > 0)  # select columns with length <= 3

View(flights_text)

# Exercise 3: arrange the starwars characters from tallest to smallest, 
# with NA's at the top. 
library(tidyverse)

starwars_by_height <- starwars %>% 
  arrange(desc(is.na(height)), desc(height))

View(starwars_by_height)

# Exercise 4: arrange the starwars characters by species, then from 
# tallest to smallest WITHIN THEIR SPECIES! The NA's can be at the 
# bottom. Reorder the columns so that the name comes first, then the
# height, then the species, then the rest.
starwars_by_height_by_species <- starwars %>% 
  group_by(species) %>% 
  arrange(species, desc(height), na.last = TRUE) %>% 
  select(name, height, species, everything())

View(starwars_by_height_by_species)

# Exercise 5: the preceding exercises asked you to compute the given
# dataframe for a specific known dataset. Suppose that we give you a
# subset of the dataset, e.g. in exercise 4 we give you a subset 
# called df and you have to rearrange it as described. Write a function
# that returns the rearranged dataset. Do this for Exercise 1, 2, and 4.
# The names for the functions are provided below, you have to define
# the function and arguments.

# The point of this exercise is that you can no longer hardcode some
# ugly parts and check them or fix them by manually looking at the 
# data, you have to deal with the issues in code and take care of
# all the edge cases automatically.
# Notes: we will test your function with subsets of the data.
# We can drop columns or rows arbitrarily and your code should work.
# We will not add columns or rows. 
# You should interpret the exercise naturally: when we ask to include
# year, month, and day from a tibble that only has year, then of course
# you cannot include month and day, but you should include year.
# You should test that your code works by trying out some subsets,
# both column- and row-wise.

# FIXME: comment out and define function
# flights_time_function <- ...

# This one is probably hard to do without `where` and understanding
# functions as arguments. Search for another solution.

# FIXME: comment out and define function
# flights_text_function <- ...

# Assume that species and height are present so you can arrange by.
# Do not assume that name is present, but put it first if it is.

# FIXME: comment out and define function
# starwars_by_height_by_species_function <- ...


# Exercise 6: Using the flights data, for each carrier, find the flight 
# that had the smallest and the longest departure delay.
# Put these 2 numbers into a column of their own, called 'best' (shortest 
# delay) and 'worst' (longest delay).
# Keep the columns carrier, best, and worst in that order.
flights_carrier_best_worst <- flights %>%
  group_by(carrier) %>%
  summarize(best = min(dep_delay, na.rm = TRUE), worst = max(dep_delay, na.rm = TRUE)) %>%
  select(carrier, best, worst)

View(flights_carrier_best_worst)

# Exercise 7: Find the days such that the most delayed flight on that
# day is more delayed than the most delayed flight on the previous day.
flights_days_getting_worse <- flights %>%
  group_by(year, month, day) %>%
  summarize(max_arr_delay = max(arr_delay, na.rm = TRUE)) %>%
  mutate(prev_day_max_delay = lag(max_arr_delay)) %>%
  filter(max_arr_delay > prev_day_max_delay)

View(flights_days_getting_worse)
