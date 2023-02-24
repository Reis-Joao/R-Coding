# First name: João Reis

# When you get stuck, read the help on the functions that you are using;
# search the help (via `??`) for specific types of functions, or read
# about a library; post on Slack or search online.

library(nycflights13)
library(dplyr)
library(stringr)
library(tidyverse)

# Exercise 1: Get all the columns from the NYC 2013 flights data 
# that contain data about a time (of arrival, departure, etc) - but
# not about duration (e.g. flight duration). I.e. it is an event that
# happened at a specific time. Keep year, month, day too.
# Note: time_hour also counts, but air_time does not - it is about the 
# duration spent in the air, so it does not happen at a specific time.

flights_time <- flights %>%
  select(year, month, day, dep_time, sched_dep_time, arr_time, sched_arr_time, time_hour)

#flights_time


# Exercise 2: Get all the columns that contain text (i.e. character <chr>
# type) and where the contents of the columns are at most 3 characters 
# long. (NA data counts as 0-length.) 
# Look up how to get the length of strings in library stringr.

flights_text <- flights %>%
  select_if(is.character) %>% 
  select_if(~all(str_length(na.omit(.)) <= 3)) 
  # I am intentionally keeping columns that might have some NA observations
  # Since it makes no sense to exclude all the column just because some observations might be unknown.

#flights_text


# Exercise 3: arrange the starwars characters from tallest to smallest, 
# with NA's at the top. 

starwars_by_height <- starwars %>% 
  arrange(desc(is.na(height)), desc(height))

#starwars_by_height


# Exercise 4: arrange the starwars characters by species, then from 
# tallest to smallest WITHIN THEIR SPECIES! The NA's can be at the 
# bottom. Reorder the columns so that the name comes first, then the
# height, then the species, then the rest.

starwars_by_height_by_species <- starwars %>% 
  group_by(species) %>% 
  arrange(species, desc(height), na.last = TRUE) %>% 
  select(name, height, species, everything())

#starwars_by_height_by_species


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

flights_time_function <- function(data){
  data %>%
    select(any_of(c("year", "month", "day", "dep_time", "sched_dep_time", "arr_time", "sched_arr_time", "time_hour"))) 
    #https://stackoverflow.com/questions/43786883/how-do-i-select-columns-that-may-or-may-not-exist
} 

# flights_subset1 <- select(flights, time_hour, month, day, year, carrier)
# flights_subset2 <- select(flights, year, month, day, dep_time, origin, hour, minute, air_time, arr_time)
# flights_time_function(flights_subset1)
# flights_time_function(flights_subset2)


# This one is probably hard to do without `where` and understanding
# functions as arguments. Search for another solution.

flights_text_function <- function(data){
  data %>%
    select_if(is.character) %>%
    mutate_all(~na_if(., "")) %>%
    select_if(~all(str_length(na.omit(.)) <= 3))
}
    # In here I am also transforming empty strings into NAs, in case Professor adds some empty rows.
    # Like that I keep a column that has every non-NA values up to 3 characters, even when it has NA/empty strings
    # This avoids the code to return an empty output if, for example, someone adds an empty row by mistake
    # https://sparkbyexamples.com/r-programming/replace-empty-string-with-na-in-r-dataframe/#:~:text=R%20%E2%80%93%20Replace%20Empty%20String%20with%20NA%201,Replace%20on%20All%20Character%20columns%20...%20Mais%20itens

# flights_subset1 <- select(flights, year, month, day, time_hour, carrier, dest, origin)
# flights_subset2 <- select(flights, year, time_hour, origin)
# flights_text_function(flights_subset1)
# flights_text_function(flights_subset2)


# Assume that species and height are present so you can arrange by.
# Do not assume that name is present, but put it first if it is.

starwars_by_height_by_species_function <- function(data) {
  data %>%
    group_by(species) %>%
    arrange(species, desc(height), na.last = TRUE) %>%
    select(any_of(c("name", "height", "species")), everything()) 
}

# starwars_subset1 <- select(starwars, height, species, films, skin_color)
# starwars_subset2 <- select(starwars, height, species, sex, name, vehicles)
# starwars_by_height_by_species_function(starwars_subset1)
# starwars_by_height_by_species_function(starwars_subset2)


# Exercise 6: Using the flights data, for each carrier, find the flight 
# that had the smallest and the longest departure delay.
# Put these 2 numbers into a column of their own, called 'best' (shortest 
# delay) and 'worst' (longest delay).
# Keep the columns carrier, best, and worst in that order.

flights_carrier_best_worst <- flights %>%
  group_by(carrier) %>%
  summarise(best = min(dep_delay, na.rm = TRUE), worst = max(dep_delay, na.rm = TRUE)) %>%
  select(carrier, best, worst)

# If the idea is to keep the flight number instead of the delay (comment out the following lines):

# flights_carrier_best_worst <- flights %>%
#   group_by(carrier) %>%
#   summarize(best = flight[which(dep_delay == min(dep_delay, na.rm = TRUE))][1],
#             worst = flight[which(dep_delay == max(dep_delay, na.rm = TRUE))][1]) %>%
#   # https://stackoverflow.com/questions/42132028/how-to-get-the-observation-value-by-using-the-index-number-in-r
#   select(carrier, best, worst)

# We could eliminate the [1] if we wanted to keep all observations in which, for the same carrier,
# there are more than one flight with the same max/min dep_delay

# flights_carrier_best_worst

# Exercise 7: Find the days such that the most delayed flight (by departure)
# on that day of the year is more delayed than the most delayed flight on the 
# previous day. Keep year, month, day, and dep_delay columns.

flights_days_getting_worse <- flights %>%
  select(year, month, day, dep_delay) %>%
  group_by(year, month, day) %>%
  filter(dep_delay == max(dep_delay, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(dep_delay > lag(dep_delay)) %>%
  arrange(year, month, day, dep_delay)

#flights_days_getting_worse



