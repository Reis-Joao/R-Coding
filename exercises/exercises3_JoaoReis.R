# First name: João Reis

library(tidyverse)
library(dplyr)
library(nycflights13)

# When you get stuck, read the help on the functions that you are using;
# search the help (via `??`) for specific types of functions, or read
# about a library; post on Slack or search online.

# Exercise 1: read documentation for `across` and figure out related functions
# Use one of them to solve the following problem of filtering only those
# rows of a df for which the columns named 'a' and 'b' are not NA, but other
# columns can be NA.
# For example: in the df below, you should be left only with the second and
# third row.

# ??across

df <- tibble(a = c(1, 2, 3, NA), b = c(NA, 2, 3, NA), c = c(1, NA, 3, 4))
a_b_notNA <- df %>%
  filter(if_all(c(a, b), ~ !is.na(.)))

# df
# a_b_notNA

#-----------------------#-----------------------#-----------------------#-----------------------#

# Exercise 2: Let us create summary statistics. For the flights data:
# - get the average dep_delay, arr_delay, and air_time per month
# You should get a 12x4 tibble with the following columns: month, dep_delay, arr_delay, air_time

avg_by_month <- flights %>%
  group_by(year,month) %>%
  summarise(
    dep_delay = mean(dep_delay, na.rm = TRUE),
    arr_delay = mean(arr_delay, na.rm = TRUE),
    air_time = mean(air_time, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(-year)

# avg_by_month

#-----------------------#-----------------------#-----------------------#-----------------------#

# Exercise 3: More summary statistics. For the flights data:
# - get the monthly mean, median, and standard deviation for
#   dep_delay, arr_delay, and air_time per month
# The output should have columns in this order: first the mean, median, sd,
# for dep_delay, then the mean, median, sd for arr_delay, then ... for air_time

stats_by_month <- flights %>%
  group_by(year, month) %>%
  summarise(across(c(dep_delay, arr_delay, air_time), list(mean = mean, median = median, sd = sd), na.rm = TRUE)) %>%
  # https://dplyr.tidyverse.org/reference/summarise_all.html
  ungroup() %>%
  select(-c(year)) # After discussion in class I didn't drop month, although it is not mentioned in the output columns

# stats_by_month

#-----------------------#-----------------------#-----------------------#-----------------------#

# Exercise 4: With the nycflights13 data, compute for each flight how
# many standard deviations worse it is in terms of dep_delay than
# the monthly average. This means you need to compare the dep_delay
# of a given flight to the average dep_delay in that month, and figure
# out how many standard deviations (for that month!) this is (negative
# standard deviations mean the dep_delay is shorter than average).
# Keep all the columns and rows, and put this new column as the first
# column, calling it stdevs_better_than_month.

delay_sddiff <- flights %>%
  filter(!is.na(dep_delay)) %>% # After discussion in class, I dropped flights with NA dep_delay before any computation, 
  # as the results are not meaningful to such flights. Comment the previous line if such dropping is not intended. 
  # (less than 10 thousand observations are dropped)
  group_by(year, month) %>%
  mutate(stdevs_better_than_month = (dep_delay - mean(dep_delay, na.rm = TRUE)) / sd(dep_delay, na.rm = TRUE)) %>%
  # If NAs are previously dropped, the na.rm = TRUE is redundant. 
  # I keep it in case one wants to comment out the filter line, keeping the NAs in the final output
  select(stdevs_better_than_month, everything())
  
# delay_sddiff


#-----------------------#-----------------------#-----------------------#-----------------------#

#Exercise 5. Copy the hotels_europe_strange.csv file to your working directory. This is a comma delimited file, the first row
#contains the variable names and the second one descriptions of the variables (variable labels).
#Read in the first six columns of the data into a tibble called hotels_europe.
#Your tibble should look like this: column names are from the first row of the csv, descriptions of the variables are not included,
#and column types are the following:
#hotel_id: integer, city: character, distance: double, stars: double, rating: double, country:character
#Missing values in the csv are denoted as "missing data", set these to NA in your tibble.

# setwd("C:/Users/João Reis/Documents/Master Economic Policy In Global Markets/SecondYear/WinterTerm/IntroductionToDataAnalysisInR/R-Coding/data")

hotels_europe <- read_csv("hotels-europe_strange.csv", col_names = TRUE, na = "missing data", col_types = "icdddc") %>%
  slice(-1) %>%
  select(1:6)

# hotels_europe

#-----------------------#-----------------------#-----------------------#-----------------------#

#Exercise 6.
#Write a function which reads in a csv, and checks if
#the first row of the data contains only character type values. If it does, should return
#TRUE, otherwise it should return FALSE.

check_first_row <- function(file_path) {
  data <- read_csv(file_path, col_names = FALSE, col_types = cols(),n_max = 1)
  all(sapply(data, is.character))
}


# check_first_row("hotels-europe_strange.csv")
# check_first_row("hotels-europe_strange - Copy.csv") # deleted the first two rows so the first row does not contain only characters
# check_first_row("hotels-europe_strange - Copy (2).csv") # deleted the first row so the first row contain only characters but also some NAs

# hotels_europe_strange <- read_csv("hotels-europe_strange.csv", col_names = FALSE, col_types = cols(), n_max = 1)
# hotels_europe_strange

#-----------------------#-----------------------#-----------------------#-----------------------#

#Exercise 7.
# Take the countries_wide tibble (generated here), where gdp_1999, gdp_2000, unemployment_1999 etc shows
# gdp in a given year. Convert the data to a long form, where there is only one column for gdp, and one for unemployment, with a separate column for year
#You should get a tibble with 16 rows and the following 6 columns, in this order:
# country, population, area, year, gdp, unemployment
#Hint: use the pivot_longer function

set.seed(3588)
countries_wide <- tibble(
  country = c("USA", "Canada", "Japan", "Germany"),
  gdp_1999 = c(round(runif(4, 20000, 50000), 2)),
  gdp_2000 = c(round(runif(4, 25000, 55000), 2)),
  gdp_2001 = round(runif(4, 30000, 60000), 2),
  population = round(runif(4, 5000000, 50000000)),
  area = round(runif(4, 100000, 1000000)),
  unemployment_1999 = round(runif(4, 1, 20), 2),
  unemployment_2000 = round(runif(4, 1, 20), 2),
  unemployment_2001 = round(runif(4, 1, 20), 2),
  unemployment_2002 = round(runif(4, 1, 20), 2)
)

# countries_wide

countries_long <- countries_wide %>%
  pivot_longer(cols = starts_with(c("gdp_", "unemployment_")), 
               names_to = c(".value", "year"), 
               names_sep = "_") %>%
  select(country, population, area, year, gdp, unemployment) %>%
  type_convert(col_types = cols(year = col_integer(), 
                                population = col_integer()))

# countries_long 


