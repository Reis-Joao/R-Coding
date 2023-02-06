# Name of submitter: João Reis
library(dplyr)
library(stringr)
library(ggplot2)

# Exercise 1: Define the function sq that squares a single number x
sq <- function(x) {
  x^2
}
stopifnot((sq(2) == 4) && (sq(-2) == 4))
sq(2) == sq(-2) 
sq(2) == sq(3)
# Exercise 2: From the `starwars` data, get all the non-human characters with yellow or blue-gray eyes.
# Keep all the columns.
View(starwars)
non_human_eyes <- starwars %>%
  filter(species != "Human") %>%
  filter(eye_color == "yellow" | eye_color == "blue-gray")
  # filter(grepl("yellow", eye_color) | grepl("blue-gray", eye_color)) if the idea is to keep even when yellow is not the only colour
  # https://stackoverflow.com/questions/18587334/subset-data-to-contain-only-columns-whose-names-match-a-condition
View(non_human_eyes)  

# Exercise 3: write the body of the function `non_human_hair` that takes a single argument.
# This argument is a subset from the `starwars` data, and your function should return all the
# non-human characters who could possibly have brown, auburn, or no hair
# Keep only the following columns: name, species, eye_color, homeworld, and hair_color IN THAT ORDER
# Order the rows by species, then eye_color, both ascending alphabetically
non_human_hair <- function(df) {
  df %>%
    filter(species != "Human") %>%
    filter(hair_color %in% c("brown", "auburn", "none", "NA")) %>%
    select(name, species, eye_color, homeworld, hair_color) %>%
    arrange(species, eye_color)
}
new_starwars <- non_human_hair(starwars)
View(new_starwars)

#Use the `msleep` data (bulit-in dataset in the dplyr package) for Exercises 4-7

#Exercise 4. Get all the animals who are heavier than the average bodyweight in the data
#Keep the "name" and "bodywt" of these animals
#Order the rows by bodyweight in a descending order
View(msleep)
heavy_animals <- msleep %>%
  mutate(average_bodywt = mean(bodywt)) %>%
  filter(bodywt > average_bodywt) %>%
  select(name, bodywt) %>%
  arrange(desc(bodywt))

View(heavy_animals)

#Exercise 5. Create a new column called brainwt_ratio showing the ratio of
# brain mass to the total body weight. Round the ratio to 4 digits. Keep the name and brainwt colums
# and keep the 10 animals with the highest relative brain mass.

clever_animals <- msleep %>%
  mutate(brainwt_ratio = round(brainwt / bodywt, 4)) %>%
  top_n(10, brainwt_ratio) %>% #https://datascience.stackexchange.com/questions/100261/filter-for-top-10-highest-values-of-group-in-dataset-in-r
  # Although I know I could just order it with arrange(desc) as in class, and slice the first 10 rows
  arrange(desc(brainwt_ratio)) %>%
  select(name, brainwt_ratio)
  
View(clever_animals)

#Exercise 6 Create a new column called brainwt_ratio, and keep only this column.
# Use the transmute command
brainweight <- msleep %>%
  transmute(brainwt_ratio = brainwt / bodywt)

View(brainweight)

#Exercise 7 Check whether carnivores, herbivores, insectivores, or omnivores sleep more.
# First, remove the rows where vore is missing (NA)
# Create a data table with 4 rows and 3 columns showing the average,
# and the standard deviation for total sleep time for these 4 groups
meansleep_by_vore <- msleep %>%
  filter(!is.na(vore)) %>% #It works with != "NA" but it does not keep the "NA" observations if I use == "NA". Why?
  group_by(vore) %>%
    summarize(average_sleep = mean(sleep_total), standard_deviation = sd(sleep_total))

View(meansleep_by_vore)  


