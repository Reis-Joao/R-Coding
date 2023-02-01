# Function definition


#Wirte a function "cube" that returns the third power of a number
cube <- function(x){
  x^3
}

stopifnot((cube(2) == 8) && (cube(-2) == -8))

# Write a function "power" that returns the nth power of a number x
power <- function(x,n){
  x^n
}

stopifnot((power(2,3) == 8) && (power(4,4)==256) && (power(-2,3) == cube(-2))

power_return <- function (x,n) {
  return(x^n)
}

stopifnot((power(2,3) == power_return(2,3))

# Finish arrange, select, some ggplot  
library(dplyr)
library(tidyr)
?drop_na
?complete.cases

# Get all the flights that departed with less than 120 minutes delay
# but arrived more than 120 delay
library(nycflights13)
dep_ok_arr_not <- filter(flights, dep_delay <= 120, arr_delay >120)
library(ggplot2)  
flights
# Plot histogram of dep_delay for dep_ok_arr_not
hist(dep_ok_arr_not$dep_delay)
ggplot(
  data = dep_ok_arr_not, 
  mapping = aes(x = dep_delay)
) + geom_histogram()

flights |>
  filter(dep_delay <=120)|>
  ggplot(
    #Data = ... skipped, because the pipe pipes the data into this argument
    mapping = aes(x=dep_delay)
) + geom_histogram()

# arrange
?arrange
flights
arrange(flights, dep_time)
arrange(flights, desc(dep_time), carrier)

(df <- tibble(x = c(1,NA,3)))
arrange(df,x)
arrange(df,desc(x))
arrange(df, !is.na(x))
(df2 <- tibble(x = c(3,NA,1)))
arrange(df2, is.na(x))
# Think about the next line and why it does what it does
arrange(df2, !is.na(x),x)
# In detail:
arrange(df2, is.na(x))
is.na(df2$x)
# arrange(df2, c(FALSE, TRUE, FALSE))
arrange(df2, c(0,1,0))

# select
flights
select(flights, year, month, day)
select(flights, year, day, month)
select(flights, air_time, origin, dest) |>
  arrange(air_time)
 flights |>
   select(air_time, origin, dest) |> # Could use the magrittr pipe: %>%
   arrange(air_time)

# Select all columns "year" to column "day"
 flights %>%
   select(year:day)

 flights %>%
   select(year, day, month) %>%
   select(year:day)
 
 # drop columns
flights %>%
   select(-month)
new_flights <- flights %>%
  select(-c(month,day))
  #select(-month, - day)
flights
new_flights

#select or drop multiple columns at the same tyme by some property
flights %>%
  select(starts_with("arr"))
flights %>%
  select(-starts_with("arr"))
flights %>%
  select(ends_with("hour"))
flights %>%
  select(-contains("time"))
flights
my_time_columns <- c("year","month","day","arr_time")
flights%>%
  select(all_of(my_time_columns))
# Notice, this keeps all collumns that satisfy one or the other condition
flights %>%
  select(-contains("time"), starts_with("arr")) # or condition
# To satisfy both:
flights%>%
  select(-contains("time")) %>%
  select(starts_with("arr"))
flights %>% select(-contains("time") & starts_with("arr"))
# Difference between & and &&
c(FALSE, TRUE) & c(TRUE,TRUE) # Compare two vectors index by index
c(FALSE, TRUE) && c(TRUE,TRUE) # It is just to compare two single boolians 
TRUE && TRUE
c(1) && c(2)
c(0) && c(1)

# -contains() and !contains() works the same most of times, although the intent changes sometimes
select(flights,!month)

# rename
rename(flights, destination = dest) %>%
  select(year, month, day, destination)

select(flights, day, month, year, everything()) #it orders the one we mention, the other keeps in order

# mutate
flights_small <- flights %>%
  select(
    year:day,
    ends_with("delay"),
    distance,
    air_time
  )
flights_small

mutate(
  flights_small,
  catchup = dep_delay - arr_delay,
  speed_miles = (distance/air_time)*60
) %>%
  mutate(
    speed_km = 1.61 * speed_miles
  )

# No one knows what miles is, so let's turn into km
mutate(flights_small, speed_km = (distance * 1.61 / air_time)*60)

# So 1.61 is a magic number. They are evil.
KM_PER_MILE <- 1.61
# MINS_PER_HOUR <- 60 # Some magic numbers are less evil

flights_small %>%
  mutate(
    speed_km = (distance * KM_PER_MILE/air_time)* 60
)

flights_small %>%
  mutate(
    speed_miles = (distance/air_time)*60,
    speed_km = speed_miles * KM_PER_MILE
  )

View(flights)

# transmute: creates new column and keeps only the newly created ones
flights_small %>%
  transmute(
    speed_miles = (distance / air_time) * 60
  )

# If you want to keep your changed df/tibble, you have to assign to  new variable
changed_df <- flights_small %>%
  transmute(
    speed_miles = (distance/air_time)*60,
    speed_km = speed_miles * KM_PER_MILE
  )
changed_df

# $ is a inplace 
x <- list(a=5 , b = "bla")
x
x$a <- "also bla"
x
df <- tibble(a = c(0,1))
df$a <- c(2,4)
df
