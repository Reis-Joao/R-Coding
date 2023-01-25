#Lecture 3
#Assignment 1
#Knitting last time with View
#Just comment out 'View(gapminder)'

# Follows R4DS, chapter 5

install.packages("nycflights13")
library(nycflights13)
flights
?flights
View(flights)
view(flights) #throws an error

library(tidyverse)
# Today: fliter() ; arrange() ; select()
# Next week: mutate() ; transmute() ; group_by() and summarize()
# All of these take as their first argument a tibble (a tidyverse version of dataform)

# Aside: if you want faster code, look at data.table 

# Filtering
dim(flights) #dim stands for dimension (rows / columns)
filter(flights, month == 1)
dim(filter(flights, month == 1))
mutate
flights$month #print only the column month
unique(flights$month)
flights[['month']]
jan_flights <- filter(flights, month == 1)
unique(jan_flights[['month']])
flights
dim(jan_flights)
unique(c(0,0,0,1,1,2))

# To ensure your code stops if some condition is not met
stopifnot(unique(jan_flights[['month']])==c(1))
flights[1,] # Get row number 1 (starts counting at 1!!)
flights[1,2]
flights[1, 'month']
flights[c('month','year')] # DON'T DO THIS IF YOU WANT A SINGLE COLUMN
flights['month']
flights[['month']] # use this syntax if you really want a single column as a vector
flights$month #same as before

jan_1 <- filter(flights, month ==1, day == 1)
tempflights <- flights
flights <- filter(flights, month ==1)
flights
flights <- tempflights
flights


feb1 <- filter(flights, month ==2, day ==1)
feb1
(feb1 <- filter(flights, month ==2, day ==1)) # if we put between bruckets immediatly runs the line

# Some notes on comparisons
sqrt(2)^2 == 2
sqrt(4)^2 == 4
(1/3)*3 == 1
(1/49)*49 == 1
1/(7^9)*7^9 == 1
# It is related on how numbers are stored in memory (e.g.: sqrt(2) is irrational, so it is stored unperfectly)
near(sqrt(2)^2,2)
?near

# Multiple constraints 
# '|" is "or" operator
(jan_feb <- filter(flights, month ==1 | month == 2))
#'!' is "not" operator
(not_jan <- filter(flights, !(month ==1)))
(not_jan <- filter(flights, month !=1))
(before_june <- filter(flights, month <= 6))
feb <- filter(flights, month ==2)
jan <- filter(flights, month ==1)
# To check that jan_fab is what it should be
stopifnot(nrow(jan_feb) == nrow(jan) + nrow(feb))
near(sqrt(2), 1.414, tol=0.1)
View(not_jan)

# Class exercise: get all flights that departed with less than 120 minutes delay
# but arrived with more than 120 minutes delay
# dep_delay , arr_delay
(dep_ok_arr_not <- filter(flights, dep_delay <= 120, arr_delay >= 120))
(dep_ok_arr_not <- filter(flights, (dep_delay <= 120) & (arr_delay >= 120)) # in this case works without () but with some operators precedence it is not obvious
dep_ok_arr_not_pipe <- flights |> # |> is the pipe operator in base R; %>% is pipe in tidyverse
  #the pipe takes the object on its left and puts it as the first argument to the function on the right
  filter(dep_delay <= 120) |>
  filter(arr_delay >= 120)
dep_ok_arr_not_pipe <- flights |>
  filter(
    dep_delay <= 120,
    arr_delay >= 120
  )

# arrange 
flights
arrange(flights, year, month, day)
arrange(flights, dep_delay)
arrange(flights, desc(dep_delay))

arrange(jan, desc(month))
(df <- tibble(x = c(1,NA,3)))

# NA: Not Available -> R treats it as not knowing. E.g.:
NA > 5
10 == NA
NA == NA
FALSE & NA
TRUE & NA
TRUE | NA
0 * NA # It is NA because of the following line (R allows for infinite)
0 * Inf # NaN Not a Number
NA ^ 0
0 ^ NA # Because 0 to the power of 0 is 1
0^0
0^2
