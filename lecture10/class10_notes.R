# Lecture 10

library(tidyverse)
library(nycflights13)

# Joins
View(planes)

# tailnum is a primary key
planes %>%
  count(tailnum) %>%
  filter(n > 1)

# What is the pkey for flights? Is it flight by day
flights %>%
  count(year, month, day, flight) %>%
  filter(n>1)

(flights2 <- flights %>%
    mutate(pkey = row_number()) %>%
    select (pkey, everything()))

colnames(airlines)
airlines
colnames(flights2)
flights %>% select(carrier)

flights2 <- flights2 %>%
  select(pkey, year:day, hour, origin, dest, carrier, tailnum)

flights2 %>% slice(2)
flights2 %>% 
  left_join(airlines, by = "carrier")

# Recommendation: work through 13.4.1 in R4DS

x <- tibble(
  name= c("m","lm","wm"),
  height = c(1.19, 1.80, 0.00)
)

y <- tibble(
  name = c("m","m","lm","bm"),
  class = c("R", "I", "LR", "NC")
)

x
y

left_join(x,y)
right_join(y,x)
right_join(x,y) 
inner_join(x,y) # demand joining both the left and right
full_join(x,y) # merge everything on both sides

?merge

View(weather)
View(flights)

unique(weather$origin)

flights2 %>%
  left_join(weather)

# If you wanted to match on weather at destination and if weather had data on non-NYC airports, you'd do this:
flights2 %>%
  left_join(weather, by = c("year", "month", "day", "hour", "dest"="origin"))

# 13.5 Filtering Join

# Find the top 10 most popular destination

(top10 <- flights %>%
    count(dest) %>%
    arrange(desc(n))%>%
    slice(1:10))

(top10 <- flights %>%
    count(dest)%>%
    filter(min_rank(desc(n)) < 11))

#or 

(top10 <- flights %>%
    count(dest)%>%
    mutate( r = min_rank(desc(n))) %>%
    filter(r < 11))

# All the flights to the top destinations
flights %>%
  filter(dest %in% top10$dest)

# Match without changing the dataset. good way of checking if two datasets have matching. Also for some filtering.
flights %>%
  semi_join(top10)

# get the top 11 destinations
(ranked_dest <- flights %>%
    count(dest) %>%
    mutate(r= min_rank(desc(n))) %>%
    arrange(r))

flights %>%
  semi_join(ranked_dest %>% filter(r <= 11))

flights %>%
  filter(dest %in% ranked_dest$dest[1:11])

flights %>%
  anti_join(planes, by = "tailnum")

# Three steps to make sure that joining works:
# 1. What is the primary key?
# 2. Is the primary key present for all rows?
# 3. Does the pkey exist in both tables, and does every observation match?
#   - Use "anti_join" (vantage over semi_join is that it shows the problematic rows)

# setdiff, intersect(), union()
df1 <- slice(flights2, 1:20)
df2 <- slice(flights2, 10:30)
df1
df2
setdiff(df1, df2)
setdiff(df2, df1)
union(df1,df2) # Does not repeat rows that are in both, different from stacking two datasets
intersect(df1, df2)
rbind(df1, df2) # It appends/stacks two datasets, and it is computational faster than union

map(
  list(df2,df3,df4,df5,...),
  function (df) (semi_join(df,df1))
) %>%
  reduce(rbind)