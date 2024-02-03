# Section 7: dplyr relational data

rm(list = ls())
graphics.off()

# library
library(tidyverse)
#install.packages("nycflights13")
library(nycflights13)

# 7.2 Example database

# tables

View(airlines)
View(airports)
View(planes)
View(weather)
View(flights)


# 7.3 Mutating Joins

# simple table
table_x <- tribble(~ key, ~ val,
                   1,    "a1",
                   2,    "a2",
                   3,    "a3")

table_y <- tribble(~ key, ~ val,
                   1,    "b1",
                   2,    "b2",
                   4,    "b4")

# inner join
inner_join(x = table_x,
           y = table_y,
           by = "key")

inner_join(x = table_x,
           y = table_y,
           by = "key",
           suffix = c("_tabx", "_taby"))

airlines
flights

flights %>% colnames()

df <- flights %>% 
  inner_join(x = .,
             y = airlines,
             by = c("carrier" = "carrier")) %>% 
  rename(carrier_name = name)

df %>% count(carrier_name) %>% 
  arrange(desc(n))

# Left Join
left_join(x = table_x,
           y = table_y,
           by = "key")

##fix year col in planes table, rename to avoid confusion
df.planes <- planes %>% rename(year_plane = year)

## we will add data in a pipe (multiple left joins)
df.all <- flights %>% 
  left_join(x = .,
            y = airlines,
            by = "carrier") %>% 
  rename(carrier_name = name) %>% 
  left_join(x = .,
            y = airports,
            by = c("dest" = "faa")) %>% 
  rename(dest_name = name) %>% 
  left_join(x = .,
            y = df.planes,
            by = c("tailnum" = "tailnum")) %>% 
  left_join(x = .,
            y = weather,
            by = c("year" = "year",
                   "month" = "month",
                   "day" = "day",
                   "hour" = "hour",
                   "origin" = "origin"))

# right join
right_join(x = table_x,
          y = table_y,
          by = "key")

## flights counts
df.flights.counts <- flights %>% count(tailnum)

## bring flights counts to planes table 
df.planes <- df.flights.counts %>% 
  right_join(x = .,
             y = planes,
             by = "tailnum") %>% 
  rename(`number of flights` = n)

## full join
full_join(x = table_x,
           y = table_y,
           by = "key")

## select only relevent columns 
df.dest <- flights %>% 
  select(carrier, dest)

## do the full join
df.carrier_dest <- airlines %>% 
  full_join(x = .,
            y = df.dest,
            by = "carrier")

## do the check 
df.carrier_dest %>% 
  filter(is.na(dest))

df.carrier_dest %>% 
  filter(is.na(carrier))

# 7.4 Filtering Joins

# Semi Join
semi_join(x = table_x,
          y = table_y,
          by = "key")

airlines1 <- airlines %>% 
  filter(carrier %in% c("AA", "VX", "DL"))

semi_join(x = airlines1,
          y = flights,
          by = "carrier")

semi_join(x = flights,
          y = airlines1,
          by = "carrier")

# anti join 
anti_join(x = table_x,
          y = tably_y,
          by = "key")

#below is empty because the tibble flights contains the carriers in the airlines1 table
anti_join(x = airlines1,
          y = flights,
          by = "carrier") 

anti_join(x = flights,
          y = airlines1,
          by = "carrier")

# 7.5 set operations 

# create two simple tables
airlines1 <- airlines %>% slice(c(1,3,5,7,9))
airlines2 <- airlines %>% slice(c(2,4,5,8,9))

bind_cols(airlines1, airlines2)

bind_rows(airlines1, airlines2)

intersect(airlines1, airlines2)

setdiff(airlines1, airlines2)

union(airlines1, airlines2)


# 7.6 dplyr additional functions

df <-  flights %>% 
  filter(carrier == "AA") %>% 
  arrange(time_hour)


## check1
df <- df %>% 
  mutate(`origin prev flight` = lag(x = origin, n = 1)) %>% 
  mutate(`origin test` = case_when(origin == `origin prev flight` ~ TRUE,
                                   T ~ FALSE))

df %>% filter(`origin test`) %>% count()


## check2 
df <- df %>% 
  mutate(`distance successive flights` = distance + lead(x = distance, n = 1)) %>% 
  mutate(`distance test` = case_when(`distance successive flights` >= 2000 ~ TRUE,
                                     T ~ FALSE))

df %>% filter(`distance test`) %>% count()

## check3 
df <- df %>% 
  mutate(`distance running tot` = cumsum(distance))

df %>% 
  mutate(`flight id` = row_number()) %>% 
  filter(`distance running tot` >= 1000000) %>% 
  select(`flight id`, everything()) %>% 
  head(1) %>% 
  as.data.frame()

## rank flights
df <- df %>% 
  mutate(`rank flight` = dense_rank(distance))
