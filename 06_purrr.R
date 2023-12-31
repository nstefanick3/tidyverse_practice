# Functional programming purrr()

## map() is a function used to map a function iteratively to each element of a list or a vector

rm(list=ls())
graphics.off()

library(tidyverse)
library(hflights)


# 6.2 map: apply function 

# map()

df <- hflights %>% 
  select(ActualElapsedTime, AirTime, Distance, TaxiIn, TaxiOut)

df <- df %>% 
  as_tibble()

df %>% map(.x = ., .f = mean, na.rm = T)
df %>% map(.f = min, na.rm = T)
df %>% map(.f = max, na.rm = T)
df %>% map(.f = sd, na.rm = T)

# Ex2
# trying to extract all numerice columns from a data frame
str(mpg) 
mpg %>%
  select(displ, year, cyl, cty, hwy) # long method 
  
numeric.cols <- map(.x = mpg, .f = is.numeric) %>% 
  unlist() %>% 
  tibble(column = names(.), # put list into tibble where the first column is the names 
         numeric = .) %>%   # and the second column is the data
  filter(numeric == T) %>% 
  pull(column) # faster if you have like 100 columns

numeric.cols

numeric.cols <- map(.x = hflights, .f = is.numeric) %>%   # now using hflights
  unlist() %>% 
  tibble(column = names(.), 
         numeric = .) %>% 
  filter(numeric == T) %>% 
  pull(column) 

numeric.cols


# 6.3: map() control the output

# map_dbl() - double numeric vector
df %>% map(.x = ., .f = mean, na.rm = T) %>% class() # is a list
df %>% map(.f = min, na.rm = T)
df %>% map(.f = max, na.rm = T)
df %>% map(.f = sd, na.rm = T)

df %>% map_dbl(.x = ., .f = mean, na.rm = T)
df %>% map_dbl(.x = ., .f = mean, na.rm = T) %>% class() # is a numerice named vector
df %>% map_dbl(.f = min, na.rm = T)
df %>% map_dbl(.f = max, na.rm = T)
df %>% map_dbl(.f = sd, na.rm = T)

# create a summary table
df %>% 
  colnames() %>% 
  tibble(variable = .,
         mean = df %>% map_dbl(., mean, na.rm = T),
         sd = df %>% map_dbl(., sd, na.rm = T))

# map_int() - integer vector
list <- list(a = 1,
             b = "word",
             c = 1:10,
             d = mpg)
list

## return length of each list item
list %>% map_int(., length) # conunts colunmns for a table

# map_dfc() - data frame column bind

## summaries
df %>% map_dfc(., mean, na.rm = T)


# 6.4 map shortcuts

# fit multiple regression lines to the cars data set for multiple cylendars

## prepare data
df.mpg <- mpg %>% 
  select(hwy, displ, cyl) %>% 
  mutate(cyl = as.factor(cyl))

## fit model - map() no shortcuts
models <- df.mpg %>% 
  split(.$cyl) %>% # split data by different number of cylinders
  map(function(df) lm(formula = hwy ~ displ, 
                      data = df)) # estimates linear model for each different type cly

## fit model - map() shorter syntax
models <- df.mpg %>% 
  split(.$cyl) %>% # split data by different number of cylinders
  map(~ lm(hwy ~ displ, data = .)) # estimates linear model for each different type cly
  # use dot as the data has to be taken from the left side of the pipe

models

# now extract R squared for each model

## longer syntax
models %>% 
  map(summary) %>% # goes through list and maps the summary
  map_dbl(~.$"r.squared") # grab the r-squared value from all summaries

## shorter syntax
models %>% 
  map(summary) %>%
  map_dbl("r.squared") # removed data pull from left before "r-squared"

# shortcuts for extracting elements by postion

## list
list <- list(list(1:3,4:6,7:9),
             list(10:12,13:15,16:18),
             list(19:21,22:24,25:27))
list

## extract third element from each sub-list and put the extraction inside of a list
list %>% map(3)







