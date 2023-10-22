# 3 Data Wrangle: strings - factors (stringr & forcats)

rm(list = ls())
graphics.off()

# Install packages and load package
install.packages("stringr")
install.packages("forcats")     

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(stringr)
library(forcats)


# 3.1 Strings inside tidyverse

# Create a string
s1 <- "Double quotes string"
s1

s2 <- 'Single quotes string'
s2

s3 <- "Double quotes outside, 'single quotes inside' a string"
s3

s4 <- 'Single quotes outside, "double quotes inside" a string'
s4

# Be careful of mixing quotes (hit ESC when R is stuck - code below!)
# Not possible!
s5 <- "Double quotes inside "double" quotes"
s6 <- 'Single quotes inside 'single' quotes'
# s7 <-  "not "working" 
# s8 <-  'not 'working'
# s9 <- "Missing closing quote


# Create a vector of strings
vec <- c("a", "b", "c")
vec  


# Character vector inside a tibble
df <- tibble(letters = vec)
df  


# Special characters
# How to escape a character ~ Regular expressions

#   Literal single or double quotes
"\""   # escape a special charatcer with back slash - \
'\''

#   Some other special characters (will be seen later at regular expressions)

#   New line
"\n"

#   Tabulator
"\t"

#   Unicode non-english characters
"\u03B1"

# See raw content of the string (omiited escape characters and outside quotes)
s <- "string"
s
writeLines(s)

s <- "\""
s
writeLines(s)

s <- "line 1 \nline 2"
s
writeLines(s)

rm(list =ls())

# 3.2 Strings matching

# load strings dataset
load("./data/strings.RData")
ls()

# str_detect() - Detect s pattern
# similar base R: grepl()

#   Find a fruit containing letter "a" (anywhere in word)
fruit
ind <- str_detect(string = fruit, pattern = "a") # returns TRUE / FALSE
fruit[ind]

fruit[grepl(pattern = "a", x = fruit)]

#   Find a fruit not containing any letter "a" !
#   we use negation
fruit[str_detect(fruit, "a", negate = T)]
fruit[!str_detect(fruit, "a")]

#   Inside tibble add flag if fruit contains letter "a" or if it doesn't contain letter "a"
fruit.df %>% 
  mutate(flag = case_when(str_detect(fruit, pattern = "a") ~ "contains 'a'",
                          T ~ "does not contain 'a'"))

#   Find fruits starting or ending with letter "a"
ind.start.a <- str_detect(fruit, pattern = "^a")
fruit[ind.start.a]

ind.end.a <- str_detect(fruit, pattern = "a$")
fruit[ind.end.a]


# str_which() - Detect s pattern return index position

# similar base R: grep()
#   Find a fruit containing letter "a" (anywhere in word)
ind <- str_which(string = fruit, pattern = "a") # returns index position
fruit[ind]

fruit[grep(pattern = "a", x = fruit)]


# str_count() - Count number of pattern matches in string

#   Add count of letter "a" in each fruit (use table)
fruit.df1 <- fruit.df %>% 
  mutate(`count a` = str_count(fruit, pattern = "a"))

#   Show counts of letter "a" in fruits
fruit.df1 %>% 
  count(`count a`)

#   Show fruit with 3 "a" letters
fruit.df1 %>% 
  filter(`count a` == 3)


# str_locate() / str_locate_all() - Locate position(s) of pattern match in string

#   Locate position of first letter "a" in each fruit (matrix is returned)
str_locate(fruit, pattern = "a") 

fruit.df1 <- str_locate(fruit, pattern = "a") %>% 
  as_tibble() %>% # convert matrix of positions to tibble
  mutate(fruit = fruit) %>% # add fruit name column
  select(fruit, start, end) # re-arrange columns

#   Locate position of all letters "a" in each fruit (list is returned)
str_locate_all(fruit, pattern = "a") 
