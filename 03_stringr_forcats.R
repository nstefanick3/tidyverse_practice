#install.packages("tidyverse")
library(tidyverse)

str_length(c("pineapple", "pear", "apple", "banana"))

length("a'a'b")
length(c("a","b"))

rm(list = ls())
graphics.off()

#####################################################

# 3.1 strings inside tidyverse

s1 <- "string"
s2 <- 'string'
s3 <- 'st"ri"ng' #enters escape character
s4 <- "st'ri'ng" # does not input escape character

vec <- c("a", "b", "c")
vec

#charachter vector inside tibble
df <-  tibble(lett3rs = vec)
df

#special characters 
##escape characters

"\n" #newline
"\t" #tab
"\u03B1" # alpha

#see raw content of your string
s <- "string"
s
writeLines(s)

s <- "\""
writeLines(s)

fruit <- c("pineapple", "pear", "apple", "banana")
str_detect(fruit, "e")
str_which(fruit, "e") # list of locations in vector where string exists 
str_count(fruit, "e")
str_locate(fruit,"e")
str_locate_all(fruit,"e")


########################################################################

load("./data/strings/strings.Rdata") #load .Rdata from file saved in project
ls() # lists data in environment

# str_detect() - detect a pattern
# similar to base R: grepl()

## find a fruit containing letter "a" anywhere
fruit
sbset <- str_detect(fruit, "a")
fruit[sbset]

sbset <- str_which(fruit, "a")
fruit[sbset]

#old way 
grepl(pattern = "a", x = fruit)
fruit[grepl(pattern = "a", x = fruit)]

#find a fruit not containing letter a
sbset <- str_detect(fruit, "a")
fruit[!sbset]

#or
str_detect(fruit, "a", negate = T)

# in tibble detect and throw flag if fruit contains letter a
fruit.df %>% 
  mutate(flag = case_when(str_detect(fruit, "a") ~ "Contains a",
                          T ~ "Does not contain a"))

# str_which() - detects a pattern and returns index position
# base r: grep()
str_which(string = fruit, pattern = "a")

# str_count() - counts number of times pattern appears
str_count(fruit, "a")

fruit.df1 <- fruit.df %>% 
  mutate(count_a = str_count(fruit, "a")) %>% 
  arrange(desc(count_a))

#show me all fruits containing letter a 3 times
fruit.df1 <- fruit.df %>% 
  mutate(count_a = str_count(fruit, "a")) %>% 
  arrange(desc(count_a)) %>% 
  filter(count_a >= 3)

#str_locate() and str_locate_all()
#locate place of first postion of a
str_locate(fruit,"a")

fruit.df1 <- str_locate(fruit, "a") %>% 
  as_tibble() %>% 
  mutate(fruit = fruit) %>% 
  select(fruit, start, end)

#location of all postions of a's
str_locate_all(fruit, "a")

#moved develop further along

#moved develop further along again

#trying to push to remote develop through r



