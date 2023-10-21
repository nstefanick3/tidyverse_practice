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





