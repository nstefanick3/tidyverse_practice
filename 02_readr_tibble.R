tibble(mpg)
mpg
mpg[1] # first col as tibble
mpg[1,] #first row as tibble
mpg[,1] #first col as tibble
mpg[[1]] #first col as vector
mpg[1,1] #tibble of first row first col
mpg[[1,1]] #first row and first col as vector
mpg["hwy"]
mpg[["hwy"]]

# data import

rm(list = ls())
#install.packages("tibble")

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)

# 2.1 Table - Tibble
ggplot2::diamonds
class(ggplot2::diamonds)

ggplot2::economics

#hflights data set
hflights::hflights
class(hflights::hflights)
as_tibble(hflights::hflights)


# 2.2 Create tibble

# as_tibble()
## convert hflights data frame to tibble
as_tibble(hflights::hflights)

# convert a custom df
df <- data.frame(x = 1:10,
                 y = seq.Date(from = as.Date("2021-01-01"),
                              to = as.Date("2021-01-10"),
                              by = "day"))
df
as_tibble(df)


tibble(v1 = seq(from = 1, to = 100, by = 1),
       v2 = pi,
       v3 = sqrt(1),
       v4 = seq.Date(from = as.Date("2021-01-01"),
                     length.out = 100,
                     by = "day"))

tribble(
  ~name, ~surname, ~male, ~age,
  "Max", "Smith",   T,      35,
  "Lily", "Brown",  F,      27
)

#2.3 DF vs. Tibble
mpg %>% .$model #pipe get vector
mpg %>% .[["model"]]
mpg
mpg %>% .["model":"year"] ##error
mpg %>% select(model:year)

library(tidyverse)


# import files using readr
#install.packages("readr")
library(readr)

# read inline csv file
read_csv("c1,c2,c3
          1,a,T
          2,b,F
          3,c,T") ##csvs have no comma to denote a newline at the end, there are no spaces between

# inline files withe meta header lines
read_csv("First meta line  
         Second meta line
         c1,c2,c3
          1,a,T
          2,b,F
          3,c,T", skip = 2) # also do not start first line with newline

# inline files with comments
read_csv("c1,c2,c3  #comment
          1,a,T     #comment
          2,b,F
          3,c,T", comment = "#")

#read comma separated files .csv from computer files
getwd()
list.files(path = "./data/02_05_read_files")

# small mpg table 
df <- read_csv(file = "./data/02_05_read_files/mpg_mini.csv")

# small mpg table - column is different, separator is ";"
df <- read_csv2(file = "./data/02_05_read_files/mpg_mini2.csv")

# read tab delimeted file - .tsv
df <- read_tsv(file = "./data/02_05_read_files/mpg.tsv")

# read files with selected delimiter / custom delimiter in this case ~
df <- read_delim(file = "./data/02_05_read_files/mpg_delim.txt",
                 delim = "~")

# read txt files
df <- read_delim(file = "./data/02_05_read_files/mpg.txt",
                 delim = " ",
                 col_names = T,           ##aka first row as headers
                 skip = 3,
                 skip_empty_rows = T,
                 quote = "\"")            ##single character used to quote strings

#read log file
read_log(file = "./data/02_05_read_files/example.log")

#read a large csv file and check execution time 
system.time(
  df <- read.csv(file = "./data/02_05_read_files/mpg_maxi.csv")
)

system.time(
  df <- read_csv(file = "./data/02_05_read_files/mpg_maxi.csv")
)

# 2.6 vector parsing theses tools are used to in data cleaning to look for exceptions in the data

# parse a charachter vector
parse_character(c("one", "two", "3"))


#parse logical
g <- parse_logical(c("TRUE", "TRUE", "FALSE", "F", "NA", "f"))

#problems with parsing
x <- parse_logical(c("TRUE", "TRUE", "FALSE", "F", "NA", "string"))
problems(x)

#parse integer
parse_integer(c("10", "15", "20", "1", "NA"))

#parse factor
parse_factor(c("a", "a", "b"), levels = c("b", "a")) ##levels are ordered

#parse double
parse_double(c("11.2", "27.8"))

parse_double(c("11,2", "27,8"),
             locale = locale(decimal_mark = ","))

#parse number
parse_number(c("1", "2.2", "$10", "5%", "1,100"))

#specify grouping marks
parse_number(c("100,000.3"), locale = locale(grouping_mark = ","))

#parse date
parse_date("2021-01-31") %>% class()

#specify date format
parse_date("20210131", format = "%Y%m%d")
parse_date("2021/31/01", format = "%Y/%d/%m") #notice r returns always like YYYY-MM-DD

#parse_time
parse_time("4:01 pm")
parse_time("00:01 am")
parse_time("00:01:35") # last one is seconds

parse_datetime("2021-01-31 00:01:12")

##File Parsing

# guess parser heuristic
guess_parser(c("T", "F", "f"))
guess_parser(c("T", "F", "f", "great"))

#parse each column in mpg table
read_tsv(file = "./data/02_05_read_files/mpg.tsv",
         col_types = cols(
           manufacturer = col_factor(),
           model = col_factor(),
           displ = col_double(),
           year = col_integer(),
           cyl = col_integer(),
           trans = col_character(),
           drv = col_factor(),
           cty = col_double(),
           hwy = col_double(),
           fl = col_factor(),
           class = col_factor()
         ))

# import table
# do not specify column types at import
# change column type inside R
read_tsv(file = "./data/02_05_read_files/mpg.tsv") %>% 
  mutate_at(.vars = c("year", "cyl"), .funs = as.numeric) %>% 
  mutate_at(.vars = c("manufacturer", "model"), .funs = as.factor) %>% 
  mutate(across(c(cty,hwy))*10)

#other inport export files
##readxl = part of tidyverse
##rio = automated excel reading
##haven = part of tidyverse, a bit older, supports imports from other stats softwares like sas stata SPSS
###normally now you get format from json, csv, flat files, databases
##Raw (fast import) data.table package with fread function
##DBI to get data from database
##jsonlite for json files aka grabbing data from APIs
##xml2 to grab data in xml format

#2.8 other useful libraries
library(readxl)
library(rio)
library(data.table)

#import excel

##read excel
read_excel(path = "./data/02_08_other_useful_import_libraries/mpg.xlsx") %>% class()
read_excel(path = "./data/02_08_other_useful_import_libraries/mpg.xlsx", sheet = "Sheet 1")
read_excel(path = "./data/02_08_other_useful_import_libraries/mpg.xlsx", range = "A1:C10")

#rio
rio::import(file = "./data/02_08_other_useful_import_libraries/mpg.xlsx")
rio::import(file = "./data/02_08_other_useful_import_libraries/mpg.xlsx", sheet = "Sheet 1")

#Import larger files with fread
df.f <- fread(file = "./data/02_05_read_files/mpg_maxi.csv", sep = ",")

system.time(
  df1 <- read.csv(file = "./data/02_05_read_files/mpg_maxi.csv")
)

system.time(
  df2 <- fread(file = "./data/02_05_read_files/mpg_maxi.csv", sep = ",") ###fastest
)

system.time(
  df3 <- read_csv(file = "./data/02_05_read_files/mpg_maxi.csv")
)

#2.9 write files

##comma delimited
write_csv(x = mpg,
          file = "./data/mpg_w.csv",
          col_names = T)

##semicolon separated
write_csv2(x = mpg,
           file = "./data/mpg_w2.csv",
           col_names = T)

#write excel file
rio::export(x = mpg, 
            file = "./data/mpg_w3.csv")

#write to r data table type
write_rds(x= mpg, 
          file = "./data/mpg.rds")
read_rds(file = "./data/mpg.rds")

# write/ read feather file 
#install.packages("feather")
library(feather)

write_feather(x = mpg,
              path = "./data/mpg.feather")
read_feather(path = "./data/mpg.feather")





