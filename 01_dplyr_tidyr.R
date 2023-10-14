# 1 tidyverse essentials:

rm(list = ls())
ls()
graphics.off() #remove all graphics

#install packages
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyr")

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#inspect data 
help("mpg")
df <- mpg
View(df)
print(df)
str(df)
nrow(df); ncol(df)

#manipulate variables (cols)
#select() - columns selection

## extract: manufacturer, model, year
select(df, manufacturer, model, year)
df.car.info <- select(df, manufacturer, model, year)

## columns that begin wtih letter: 'm'
select(df, starts_with(match = "m"))

##columns that contain letter r
select(df, contains(match = "r"))

## columns that end with y
select(df, ends_with(match = "y"))

## select columns by column index or position
select(df, 1:3)
select(df, c(3,5,7))
select(df, (ncol(df)-2):ncol(df)) # last 3 columns
select(df, (ncol(df)-2):ncol(df))

#rename() - rename columns 

## rename "manufacturer" and "model"
df1 <-rename(df,
             mnfc = manufacturer,
             mod = model)
colnames(df1)

## select and rename columns in one call
df1 <- select(df, mnfc = manufacturer, mod = model,
       everything()) ##everything grabs the rest of the dataframe


# mutate() - create a new variable

# dplyr::mutate()

## create variable: average between highway and city miles per gallon
df <- mutate(df, 
             `avg miles per gallon` = (cty + hwy)/ 2)

## "car", "cyl" / "trans"
df <-  mutate(df, 
              car = paste(manufacturer, model, sep =" "),
              `cyl / trans` = paste(cyl , ' / ', trans))

paste0(1:12)
paste(1:12)        # same
as.character(1:12)

(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
nth
letters
LETTERS

mutate(df, `avg mph` = (cty+hwy)/2)

# transmute() - create new variable and drop other variables
(df1 <- transmute(df, 
          `avg miles per gallon` = (cty+hwy)/2))

df2 <- transmute(df, 
          car = paste(manufacturer, model, sep =" "),
          `cyl / trans` = paste(cyl , ' / ', trans))

#filter vs slice: filter is by condition, slice is by index number

#reset data
rm(df1, df2)

#manipulate cases (rows)

#filter() - filter rows by condition
df <- mpg

##filter where manufacture "audi"
filter(df, manufacturer == "audi")

##filter where manufacture "audi"
filter(df, manufacturer == "audi" & year == 1999)

## where manufacturer is either "audi" or "dodge"
filter(df, manufacturer == "audi" | manufacturer == "dodge")

## hwy is greater or equal than 30
filter(df, hwy >= 30)

## where year is not equal 1999
filter(df, year != 1999)

# slice() - extract rows by postion

## extract first 5 rows 
slice(df, 1:5)
slice(df, 20:30)
slice(df, (nrow(df)-9):nrow(df))
slice(df, c(1,3,5))


# arrange is the equivalent of order by in sql
df %>% 
  slice(1:5) %>% 
  arrange(desc(year), desc(cyl))

slice(df, 1:200)

#distinct() - unique rows
df2 <- distinct(df)

## our small example
df.example <- tibble(id = 1:3,
                         name = c("John", "Max", "Julia"))

df.example <- bind_rows(df.example, slice(df.example,2))
df.example <- arrange(df.example, id)

#remove duplicate row
df.example <- distinct(df.example, slice(df.example, 2))
df.example

## back to mpg example - lets create table with duplicates
(df.dupl <- select(df, manufacturer, model))
df.noduple <- distinct(df.dupl)

# Sample rows
set.seed(123)
sample_n(df, 4, replace = F)
sample_frac(df,.25)

#summmarise - apply summary functions to our table to make summaries
summarise(df, 
          `mean hwy consumption` = mean(hwy),
          rows = n(),
          `nr models` = n_distinct(model),
          `min hwy` = min(hwy),
          `min cty` = min(cty),
          `max hwy` = max(cty),
          `max cty` = max(cty))
 

summarise(df.example, `count of rows` = n())
#group_by() - group cases using one or more grouping variables

##group by manufacturer
df
group_by(df, manufacturer)

#combine summarise and group_by
# count the number of cars per manufacturer
summarise(group_by(df, manufacturer),
  `count per manufacturer` = n(),
  `avg cty` = mean(cty),
  `min hwy` = min(hwy),
  `min cty` = min(cty),
  `max hwy` = max(hwy),
  `max cty` = max(cty)
)

nrow(df)
ncol(df)
## count number of rows/cars per model

count(df, model)

summarise(group_by(df, manufacturer,model),
          `count` = n(),
          `avg cty` = mean(cty))

df %>% 
  group_by(manufacturer, model) %>% 
  summarise(`count` = n(),
            `avg cty` = mean(cty))

expand(df, nesting(manufacturer, model))

df %>% 
  dplyr::filter(class == 'compact') %>% 
  dplyr::select(manufacturer, model, class, cty, hwy) %>%
  dplyr::mutate(avg_mpg = (cty+hwy)/2) %>% 
  dplyr::group_by(manufacturer, model) %>% 
  dplyr::summarise(avg_mpg_per_model = mean(avg_mpg)) %>% 
  dplyr::arrange(avg_mpg_per_model) %>% 
  dplyr::filter(grepl("^a",model))

df %>%
  filter(manufacturer == "audi" | manufacturer == "dodge") %>% 
  select(manufacturer, model, year, class)

#>calculate average hwy and count cars for each manufacturer 
#>model class and transmission type also filter results where
#>average hwy is is greater thant 30 and show results based on 
#>decending order based on avg hwy
#

df %>% 
  group_by(manufacturer, model, class, trans) %>% 
  summarise(count_cars = n(),
            avg_hwy = mean(hwy)) %>% 
  filter(avg_hwy > 30) %>% 
  arrange(desc(avg_hwy))
  
# pivoting converting long to wide and vice versa

##lets create a simple table in long format
(table.long <- data.frame(id = 1:6,
                         id2 = 11:16,
                         type = c("a" , "b", "a", "c", "c", "a"),
                         count = c(20,50,45,15,12,5)))
table.long

# pivot_wider - converts long data to wide data - each type is written in its own column
table.wide <- pivot_wider(table.long, 
                          names_from = type,
                          values_from = count)
table.wide


#pivot_longer() - convert wide data to long data

##convert table back to long format
table.long1 <- pivot_longer(table.wide, 
                            cols= c("a", "b", "c"),
                            names_to = "type",
                            values_to = "count",
                            values_drop_na = F) 
table.long1  

#now lets pivot our car dataset

#> filter rows where manufacturer is jeep, land rover or hyundai
#> select model, trans., hwy
#> calculate avg. hwy for each model and trans
#> this will be long table format
df.long <- mpg %>% 
  filter(manufacturer %in% c("jeep", "land rover", "hyundai")) %>% 
  select(model, trans, hwy) %>% 
  group_by(model, trans) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  ungroup()
df.long

#convert long to wide format - where transmission type is transformed into columns
df.wide <- df.long %>% 
  pivot_wider(names_from = trans,
              values_from = mean_hwy)
df.wide

##convert back to long format
df.long <- df.wide %>% 
  pivot_longer(-model,
               names_to = "trans",
               values_to = "mean_hwy",
               values_drop_na = T)
df.long

# separating and uniting columns

## let's create a table with date column (generate date for 1 year)
dates <- seq.Date(from = as.Date("2021-01-01"), 
                  to = as.Date("2021-12-31"), 
                  by = "day")
table <- data.frame(date = dates)
table %>% head()
table %>% tail()

#separate() - split one column into multiple columns

##split date into year, month and day of month
##remove leading zeros where necessary
##sort columns 
table.sep <- table %>% 
  #filter(date == "2021-11-1") %>% 
  separate(., 
           col = date,
           into = c("year", "month", "day"),
           sep = "-") %>% 
#  mutate(Month = as.numeric(Month),
#         Day = as.numeric(Day))
           mutate_at(.tbl = .,                       #which table
                     .vars = c("month", "day"),      #which variables
                     .funs = as.numeric) %>%         #which function is applied
           arrange(year, month, day)

table.sep
str(table.sep)

tempdir()
dir.create(tempdir())

#unite() - combine multiple columns into one column
## we add leading zeros for month and day of month
## so we will use library stringr (from tidyverse)
library(stringr)

## create one date column by merging:
## - year, month, dayofmonth
## - add leading zeros (stringr)
table.unite <- table.sep %>% 
  #add leading zeros
  #mutate(month = str_pad(month, width = 2, side = "left", pad = "0"),
  #       day   = str_pad(day  , width = 2, side = "left", pad = "0")) %>% 
  mutate_at(.tbl = ., .vars = c("month", "day"), .funs = str_pad, 2, "left", "0") %>%  #turns numeric month and day into strings with 0s in front 
  unite(data = .,
        col = "date",
        year, month, day,
        sep = "-") %>% 
  arrange(date)

table.unite


#############################################################
#session 2
#############################################################
rm(list = ls())

library(hflights)
hflights::hflights
str(hflights)


#dplyr and tidyr in action 
#pull() extract a column as a vector
df <- mpg

#class() data type of object

df %>% pull(hwy) %>% class()
df %>% select(hwy) %>% class()

# group_by() + mutate

## calculate avg hwy per car manufacturer and car model
df1 <- df %>% 
  group_by(manufacturer, model) %>% 
  mutate(mean_hwy = mean(hwy))

df2 <- df %>% 
  group_by(manufacturer, model) %>% 
  mutate(mean_hwy = mean(hwy)) %>% 
  ungroup() #ungroup get rid of the grouping attribute on the table

#case when

##add  variable called transmission type "automatic" or "manual"
df %>%  count(trans)
df <- df %>% 
  mutate(trans_ = str_sub(string = trans, 
                          start = 1, 
                          end = 1)) %>% 
  mutate(`transmission type` = case_when(trans_ == "a" ~ "automatic",
                                         trans_ == "m" ~ "manual",
                                         TRUE ~ "NA")
         ) %>% 
  select(-trans_)

df %>% count(`transmission type`, trans)

#row_number() - ranks

## add a car rank / id not considering groups
df <- df %>% 
  mutate(`car id` = row_number())

df
##add car id // considering groups
df <- df %>% 
  group_by(manufacturer) %>% 
  mutate(car_id_1 = row_number()) %>% 
  ungroup()

rm(list = ls())

# transform flights data

df <- hflights

nrow(df);ncol(df)

df %>% count(UniqueCarrier, FlightNum, TailNum, Year, Month, DayofMonth) #count grouped by the variables in count

##how many columns begin with the word Taxi

df %>% select(starts_with("Taxi"))

## how many flights were flown with less than 1000 miles vs greater than or equal to 1000
df %>% mutate(miles1000 = case_when(Distance < 1000 ~ "Less than 1000",
                                    Distance >= 1000 ~ "Greater or equal to 1000",
                                    TRUE ~ "NA")) %>% 
  count(miles1000)

## how many flights per carrier - sort by top to bottom
df %>% group_by(UniqueCarrier) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  ungroup()

## number of cancelled flights per each carrier
str(df)
df %>% group_by(UniqueCarrier) %>% 
  summarise(total_cancelled_flights = sum(Cancelled)) %>% 
  ungroup() %>% 
  arrange(desc(total_cancelled_flights))

# or 
df %>% 
  filter(Cancelled == 1) %>% 
  group_by(UniqueCarrier) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

#now find the percentage of cancelled flights per carrier
#sum(df$Cancelled)
#percentage of flights cancelled against each other
df %>% 
  filter(Cancelled == 1) %>% 
  group_by(UniqueCarrier) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(percent_cancelled = n/sum(df$Cancelled))

# percentage of flights cancelled against total flights
str(df)
df %>% group_by(UniqueCarrier) %>% 
  summarise(total_cancelled_flights = sum(Cancelled),
            total_flights = n(),
            percent_cancelled = (sum(Cancelled)/n())*100) %>% 
  ungroup() %>% 
  arrange(desc(total_cancelled_flights)) %>% 
  arrange(desc(percent_cancelled))

# create column date by combining year month and day of month and remove those 3 columns
df %>% mutate_at(., c("Year","Month","DayofMonth"), .funs = str_pad, 2, "left", "0") %>% 
  unite("Date",Year:DayofMonth, sep= "-")

# Count flights per cancelled codes (codes in columns)
## and per carriers (in rows)

df %>% select(UniqueCarrier, Cancelled, CancellationCode) %>% 
  mutate(CancellationCode = str_replace(CancellationCode,"^$", "None")) %>%
  #filter(CancellationCode != "") %>% 
  #mutate_at('CancellationCode', ~na_if(.,'')) %>% 
  pivot_wider(names_from = CancellationCode, values_from = Cancelled, values_fn = sum) ## bad, does not count nulls correct

# Professors answer
df %>% 
  mutate(CancellationCode = case_when(CancellationCode == '' ~ '0',
                                      TRUE ~ CancellationCode)) %>% 
  group_by(UniqueCarrier, CancellationCode) %>% 
  count() %>% 
  ungroup() %>% 
  pivot_wider(names_from = CancellationCode, values_from = n, values_fill = 0)




