#HW2_readr_tibble

##Excercise 1
library(tidyverse)
continents <- tribble(
  ~`Date (data published)`, ~Continent,  ~`Area (km2)`,  ~`Percent total landmass`,  ~`Population`, ~`Percent total pop.`, # header
  #----------------------------------------------------------------------------------------------------------------------#
  "2017-11-10",              "Africa",        30370000,   20.4,                       1287920000,    16.9,
  "2017-11-10",              "Antarctica",    14000000,    9.2,                             4490,     0.1,
  "2017-11-10",              "Asia",          44579000,   29.5,                       4545133000,    59.5,
  "2017-11-10",              "Europe",        10180000,    6.8,                        742648000,     9.7,
  "2017-11-10",              "North America", 24709000,   16.5,                        587615000,     7.7,
  "2017-11-10",              "South America", 17840000,   12.0,                        428240000,     5.6,
  "2017-11-10",              "Australia",      8600000,    5.9,                         41264000,     0.5
)

continents

continents %>% summarise(total_area = sum(`Area (km2)`),
                         total_population = sum(Population),
                         sum_of_percentage_landmass = sum(`Percent total landmass`),
                         sum_of_percentage_pop = sum(`Percent total pop.`))

##Exercise 2
df2 <- read_csv(file = "./data/data_import/data_import/flights_02.csv",
                col_names = T,
                col_types = cols(
                  UniqueCarrier = col_character(),
                  FlightNum = col_double(),
                  Year = col_double(),
                  Month = col_double(),
                  DayofMonth = col_double(),
                  Origin = col_character(),
                  Dest = col_character(),
                  Distance = col_double()
                ))
problems(df2)
str(df2)


##Exercise 3 
#'import flights_03.csv to and object df3
#'for import must use additional import strategies
#'parse all columns as characters on initial import
#'change individual column types after import

df3_names <- read_csv(file = str_c(getwd(), "/data/data_import/data_import/flights_03.csv"),
                      skip = 4,
                      n_max = 6,
                      col_names = F) %>% 
  mutate(clean_col = trimws(str_match(X1, "\\. (.*?) ~")[,2]))

df3 <- read_delim(file = str_c(getwd(), "/data/data_import/data_import/flights_03.csv"),
                  delim = "|",
                  skip = 12,
                  col_names = c(df3_names$clean_col),
                  comment = "#",
                  col_types = cols(.default = "c"),
                  trim_ws = T) %>% 
  mutate(across(c(UniqueCarrier, Origin, Dest), as.factor)) %>% 
  mutate(across(FlightNum, as.double)) %>% 
  mutate(Date = lubridate::ymd(Date))

##Exercise 3
#'Import big_table_04.csv 2 times, once through readr, once through data.table's fread
#'when importing with readr do column parsing to the point of import
#'when importing with fread force all columns to be parsed as characters
#'compare execution times 
#'
#'
#'Removed table to be able to sync to github
system.time(
df4_r <- readr::read_csv2(file = "./data/data_import/data_import/big_table_04.csv",
                   guess_max = 1000)
)

system.time(
df4_f <- data.table::fread(file = "./data/data_import/data_import/big_table_04.csv",
                           colClasses = "character")
)












