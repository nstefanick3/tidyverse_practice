# HW 4 Lubridate and HMS
library(tidyverse)
library(hms)

## Exercise 1: Parse the following with the lubridate package
"2021-01-15 23:05:30"
"2030-01-01 05"
"2000-28-02 10:15"
"1990-15-03 04"
"05/30/1995 9:15:45"
"1 Nov 2040 01/02:00"
"30 Jun 2035 20:45:00"
"20000101"
"January 1st 2029"
"October 2nd 2028"
"July 15th 2027"
"30th March 25"
"2015: Q2"

ymd_hms("2021-01-15 23:05:30")
ymd_h("2030-01-01 05")
ydm_hm("2000-28-02 10:15")
ydm_h("1990-15-03 04")
mdy_hms("05/30/1995 9:15:45")
dmy_hms("1 Nov 2040 01/02:00")
dmy_hms("30 Jun 2035 20:45:00")
ymd("20000101")
mdy("January 1st 2029")
mdy("October 2nd 2028")
mdy("July 15th 2027")
dmy("30th March 25")
yq("2015: Q2")

##' Exercise 2: Check what are the leap years between year 1 and year 3000 using 
##' lubridate and seq.Date()

# first create your date sequence 
yr1to3000 <- seq.Date(as.Date("01-01-01"), as.Date("3000-01-01"), by = "year")

# How many leap years are all together
yr1to3000 %>% 
  as_tibble() %>% 
  rename(seq = value) %>% 
  mutate(year = year(seq)) %>% 
  mutate(delta_year = year - lag(year)) %>% 
  mutate(ly_flag = leap_year(seq)) %>% 
  count(ly_flag)

# Which are the leap years
yr1to3000 %>% 
  as_tibble() %>% 
  rename(seq = value) %>% 
  mutate(year = year(seq)) %>% 
  mutate(delta_year = year - lag(year)) %>% 
  mutate(ly_flag = leap_year(seq)) %>% 
  filter(ly_flag == TRUE) %>% 
  pull(seq)

# count leap years per century
yr1to3000 %>% 
  as_tibble() %>% 
  rename(seq = value) %>% 
  mutate(year = year(seq)) %>% 
  mutate(delta_year = year - lag(year)) %>% 
  mutate(ly_flag = leap_year(seq)) %>% 
  filter(ly_flag == TRUE) %>% 
  mutate(century = floor(year/100)) %>% 
  group_by(century) %>% 
  summarise(`ct per century` = n()) %>% print(n = 30)

# all centuries do not have the same number of leap years, every 4th century has 25 while the rest have 24

## Exercise 3

# Store all holidays in a tribble

us.holidays <- tribble( ~holiday,                        ~date,
                        "New Year’s Day",                "2021-01-01",
                        "Martin Luther King, Jr. Day",   "2021-01-18",
                        "President’s Day",               "2021-02-15",
                        "Memorial Day",                  "2021-05-31",
                        "Independence Day",              "2021-07-04",
                        "Independence Day (observed)",   "2021-07-05",
                        "Labor Day",                     "2021-09-06",
                        "Columbus Day",                  "2021-10-11",
                        "Veterans Day",                  "2021-11-11",
                        "Thanksgiving Day",              "2021-11-25",
                        "Christmas Day (observed)",      "2021-12-24",
                        "Christmas Day",                 "2021-12-25",
                        "New Year’s Day (observed)",     "2021-12-31") %>% 
  mutate(date = ymd(date))

# how many days weeks hours seconds between holidays
us.holidays %>% mutate(priordate = lag(date)) %>% 
  mutate(days = as.double(date-priordate),
         weeks = days/7,
         hours = days*24,
         seconds = hours*60*60)

# Is today a holiday
today() %in% (us.holidays %>% pull(date))

#data prep
us.holidays <- us.holidays %>% 
  mutate(today = ymd("2021-06-26"),
         diff_today = as.period(date - today))

# Which holiday was the last one
td <- ymd("2021-06-01") - (us.holidays %>% pull(date))
min(td[td>0])
latest_holiday_index <- which(td==min(td[td>0]))
(us.holidays %>% pull(holiday))[latest_holiday_index]
#markos answer
us.holidays %>% filter(diff_today < 0) %>% arrange(diff_today) %>% select(holiday,date,diff_today) %>% tail(1)

# Which holiday will be the next one
td <- ymd("2021-06-01") - (us.holidays %>% pull(date))
max(td[td<0])
next_holiday_index <- which(td==max(td[td<0]))
(us.holidays %>% pull(holiday))[next_holiday_index]


# Exercise 4 

#  import the .csv
pjm <- read_csv("./data/energy_consumption/pjm_hourly_est.csv")

#  keep only columns Datetime and PJME
#  do not forget column parsing!
#  remove rows where PJME data is missing!
#  sort rows based on date time column
pjm %>% select(Datetime, PJME) %>% 
  drop_na(PJME)

#  check if data is for every hour in given time span?
difftime("2002-12-31 01:00:00", "2018-01-02 00:00:00", units = "hours")
#markos answer
pjm %>% mutate(diff_h = as.period(Datetime-lag(Datetime))/hours(1)) %>% filter(diff_h != 1)
###its not?

#  now add columns: date, month, year 
pjm <- pjm %>% 
  mutate(Datetime = ymd_hms(Datetime),
         econs = as.numeric(PJME)) %>% 
  select(Datetime, econs) %>% 
  drop_na(econs) %>% 
  mutate(day = day(Datetime),
         month = month(Datetime),
         year = year(Datetime)) %>% 
  arrange(Datetime)
#  calculate time intervals: year intervals, month intervals, day intervals
#  HINT: per year / month / day calculate minimum and maximum time stamp
#  HINT: calculate intervals using lubridate

###year consumption
pjm %>% select(Datetime, year, econs) %>% 
  group_by(year) %>% 
  #create interval
  mutate(int_start = min(Datetime),
         int_end = max(Datetime),
         int = int_start %--% int_end) %>% 
  ungroup() %>% 
  #caluculate averages
  group_by(year, int) %>% 
  summarize(econs_tot = sum(econs, na.rm = T),
            econs_avg = mean(econs, na.rm = T))


#  now use your intervals and calculate total and mean hourly energy consumption per each
# year / each month / each day










