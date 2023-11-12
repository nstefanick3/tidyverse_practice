# 04 lubridate and hms

rm(list=ls())

library(tidyverse)
library(hms)

# Data wrangle Dates / Times
#install.packages("nycflights13")
library(nycflights13)

# 4.2 create dates / times

# basic objects 
d <- as_date(18992)

t <- as_hms(120)
t

dt <- as_datetime(1640952000)

# parsing: string or number ...
ymd_hms("2021-12-31 12:00:00")
ymd_hm("2021-12-31 12:00")
ymd_h("2021-12-31 12")

dmy_hms("31 Dec 2021 22/15:00")

yq("2021: q3")
 
hms::hms(seconds = 1, minutes = 1, hours = 0)
lubridate::hms("00:01:05")


#date_decimal() parse date as a decimal number
d <- seq(2021,2022,0.25)
d
date_decimal(d)

# faste_strptime() - parse datetime
fast_strptime(x="2021-12-31 12:00:00", format = "%Y-%m-%d %H:%M:%S") %>% class() # must specify format

# parse_date_time() easier parsing
parse_date_time("2021-12-31 12:00:00", orders = "ymd HMS")

# Create date/time from individual components
flights

#create date and date time using other compontents
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(datetime = make_datetime(year, month, day, minute),
         date = make_date(year, month, day))

# create date / time from existing objects 

## current timestamp and todays date 
now()
today()
as_date(now())
as_datetime(today())

# 4.3 Components

# extract different components
dt <- now()
dt

year(dt)
month(dt)
day(dt)
hour(dt)
minute(dt)
second(dt)

# additional components
isoyear(dt)
epiyear(dt)
wday(dt)
qday(dt)
quarter(dt)
semester(dt)
am(dt)
pm(dt)
dst(dt) #daylight savings time
leap_year(dt)
cy <- seq.Date(as.Date("2023-1-1"), as.Date("2023-12-31"), by = "day")
cy
dst(cy)

dst("2023-6-6")
x <- ymd("2012-03-26")
dst

# store values of a components into a column
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(datetime = make_datetime(year, month, day, hour, minute)) %>% 
  mutate(wday = wday(datetime, week_start = 1),
         week = week(datetime),
         quarter = quarter(datetime)) %>% 
  arrange(desc(datetime))

# 4.4 rounding values and setting components

# rounding dates at the month level
d <- today()
d

floor_date(d, unit = "month")
ceiling_date(d, unit = "month")
round_date(d, unit = "month")
rollback(d)

floor_date(d, unit = "year")
ceiling_date(d, unit = "year")
round_date(d, unit = "year")

floor_date(now(), unit = "minute")

dt
year(dt) <- 2024 # change year
month(dt) <- 12

#update all in one take
update(dt, year = 2020, month = 1, day = 15, hour = 0, minute = 44, second = 5)

#too great values - rollback
dt
update(dt, month = 13) # 12 + 1 month = 13 months aka 1 year and 1 month

#' Date arithmatics, we introduce time span
#' durations - represent an exact number of seconds
#' periods track clock changes in human units like weeks/ months 
#' intervals - intervals on timelines with specific start an end points

# 4.5 data arithmatics and durations

# some basic arithmatics
td <- today()
td
td+1 # in days
td-1

now <- now()
now
now+1 # in seconds
now-1
now+3600

## how old are you
birth_date <- ymd("1995-02-27")
age <- td-birth_date
age
start_date <- ymd("2023-09-25")
td-start_date

# Durations
as.duration(age)

#constructor functions 
x <- 1
dyears(x)
dmonths(x)
dweeks(x)
dmilliseconds(x)

# duration class
age
is.duration(age)
as.duration(age)
is.duration(as.duration(age))

## duration - arithmetics

dseconds(10)+dminutes(1)
dyears(1)-dweeks(52/2) # 1 year minus 1 half year

10 * dmonths(1) # 10 times 1 month

# inconsistant timeline behavior (duration)

## DST
dt <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
dt
leap_year(dt)

dt + ddays(1) # daylight savings causes hours to skip forward by one
dt - ddays(1) # no hour change here

# leap year 
dt <- ymd_hms("2020-02-28 23:00:00", tz = "America/New_York")
dt
leap_year(dt)

# 4.6 periods

# Periods track changes in clock times, they do no track irregularities
# no "d's" in front

age

# age as period
as.period(age)
seconds(3600)
minutes(3600)
hours(1)
days(1)
weeks(52)
years(1)

period_to_seconds(years(1))
seconds_to_period(3600)

period(3600, units = "minute")

# Periods - arithmatic
seconds(10) + minutes(1)
years(1) + weeks(27)
10 * months(1)

# inconsisitent timeline behavior
dt <- ymd_hms("2016-03-12 13:00:00", tz = "America/New_York")
dt
dt+days(1) #ignores time changes
dt+ddays(1) # does not ignore time changes

# leap year
dt <- ymd_hms("2019-02-28 23:00:00")
dt2 <- ymd_hms("2020-02-28 23:00:00")
dt 
dt + days(1) # same as nextline
dt + ddays(1)
dt2 + days(1) # same as nextline
dt2 + ddays(1)

ymd_hms("2019-02-20 23:00:00") + dyears(1)
ymd_hms("2019-02-20 23:00:00") + years(1)

ymd_hms("2020-02-20 23:00:00") + dyears(1)
ymd_hms("2020-02-20 23:00:00") + years(1)

# Intervals

# create and interval
d1 <- ymd("2021-12-30")
d2 <- ymd("2021-12-31")
d1
d2
i1 <- interval(d1,d2)
i1
i2 <- d2 %--% d1 # reverse interval
i2

# extract boundries
int_start(i1)
int_end(i1)

# is time point within given interval
today() %within% i1

# Do intervals overlap?
int_overlaps(i1, i2)

# create intervals form vector of dates 
dates <- now() + days(1:365)
dates
length(dates)

int_dates <- int_diff(dates) # creates a vector of intervals from day to day
length(int_dates)

# length of interval / flip interval
i1
int_length(i1)
int_flip(i1)

## Timezones
Sys.timezone() # what R sees as your timezone

OlsonNames() # all timezone names
OlsonNames() %>% str_subset(., pattern = "US")

# "US" ~ "Europe" included in tz
OlsonNames() %>% str_to_lower() %>% str_subset(pattern = "europe")

with_tz(time = now(), tzone = "US/Alaska") # what is the current time in alaska

