# Exercise 1 
library(tidyverse)


rm(list =ls())
## a. How many rows and columns are in table hfights?
hflights <- hflights::hflights
nrow(hflights); ncol(hflights)

## b. How many different carriers are listed in the table (print a table with distinct carrier names)?
hflights %>% count(UniqueCarrier)
hflights %>% count(UniqueCarrier) %>% nrow()

## c. Which and how many airports were involved? Consider both origin and destination airports!
distinct(tibble(airports = c(hflights$Origin, hflights$Dest)))
#probably a better way than this with pipes
hflights %>% 
  select(Origin,Dest) %>% 
  distinct() %>% 
  pivot_longer(
    cols = everything(),
    names_to = "orgig/dest",
    values_to = "airport"
  ) %>% 
  distinct(airport) %>% 
  arrange(airport)


## d. How many fights were cancelled?
hflights %>% summarise(CancelledFlights = sum(Cancelled))



# Exercise 2 
# First, produce a table where statistics for each carrier is shown:
#  number of flights per carrier
#  total distance fown in miles per carrier
#  total actual elapsed time in hours per carrier
#  total air time in hours per carrier
#  mean distance per fight for each carrier
#  mean actual elapsed time in hours per flight for each carrier
#  mean air time in hours per flight for each carrier

summary <- hflights %>% 
  group_by(UniqueCarrier) %>% 
  summarise(num_flights = n(),
            total_distance_flown = sum(Distance),
            total_time_elapsed = round((sum(ActualElapsedTime, na.rm = T)/60),1),
            total_air_time = sum(AirTime, na.rm = T),
            mean_distance_flown = mean(Distance),
            mean_time_elapsed = round((mean(ActualElapsedTime, na.rm = T)/60),1),
            mean_air_time = mean(AirTime, na.rm = T)
            )

# Second, calculate the percentage of total distance flown by top 3 performing carriers VS total
# distance flown by remaining carriers. Execute steps:
#  first rank carriers by total distance flown
#  top 3 performers are in one group, remaining carriers are in second group
#  for each group calculate total distance flown
#  for each group calculate %: total distance flown per group / total distance all carriers

hflights %>% 
  group_by(UniqueCarrier) %>% 
  summarise(total_distance_flown = sum(Distance)) %>%
  ungroup() %>% 
  arrange(desc(total_distance_flown)) %>% 
  mutate(rank = row_number()) %>% 
  mutate(top_or_not = case_when(rank <= 3 ~ "top",
                                TRUE ~ "not")) %>% 
  group_by(top_or_not) %>% 
  summarise(sum_group = sum(total_distance_flown)) %>% 
  mutate(percent_group_total_flown = sum_group/sum(sum_group))


#Exercise 3

# Modify your main 
# ights table:
#    create date column by uniting columns: year, month, day of month
#  when uniting columns do not lose source columns (mutate each column - with slightly
#                                                    dierent name, before unite operation is executed)
#  you will need to parse date column after unite operation
#  also you should add leading zeros to month and day of month column before date is
# created
#  create columns: quarter, week

library(lubridate)

num_flights_quarterly <- hflights %>% 
  mutate(year_c = Year,
         month_c = Month,
         dayofmonth_c = DayofMonth) %>% 
  mutate_at(.tbl = ., .vars = c("month_c", "dayofmonth_c"), .funs = str_pad, 2, "left", "0") %>% 
  unite(new_date, c(year_c, month_c, dayofmonth_c), sep = "-", remove = F) %>% 
  mutate(Quarter = case_when(Month <= 3 ~ "Q1",
                             Month <= 6 ~ "Q2",
                             Month <= 9 ~ "Q3",
                             Month <= 12 ~ "Q4",
                             T ~ NA)) %>% 
  mutate(WeekofYear = week(ymd(new_date))) %>% 
  group_by(Quarter) %>% 
  summarise(num_flights = n()) %>% 
  mutate(fl_lead = lead(num_flights)) %>% 
  mutate(diff = fl_lead-num_flights)

sum_dist_monthly <- hflights %>% 
  mutate(year_c = Year,
         month_c = Month,
         dayofmonth_c = DayofMonth) %>% 
  mutate_at(.tbl = ., .vars = c("month_c", "dayofmonth_c"), .funs = str_pad, 2, "left", "0") %>% 
  unite(new_date, c(year_c, month_c, dayofmonth_c), sep = "-", remove = F) %>% 
  mutate(Quarter = case_when(Month <= 3 ~ "Q1",
                             Month <= 6 ~ "Q2",
                             Month <= 9 ~ "Q3",
                             Month <= 12 ~ "Q4",
                             T ~ NA)) %>% 
  mutate(WeekofYear = week(ymd(new_date))) %>% 
  group_by(Month) %>% 
  summarise(total_dist = sum(Distance)) %>% 
  mutate(dist_lead = lead(total_dist)) %>% 
  mutate(diff = dist_lead-total_dist)

ggplot(data=num_flights_quarterly, aes(x=Quarter, y = num_flights, group = 1)) +
  geom_line() +
  geom_point()

ggplot(data=sum_dist_monthly, aes(x=Month, y = total_dist, group = 1)) +
  geom_line() +
  geom_point()  



# Exercise 4
# The idea for the last exercise is another data wrangling task, where you will have to use
# technique called "pivoting". Build a table, that will resemble a heat map by:
#    for each carrier and month, calculate total number of flights then normalize total number of 
# flights (divide each value with maximum total number of flights, you must get values between 0 and 1!)
#  now pivot your table from long to wide format
#  so each row is represented with carrier, and each column is represented with month,
# normalized total number of 
# ights are values in table cells
# You should get a similar output:
#            Month Month 2 ... Month 12
# Carrier 1  x1;1  x1;2    ... x1;12
# Carrier 2  x2;1  x2;2    ... x2;12
# ...
# Carrier n xn;1 xn;2 ... xn;12
# Where xi;j is the normalized value of total 
# ights for carrier i and month j. In the solution
# video, the visualization of heat map will be shown using ggplot2 library.
df <- hflights::hflights

norm_fl_mo <- hflights::hflights %>% group_by(UniqueCarrier, Month) %>% 
  summarise(total_flights = n()) %>% 
  ungroup() %>% 
  mutate(total_flights_norm = total_flights/max(total_flights)) %>% 
  select(-total_flights) %>% 
  pivot_wider(names_from = Month, values_from = total_flights_norm)

norm_fl_mo_l <- hflights::hflights %>% group_by(UniqueCarrier, Month) %>% 
  summarise(total_flights = n()) %>% 
  ungroup() %>% 
  mutate(total_flights_norm = total_flights/max(total_flights))

ggplot(norm_fl_mo_l, aes(x=Month, y=UniqueCarrier, fill=total_flights_norm)) +
  geom_tile()

#newline
  
  
  
  
  
  
