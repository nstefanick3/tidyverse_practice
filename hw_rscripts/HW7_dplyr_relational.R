# Exercise 1

library(tidyverse)
library(nycflights13)

View(flights)
View(airlines)
View(planes)
flights %>% 
  left_join(x = .,
            y = airlines,
            by = "carrier") %>% 
  rename(carrier_name = name) %>% 
  left_join(x = .,
            y = planes,
            by = c("tailnum" = "tailnum")) %>% 
  ggplot(aes(x = manufacturer,
             fill = manufacturer)) +
  geom_bar(stat = "count")

# Exercise 2
df.w <- flights %>%   
  left_join(x = .,
            y = weather,
            by = c("year" = "year",
                   "month" = "month",
                   "day" = "day",
                   "hour" = "hour",
                   "origin" = "origin")) %>% 
  ggplot(aes(x = arr_delay,
             y = precip)) +
  geom_point()

# Exercise 3
distance_per_date <- flights %>% 
  mutate(Date = ymd(paste(year, month, day, sep = "-"))) %>% 
  select(carrier, Date, distance) %>% 
  group_by(carrier, Date) %>% 
  summarise(grouped_distance = sum(distance)) %>% 
  ungroup()

dates_span <-  flights %>% 
  mutate(Date = ymd(paste(year, month, day, sep = "-"))) %>% 
  select(Date) %>% 
  distinct() %>% 
  arrange(Date)

# Exercise 4
distance_per_date %>% pull(carrier) %>% unique() %>% length()
16*365

dates_span %>% 
  left_join(x = .,
            y = distance_per_date,
            by = "Date")

distance_per_date %>% 
  expand(Date, carrier) %>% 
  left_join(x = .,
            y = distance_per_date,
            by = c("Date" = "Date",
                   "carrier" = "carrier")) %>% 
  replace_na(list(grouped_distance = 0)) %>% 
  group_by(carrier) %>% 
  mutate(running_carrier_total = cumsum(grouped_distance)) %>% 
  ungroup() %>% 
  ggplot(aes(x = Date,
             y = running_carrier_total,
             color = carrier,
             group = carrier)) +
  geom_line(size = 1.2) +
  scale_y_log10() + 
  scale_color_viridis_d(option = "inferno")





