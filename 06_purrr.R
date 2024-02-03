# Functional programming purrr()

## map() is a function used to map a function iteratively to each element of a list or a vector

rm(list=ls())
graphics.off()

library(tidyverse)
library(hflights)


# 6.2 map: apply function 

# map()

df <- hflights %>% 
  select(ActualElapsedTime, AirTime, Distance, TaxiIn, TaxiOut)

df <- df %>% 
  as_tibble()

df %>% map(.x = ., .f = mean, na.rm = T)
df %>% map(.f = min, na.rm = T)
df %>% map(.f = max, na.rm = T)
df %>% map(.f = sd, na.rm = T)

# Ex2
# trying to extract all numerice columns from a data frame
str(mpg) 
mpg %>%
  select(displ, year, cyl, cty, hwy) # long method 
  
numeric.cols <- map(.x = mpg, .f = is.numeric) %>% 
  unlist() %>% 
  tibble(column = names(.), # put list into tibble where the first column is the names 
         numeric = .) %>%   # and the second column is the data
  filter(numeric == T) %>% 
  pull(column) # faster if you have like 100 columns

numeric.cols

numeric.cols <- map(.x = hflights, .f = is.numeric) %>%   # now using hflights
  unlist() %>% 
  tibble(column = names(.), 
         numeric = .) %>% 
  filter(numeric == T) %>% 
  pull(column) 

numeric.cols


# 6.3: map() control the output

# map_dbl() - double numeric vector
df %>% map(.x = ., .f = mean, na.rm = T) %>% class() # is a list
df %>% map(.f = min, na.rm = T)
df %>% map(.f = max, na.rm = T)
df %>% map(.f = sd, na.rm = T)

df %>% map_dbl(.x = ., .f = mean, na.rm = T)
df %>% map_dbl(.x = ., .f = mean, na.rm = T) %>% class() # is a numerice named vector
df %>% map_dbl(.f = min, na.rm = T)
df %>% map_dbl(.f = max, na.rm = T)
df %>% map_dbl(.f = sd, na.rm = T)

# create a summary table
df %>% 
  colnames() %>% 
  tibble(variable = .,
         mean = df %>% map_dbl(., mean, na.rm = T),
         sd = df %>% map_dbl(., sd, na.rm = T))

# map_int() - integer vector
list <- list(a = 1,
             b = "word",
             c = 1:10,
             d = mpg)
list

## return length of each list item
list %>% map_int(., length) # conunts colunmns for a table

# map_dfc() - data frame column bind

## summaries
df %>% map_dfc(., mean, na.rm = T)


# 6.4 map shortcuts

# fit multiple regression lines to the cars data set for multiple cylendars

## prepare data
df.mpg <- mpg %>% 
  select(hwy, displ, cyl) %>% 
  mutate(cyl = as.factor(cyl))

## fit model - map() no shortcuts
models <- df.mpg %>% 
  split(.$cyl) %>% # split data by different number of cylinders
  map(function(df) lm(formula = hwy ~ displ, 
                      data = df)) # estimates linear model for each different type cly

## fit model - map() shorter syntax
models <- df.mpg %>% 
  split(.$cyl) %>% # split data by different number of cylinders
  map(~ lm(hwy ~ displ, data = .)) # estimates linear model for each different type cly
  # use dot as the data has to be taken from the left side of the pipe

models

# now extract R squared for each model

## longer syntax
models %>% 
  map(summary) %>% # goes through list and maps the summary
  map_dbl(~.$"r.squared") # grab the r-squared value from all summaries

## shorter syntax
models %>% 
  map(summary) %>%
  map_dbl("r.squared") # removed data pull from left before "r-squared"

# shortcuts for extracting elements by postion

## list
list <- list(list(1:3,4:6,7:9),
             list(10:12,13:15,16:18),
             list(19:21,22:24,25:27))
list

## extract third element from each sub-list and put the extraction inside of a list
list %>% map(3)


# 6.5 map over more than one argument

# map2()

?rnorm

## dist. params. 
mu <- c(0, -4, 5, 8)
sig <- c(1, 2, 1, 3)

data <- map2(.x = mu, .y = sig, .f = rnorm, n = 1000) %>% 
  enframe() %>%  # nest the values in a data frame 
  mutate(name = paste0("norm", 1:4)) %>% 
  unnest(cols = c(value))  # unnest the the list such that we have the name of the data frame with the
                          # 1000 values on the left

data %>% count(name)

## visualize
data %>% 
  ggplot(aes(x = value,
             color = name,
             fill = name)) +
  geom_density(size = 1.3,
               alpha = .25) +
  theme_minimal()


# pmap() - same function but greater than 2 parameters
n <- c(100, 100, 1500, 10000)
mu <- c(0, -4, 5, 8)
sig <- c(1, 2, 1, 3)

args <- list(n, mu, sig)

#generate data
pmap(.l = args, .f = rnorm) %>% str()
  
args <- tibble(mean = mu,
               sd = sig,
               n = n)

## generate data
data <- args %>% 
  pmap(.f = rnorm) %>% 
  enframe() %>%  # nest the values in a data frame 
  mutate(name = paste0("norm", 1:4)) %>% 
  unnest(cols = c(value))

data %>% count(name)

data %>% 
  ggplot(aes(x = value,
             color = name,
             fill = name)) +
  geom_density(size = 1.3,
               alpha = .25) +
  theme_minimal()

# invoke_map()

?runif
?rexp

## arguements
f <- c("rnorm", "runif", "rexp")

par <- list(
  list(mean = 1, sd = 3),
  list(min = 0, max = 5),
  list(rate = 0.5)
)

## generate the data
data <- invoke_map(.f = f, .x = par, n = 10000) %>% 
  enframe() %>% 
  mutate(name = f) %>% 
  unnest(cols = c(value))

data

## visualize
data %>% 
  ggplot(aes(x = value,
             color = name,
             fill = name)) +
  geom_density(size = 1.3,
               alpha = .25) +
  theme_minimal()

# walk function - use a function for its side effects
l <- list(1:3, c(2,4,6))

map(l, print)
walk(l, print)

## export plots

## create plots 
plots <- mpg %>% 
  split(.$manufacturer) %>% 
  map(~ggplot(., aes(displ, hwy)) +
  geom_point() + 
  ggtitle(paste0(.$manufacturer))) # prints all the output of the manufacturers plus the plot for each manufacturer

plots

graphics.off()


tables <- mpg %>% 
  split(.$manufacturer) %>% 
  walk(~ggplot(., aes(displ, hwy)) +
        geom_point() + 
        ggtitle(paste0(.$manufacturer))) # prints the list of the data tables associated with each manuf.

tables # no plots in walk

## create directory for plots
path <- "./data/pwalk_plots"

if(!dir.exists(path)){
  dir.create(path)
}

## export plots
list(str_c(path, "/", names(plots), ".pdf"), plots) %>% # list of paths for each plot and each plot
  pwalk(., ggsave) # walk drops side effects


# 6.6 work with lists

## Create a list

set.seed(123)

l1 <- T
l2 <- F
s1 <- words %>% sample(1)
s2 <- words %>% sample(1)
n1 <- runif(1,0, 1000)
n2 <- runif(1,0, 1000)

### vectors
vec.l1 <- sample(c(T,F), size = round(runif(1,1,100)), replace = T) # random true or false vectors a random # of times between 1 and 100
vec.l2 <- sample(c(T,F), size = round(runif(1,1,100)), replace = T)
vec.s1 <- words %>% sample(size = round(runif(1,1,100)), replace = T)
vec.s2 <- words %>% sample(size = round(runif(1,1,100)), replace = T)
vec.n1 <- runif(1,1,100) %>% sample(size = round(runif(1,1,100)), replace = T)
vec.n2 <- runif(1,1,100) %>% sample(size = round(runif(1,1,100)), replace = T)

### tables
t1 <- mpg
t2 <- diamonds %>% sample_n(size = 500)

### lists
list1 <- list(a = 1, b = "b", vec = 1:10)
list2 <- list(vec = seq(0,10,0.05), words[1:10])

### one list
list <- list(
  l1 = l1,
  l2 = l2,
  s1 = s1,
  s2 = s2,
  vec.l1 = vec.l1,
  vec.l2 = vec.l2,
  vec.s1 = vec.s1,
  vec.s2 = vec.s2,
  vec.n1 = vec.n1,
  vec.n2 = vec.n2,
  t1 = t1,
  t2 = t2
)

list %>% str()

## reshuffle elements
list.shuffled <- list[sample(1:length(list), size = length(list), replace = F)]
list <- list.shuffled
rm(list.shuffled)

list

#pluck

pluck(list, 1)
pluck(list, "l1")
pluck(list, "vec.l1")

# keep()

list %>% map(class) %>% unlist()

#keep only characters objects
keep(list, is.character)

#keep only logical objects
keep(list, is.logical)

#keep only characters objects
keep(list, is_tibble)


# discard() selects elements that do not pass logical test
discard(list, is.logical)


# head_while() / tail_while()
head_while(list, is_tibble) # keeps returning until not a tibble
list

# flatten() - remove a level of an index of a list
list.f <- flatten(list)

list %>% pluck("hwy")
list.f %>% pluck("hwy")

# transpose()
list.t <- transpose(list)
list.t
list.t %>% str()

## quick transpose demo
v1 <- 1:10
v2 <- 11:20
v3 <- 21:30
names(v1) <- names(v2) <- names(v3) <- letters[1:10]
list1 <- list(v1,v2,v3)

list1 %>%  transpose()


# 6.7 Summarise and Join lists

# every / some
every(list, is.atomic)
some(list, is.atomic)

#has_element()
has_element(list, l1)
has_element(list, mpg)
has_element(list, diamonds)

# detect / detect_index
detect(list, is.numeric) #returns the element that is numeric
detect_index(list, is.numeric) # returns the index position of the numeric vector

# vec_depth()
vec_depth(list)
pluck_depth(list) # return number of levels of indexes
vec_depth(list1)
vec_depth(list(a=1, b=1))

# append() / prepend() (prepend starts from the top)
sublist1 <- list(pluck(list,1), pluck(list, 2))
sublist2 <- list(pluck(list,3), pluck(list, 4))

append(sublist1, sublist2)
append(sublist1, sublist2, after = 0) # prepend now

# splice()
splice(l1, vec.l1, list1) # use list_flatten


# 6.8 Transform Lists

# modify()

modify(list, typeof)
modify(list, class)

modify_if(list, is.data.frame, nrow) # if the list element is a dataframe then transform it into 

modify_at(list, "vec.s2", length)

# cross2()
cross2(letters[1:3], 1:2) # was depricated now use tidyr::expand_grid
tidyr::expand_grid(letters[1:3], 1:2)

# reduce()
list.c <- list(sample(letters,17),
               sample(letters,17),
               sample(letters,17),
               sample(letters,17)
               )

list.n <- list(runif(n=10),
               runif(n=10),
               runif(n=10),
               runif(n=10)
               )

## intersection of letters
reduce(list.c, intersect) # letters present in all

## calculate cumulative sum numeric vectors
reduce(list.n, sum)

# accumulate()
accumulate(list.c, intersect)
accumulate(list.n, sum)


# 6.9 Nested Data

df <- mpg %>% 
  filter(manufacturer %in% c("jeep", "land rover", "lincoln")) %>% 
  select(manufacturer, model, displ, cyl, hwy)

## nest
df.n <- df %>% 
  group_by(manufacturer) %>% 
  nest()

## unnest
df1 <- df.n %>% 
  unnest(cols = c("data")) %>% 
  ungroup()

## operations that go with nesting
df.n$data %>% map(.x = ., .f = ~length(.$model)) # maps the length of the model (number of rows)
df.n$data %>% map(.x = ., .f = ~mean(.$hwy)) # mean of the hwy var of each model


## nesting a larger data frame
mpg %>% 
  group_by(manufacturer) %>% 
  nest()

diamonds %>% 
  group_by(cut, color) %>% 
  nest() %>% 
  mutate(`avg price` = map(data, ~mean(.$price)),
         `nr diamonds` = map(data, ~length(.$price))) %>% 
  mutate(`avg price` = unlist(`avg price`),
         `nr diamonds` = unlist(`nr diamonds`))


# 6.10 Nested Data Workflow

## model fit
df.models <- mpg %>% 
  group_by(manufacturer) %>% 
  nest() %>% 
  mutate(model = map(.x = data,
                     .f = ~lm(hwy ~ displ + cyl, data = .)))

# models' coefficients
model <- df.models %>% filter(manufacturer == "audi") %>% pull(model)

model %>% flatten() %>% pluck(coefficients) %>% enframe() %>% .[[2]] # gets coefficients

# model summary
model %>% map(summary) %>% map_dbl("r.squared")


## all manufacturers / all models
extract_coef_ <- function(model){
  coefficients(model)[[1]]
}
extract_coef <- function(model, id_coef){
  coefficients(model)[[id_coef]]
}


df.models <- df.models %>% 
   mutate(summary     = map(.x = model, .f = summary),
          `r squared` = map_dbl(.x = summary, .f = "r.squared"),
          `coef a0`   = map_dbl(.x = summary, .f = extract_coef, 1), # the 1 corresponds to id_coef
          `coef a1`   = map_dbl(.x = summary, .f = extract_coef, 2),
          `coef a2`   = map_dbl(.x = summary, .f = extract_coef, 3)
          )

## closer look to models where r squared = 0
df.models %>% 
  filter(`r squared` == 0) %>% 
  select(manufacturer, data) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  ggplot(aes(x = displ,
             y = hwy,
             color = as.factor(cyl))) +
  geom_point() +
  facet_wrap(. ~ manufacturer)


## take a closer look to model with highest r squared
df.models %>% 
  arrange(desc(`r squared`)) %>% 
  head(1) 

df.models %>% 
  arrange(desc(`r squared`)) %>% 
  head(1) %>% 
  select(manufacturer, data) %>% 
  unnest(cols = c(data)) %>% 
  ungroup() %>% 
  ggplot(aes(x = displ,
             y = hwy,
             color = as.factor(cyl))) +
  geom_point() +
  facet_wrap(. ~ manufacturer)

####### purrr practice part 1 ########

# 6.11 purrr practices - looping over importing data
mpg %>% 
  map_df(
    .x = .,
    .f = ~(data.frame(missing_values = sum(is.na(.x)),
           distinct_values = n_distinct(.x),
           class = class(.x))),
    .id = "variable")

# import files into R

## import .csv files - from single directory
path <- "./data/mpg_single_level"

df1 <- tibble(directory = path,
              files = list.files(path)) %>% 
  mutate(path = str_c(directory, files, sep = "/")) %>% 
  mutate(data = map(.x = path,
                    .f = function(path_) {read_csv(path_, 
                                                   col_types = cols(.default = "c"))})) %>% 
  pull(data) %>% 
  bind_rows()

## import multiple files and specify column types 
df2 <- tibble(directory = path,
              files = list.files(path)) %>% 
  mutate(path = str_c(directory, files, sep = "/")) %>% 
  mutate(data = map(.x = path,
                    .f = function(path_) {read_csv(path_, 
                                                   col_types = cols(          # parsing
                                                     manufacturer = col_character(),
                                                     model = col_character(),
                                                     displ = col_double(),
                                                     year = col_integer(),
                                                     cyl = col_integer(),
                                                     trans = col_character(),
                                                     drv = col_character(),
                                                     cty = col_double(),
                                                     hwy = col_double(),
                                                     fl = col_character(), 
                                                     class = col_character(),
                                                     ))})) %>% 
  pull(data) %>% 
  bind_rows()

##import csv files inside 2 level directory
path <- "./data/mpg_double_level"

df3 <- tibble(directory = path,
              files = list.files(path, recursive = T)) %>%  ## added in recursive
  mutate(path = str_c(directory, files, sep = "/")) %>% 
  mutate(data = map(.x = path,
                    .f = function(path_) {read_csv(path_, 
                                                   col_types = cols(.default = "c"))})) %>% 
  pull(data) %>% 
  bind_rows()

# export multiple tables into multiple csv files using map
getwd()
directory <- "./data/mpg_export"

if(!dir.exists(directory)){
  dir.create(directory)
}

df.export <- mpg %>% 
  #add car id
  group_by(manufacturer, model) %>% 
  mutate(car_id = row_number()) %>% 
  ungroup %>% 
  #add path to model
  mutate(path = paste0(directory, "/",
         manufacturer, "_",
         str_remove_all(model, pattern = " "), "_",
         car_id, ".csv")) %>% 
  #nest data
  select(-car_id) %>% 
  group_by(path) %>% 
  nest() %>% 
  ungroup()

## create a list of data and file path for pmap to export files
list(x = df.export$data,
     file = df.export$path) %>% 
  pmap(.l = ., .f = write_csv) %>% 
  quietly()

# draw multiple plots per one table
df.plots <- mpg %>% 
  group_by(manufacturer) %>% 
  nest() %>% 
  # draw the plot
  mutate(plot = map(.x = data,
                    .f = ~ggplot(., aes(x = displ,
                                        y = hwy,
                                        color = as.factor(cyl))) +
                      geom_jitter(size = 3) +
                      scale_color_viridis_d(option = "magma")))

## show a single plot
df.plots %>% pull(plot) %>% pluck(2)


## export multiple files

directory <- "./data/mpg_plot_export"

if(!dir.exists(directory)){
  dir.create(directory)
}

## add path for each plots
df.plots <- df.plots %>% 
  mutate(plot_path = paste0(directory, "/", manufacturer, ".png"))

## export plots
list(plot     = df.plots$plot,
     filename = df.plots$plot_path) %>% 
  pmap(.l = ., .f = ggsave) %>% 
  quietly()

# delete .csv from the mistake that marko made earlier
files.to.delete <- list.files(path = "./data") %>% str_subset(., pattern = ".csv$")

for(file in files.to.delete){
  path <- paste0("./data/", file)
  file.remove(file)
}









