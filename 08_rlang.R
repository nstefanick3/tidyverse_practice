# Section 8 - rlang

library(tidyverse)

#> Quosures
#> rlang::quo(expression) quote expression or expressions as a quosure
#> rlang::enquo(arguement) call from within a function to quote what the user passed to an argument as a quosure
#> 
#> Quasiquotation
#> unquotes some parts
#> !! unquotes the symbol or call that follows
#> !!! unquotes a vector or list and splices the results as arguments into the surrounding call
#> := replaces an = to allow unquoting with the name the appears on the left hand side of the =
#> 
#> Expressions
#> rlang::expr(expression) quote contents
#> rlang::ensym(x) call from within a function to quote what the user passed to and argument as a symbol
#> 

# Programming recipes part 1
## Prgram with quoting function
data_median <- function(data, var){
  require(dplyr)
  
  var <- rlang::enquo(var) #capture users argument that will be quoted with enquo
  
  data %>% summarise(median = median(!!var)) #unquote users argument into the quoting function with !!
}

data_median(mpg, cty)

data_median_wrong <- function(data, var){
  require(dplyr)
  #var <- rlang::enquo(var) 
  data %>% summarise(median = median(var)) #removed !! operator
}
data_median_wrong(mpg, "cty") # cty
data_median(mpg, cty) #17
mpg %>% summarise(median = median(cty)) # 17
mpg %>% summarise(median = median("cty")) # cty
mpg %>% summarise(median = median(!!"cty")) # cty
mpg %>% summarise(median = median(enquo(cty))) #error, cannot summarize quosure
mpg %>% summarise(median = median(!!enquo(cty))) #error object cty not found
mpg %>% summarise(median = median(quo(cty))) #must unquote the quosure
mpg %>% summarise(median = median(!!quo(cty))) #17 yay, enquo is for inside funtions, quo is for outside


data_median_wrong(mpg, cty)
median(c("gg", "hh", "ii"))
randomise <- function(f) {(runif(1e3))}
randomise(mean)

group_median <- function(data, var, ...){
  require(dplyr)
  
  var <- rlang::enquo(var)
  group_vars <- rlang::enquos(...)
  
  data %>% 
    group_by(!!!group_vars) %>% 
    summarise(median = median(!!var))
  }
mpg
group_median(mpg, cty, manufacturer, model)

named_median <- function(data, var, name){
  require(dplyr)
  
  var <- rlang::enquo(var)
  name <- rlang::ensym(name)
  
  data %>% 
    summarise(!!name := median(!!var))
}
named_median(mpg, cty, "go")
named_median(mpg, cty, "go with space")
named_median(mpg, cty, gonoquotes)


############ understanding outputs from rlang from https://stackoverflow.com/questions/57960245/what-is-the-difference-between-ensym-and-enquo-when-programming-with-dplyr ################

library(rlang)
library(dplyr, warn.conflicts = FALSE)

head(iris)

test <- function(x){
  Species <- "inside"
  cat("--- enquo builds a quosure from any expression\n")
  print(enquo(x))
  cat("--- ensym captures a symbol or a literal string as a symbol\n")
  print(ensym(x))
  cat("--- evaltidy will evaluate the quosure in its environment\n")
  print(eval_tidy(enquo(x)))
  cat("--- evaltidy will evaluate a symbol locally\n")
  print(eval_tidy(ensym(x)))
  cat("--- but both work fine where the environment doesn't matter\n")
  identical(select(iris,!!ensym(x)), select(iris,!!enquo(x)))
}

Species <- "outside"
test(Species)
test("Species")
test(paste0("Spec", "ies"))

# Programming recipes part 2
library(rlang)

# prog. with quoting function

# Write funtion - dplyr

## summary function

my_summary <- function(df, var, ...){
  require(dplyr)
  require(rlang)
  
  var <- enquo(var)
  group_vars <- enquos(...)
  
  df %>% 
    group_by(!!!group_vars) %>% 
    summarise(min = min(!!var),
              max = max(!!var),
              median = median(!!var),
              mean = mean(!!var),
              sd = sd(!!var),
              range = max - min) %>% 
    ungroup()
}
my_summary(mpg, cty, manufacturer)

# count frequencies function
count_freq <- function(df, ...){
  require(dplyr)
  require(rlang)
  group_vars <- enquos(...)
  
  df %>% 
    group_by(!!!group_vars) %>% 
    summarise(freq = n()) %>% 
    ungroup()
}

count_freq(mpg)
count_freq(mpg, manufacturer)

# moving average - function

## some data
## daily COVID 19 infections 

df.infections <- tibble(date = seq.Date(from = as.Date("2021-01-01"),
                                        to = as.Date("2021-01-31"),
                                        by = "day"),
                        inf = 101:131)
df.infections  

## create a 3 or 5 day moving average
moving_average_infections <- function(df = df.infections, var = inf){
  require(rlang)
  require(dplyr)
  require(tidyr)
  
  var <- enquo(var)
  
  df %>% 
    # add lag values t-1 up to t-6
    mutate(x1 = lag(!!var, 1),
           x2 = lag(!!var, 2),
           x3 = lag(!!var, 3),
           x4 = lag(!!var, 4),
           x5 = lag(!!var, 5),
           x6 = lag(!!var, 6)) %>% 
    # replace NA with 0
    mutate_at(., .vars = paste0("x", 1:6), .funs = replace_na, 0 ) %>% 
    # calculate moving averages
    mutate(three_day_avg = (!!var + x1 + x2) / 3,
           seven_day_avg = (!!var + x1 + x2 + x3 + x4 + x5 + x6) / 7)
}
moving_average_infections()

# visualize this data
moving_average_infections() %>% 
  filter(date > "2021-01-05") %>% 
  select(date, inf, three_day_avg, seven_day_avg) %>% 
  pivot_longer(cols = c("inf", "three_day_avg", "seven_day_avg"),
               names_to = "variable",
               values_to = "value") %>% 
  ggplot(aes(x = date,
             y = value,
             color = variable)) +
  geom_line()


# rlang with ggplot

## generic funcntion that will plot a histogram

plot_histogram <- function(df, x){
  require(ggplot2)
  require(rlang)
  
  x_var <- enquo(x)
  
  df %>% 
    ggplot(aes(x = !!x_var)) +
    geom_histogram()
  
}

plot_histogram(mpg, hwy)
plot_histogram(mpg, hwy) + ggtitle("My Hist")

# generic scatter

plot_scatter <- function(df, x, y){
  require(ggplot2)
  require(rlang)
  
  x_var <- enquo(x)
  y_var <- enquo(y)
  
  df %>% 
    ggplot(aes(x = !!x_var, y = !!y_var)) +
    geom_point()
}

plot_scatter(mpg, hwy, cty)

# generic scatter with custom theme

## theme
theme_fonts <- theme(
  plot.title = element_text(size = 20, face = "bold", hjust = .5),
  axis.title = element_text(size = 16, face = "italic", hjust = .5),
  axis.text = element_text(size = 14)
)

plot_scatter2 <- function(df, x, y, color,
                         title = "", 
                         x_title = "", 
                         y_title = ""){
  require(ggplot2)
  require(rlang)
  
  x_var <- enquo(x)
  y_var <- enquo(y)
  col_var <- enquo(color)
  
  df %>% 
    ggplot(aes(x = !!x_var, 
               y = !!y_var,
               color = !!col_var)) +
    geom_point() +
    ggtitle(title) +
    xlab(x_title) +
    ylab(y_title) + 
    theme_minimal() +
    theme_fonts
}

plot_scatter2(mpg, hwy, cty, color = class, "the title", "the x axis", "the y axis")

