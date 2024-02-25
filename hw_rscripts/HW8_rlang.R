# Exercise 1
# Create a function "count freq" that:
#  takes a data frame
#  creates frequency counts
#  before counting is applied the function does the grouping (only one grouping variable is
#                                                              used in the function call!)
#  at the end function renames variables in the output table
#  grouping variable is called "x"
#  frequencies variable is called "freq"

# count frequencies function
count_freq <- function(df, group, 
                       group_name = "unchanged group var", 
                       freq_name = "unchanged freq var"
                       ){
  require(dplyr)
  require(rlang)
  group_var <- dplyr::enquo(group)
  var_group_name <- dplyr::ensym(group_name)
  var_freq_name <- dplyr::ensym(freq_name)
  
  df %>% 
    group_by(!!group_var) %>% 
    summarise(!!var_freq_name := n()) %>% 
    ungroup() %>% 
    rename(!!var_group_name := !!group_var)
    
}

count_freq(mpg, manufacturer, 
           group_name = "manufacturer changed", 
           freq_name = "count special")
count_freq(mpg, manufacturer, 
           group_name = manufacturer_changed, 
           freq_name = count_special)
count_freq(mpg, manufacturer)


# Exercise 2 
# Create a function "draw bar plot" that:
#  takes the output of function "count freq" (from Exercise 1)
#  draws a bar plot
#  where variable "x" is used on x axis
#  variable "freq" is used on y axis
#  you can use geom col instead or geom bar (your choice)

count_freq_plot <- function(df, group, 
                       group_name = "unchanged group var", 
                       freq_name = "unchanged freq var"
){
  require(dplyr)
  require(rlang)
  group_var <- dplyr::enquo(group)
  var_group_name <- dplyr::ensym(group_name)
  var_freq_name <- dplyr::ensym(freq_name)
  
  df %>% 
    group_by(!!group_var) %>% 
    summarise(!!var_freq_name := n()) %>% 
    ungroup() %>% 
    rename(!!var_group_name := !!group_var) %>% 
    ggplot(aes(x = !!var_group_name,
               y = !!var_freq_name)) +
    geom_col() +
    xlab(var_group_name) + # note the ensymed variable but no bang bang
    ylab(var_freq_name)
}

count_freq_plot(mpg, manufacturer, 
           group_name = "manufacturer changed", 
           freq_name = "count special")
count_freq_plot(mpg, manufacturer, 
           group_name = manufacturer_changed, 
           freq_name = count_special)
count_freq_plot(mpg, manufacturer)


# Exercise 3
# Create a function "prepare diamonds data" that:
#  takes diamonds data frame
#  randomly selects n diamonds
#  creates variable called "volume"
#  where: volume = x  y  z

prepare_diamonds_data <- function(df = diamonds){
  df %>% 
    mutate(volume = x * y * z)
}

prepare_diamonds_data()


# Exercise 4 
# Create a function "explore diamonds" that:
#  takes the output of function "prepare diamonds data" (from Exercise 3)
#  draws scatter plot
#  where variable "carat" is used on x axis
#  variable "price" is used on y axis
#  variable "volume" is used for point size geom bar (your choice)
#  variable "color" is used for point color
#  add plot title and axis titles as function arguments
#  add custom plot theme inside the function

theme_fonts <- theme(
  plot.title = element_text(size = 20, face = "bold", hjust = .5),
  axis.title = element_text(size = 16, face = "italic", hjust = .5),
  axis.text = element_text(size = 14)
)

explore_diamonds <- function(title = "default title", 
                            xaxis = "default x axis title",
                            yaxis = "default y axis title"){
  prepare_diamonds_data() %>% 
    sample_n(., size=100) %>% 
    ggplot(aes(x = carat,
                   y = price,
                   size = volume,
                   color = color)) +
    geom_point() +
    theme_fonts +
    ggtitle(title) +
    xlab(xaxis) +
    ylab(yaxis)
}

explore_diamonds()

