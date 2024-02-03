# Section 6 - purrr() - HW

library(tidyverse)
library(cowplot)
rm(list=ls())

# Exercise 1 
path <- "./data/assignment_06/simulations_blue_print.txt"

df <- read_delim(file = path,
                 delim = ";",
                 col_names = c("fun",
                               "size",
                               "par1",
                               "par2")) #%>% # comment out after this
  # mutate(size = str_extract(size, "[:digit:]+"),
  #        par1 = str_extract(par1, "[:digit:]+"),
  #        par2 = str_extract(par2, "[:digit:]+")) %>% 
  # mutate(across(c(size, par1, par2), as.double))

# create vectors for functions 
f <- df %>% pull(fun)

# create list of lists of arguements
args <- list()

for (r in 1:nrow(df)){
  args.row.text <- ""
  for(c in 2:ncol(df)){
    if(!is.na(df[r,c])){
      args.row.text <- str_c(args.row.text, df[r,c], sep = ",") # creates string vector of components of size var and 
    }
  }
  args.row.text <- str_remove(args.row.text, pattern = "^,|,$")
  args.row.text <- paste0("list(", args.row.text, ")")
  eval(parse(text = paste0("args.row.list = ", args.row.text)))
  args <- c(args, list(args.row.list))
}

## do the simulation

set.seed(123)

type <- read_csv(file = path, col_names = F) %>% 
  rename(type = X1) %>% 
  pull(type)

data.sim <- invoke_map(.f = f, .x = args) %>% 
  enframe() %>% 
  mutate(type = type,
         f = f) %>% 
  unnest(cols = c(value)) %>% 
  select(f, type, value)

# Exercise 2

## create subplots inside tibble
plots <- data.sim %>% 
  group_by(f) %>% 
  nest() %>% 
  mutate(plot = map(.x = data,
                    .f = ~ggplot(., aes(x = value,
                                        fill = type)) +
                      geom_density(alpha = 0.3) + 
                      scale_fill_viridis_d() +
                      ggtitle(paste0(f)) +
                      xlab("") +
                      theme_minimal()
                    ))

## draw subplots
library(cowplot)
plot_grid(plotlist = plots %>% pull(plot), nrow = 3)


# Exercise 3
rm(list = ls())
path <- "./data/assignment_06/gapminder"

df_countries <- tibble(directory = path,
              files = list.files(path)) %>% 
  mutate(path = str_c(directory, files, sep = "/")) %>% 
  mutate(data = map(.x = path,
                    .f = function(path_) {read_csv(path_, 
                                                   col_types = cols(          # parsing
                                                     country = col_character(),
                                                     continent = col_character(),
                                                     year = col_integer(),
                                                     lifeExp = col_double(),
                                                     pop = col_double(),
                                                     gdpPercap = col_double()
                                                   ))})) %>% 
  pull(data) %>% 
  bind_rows()

## create subplots inside tibble
plots <- df_countries %>% 
  group_by(continent) %>% 
  nest() %>% 
  mutate(plot = map(.x = data,
                    .f = ~ggplot(., aes(x = year,
                                        y = lifeExp,
                                        group = country,
                                        color = country)) +
                      geom_line(show.legend = F) + 
                      scale_color_viridis_d() +
                      scale_y_continuous(limits = c(0,100)) +
                      ggtitle(paste0(continent)) +
                      xlab("") +
                      theme_minimal()
  ))

## draw subplots
library(cowplot)
plot_grid(plotlist = plots %>% pull(plot))




