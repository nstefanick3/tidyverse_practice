# ggplot2 Date Visualization

rm(list=ls())
graphics.off()

library(tidyverse)

# visualize distribution of continueous variable

# histogram

## some random data - uniform continueous distribution
set.seed(1235)

df.unif <- runif(n=100000, min = 0, max = 1) %>% 
  tibble(x = .)

############ Some of above section got deleted somehow ##############

# Bar Plot
groups <- paste("group", 1:4, sep = " ")
probs <- c(.2, .3, .4, .1)
sum(probs)

set.seed(123)
df.data <- sample(groups,
                  size = 1000,
                  replace = T,
                  prob = probs) %>% 
  tibble(group = .)

## bar plot - stat = "count"
df.data %>% 
  ggplot(aes(x = group)) +
  geom_bar(stat = "count")

## bar plot - stat = "identity"
df.data %>% 
  group_by(group) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = group,
             y = freq)) +
  geom_bar(stat = "identity")

##bfill colors
df.data %>% 
  ggplot(aes(x = group,
            fill = group)) + 
  geom_bar(stat = "count")

## alter manual fill colors - scaling

# a) manual acolors
df.data %>% 
  ggplot(aes(x = group,
             fill = group)) + 
  geom_bar(stat = "count") +
  scale_fill_manual(values = c("red", "green", "blue", "grey"))

# palllete brewer
df.data %>% 
  ggplot(aes(x = group,
             fill = group)) + 
  geom_bar(stat = "count") +
  scale_fill_brewer(palette = 3)

# viridis - pallette
df.data %>% 
  ggplot(aes(x = group,
             fill = group)) + 
  geom_bar(stat = "count",
           color = "black") +
  scale_fill_viridis_d(option = "magma")

# add lables on top of each column
df.data %>% 
  group_by(group) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = group,
             y = freq,
             fill = group)) +
  geom_bar(stat = "identity",
           color = "black") + 
  scale_fill_viridis_d(option = "magma") + 
  geom_text(aes(label = freq,
                y = freq + 10),
            size = 10)

## cars dataset
figure <- mpg %>% 
  group_by(manufacturer) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(percentage = round(n / sum(n) * 100, 1),
         label = paste(n, " | ", percentage, "%", sep = "")) %>% 
  arrange(desc(n)) %>% 
  mutate(manufacturer = as.factor(manufacturer),
         manufacturer = fct_inorder(manufacturer)) %>% 
  ggplot(aes(x = manufacturer,
             y = n,
             fill = manufacturer)) + 
  geom_bar(stat = "identity",
           show.legend = F,
           color = "black") + 
  # labels
  geom_text(aes(label = label,
                y = n + 1),
            size = 2) +
  scale_fill_viridis_d(option = "inferno", direction = -1) + 
  xlab("Car Manufacturer") +
  ylab("Car Count") +
  ggtitle("Number of Cars per each Manf.")
  
ggsave(filename = "./data/cars_per_manf.png",
       plot = figure,
       units = "cm", width = 29, height = 21, dpi = 300)
  
  
# 5.4 Scatter plots for continueous variables

# Scatter plot

mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) + 
  geom_point(color = "red",
             size = 5,
             shape = 17)

## add a regression line 
mpg %>%
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = T)

# add scales to the axis
mpg %>%
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0,50,2.5),
                     limits = c(0,50)) +
  scale_y_continuous(breaks = seq(0,50,2.5),
                     limits = c(0,50)) 

# Diamonds dataset
set.seed(123)

df.diamonds <- diamonds %>% sample_n(size = 10000, replace = F)

df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point()

## include transparency
df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point(size = 3, alpha = 1/5) 

## alter y axis, transform nonlinear trend to linear
# sqrt transformation or log10 transformation
df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point() +
  scale_y_sqrt()

df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()

# add smoothing line - auto detect
df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price)) +
  geom_point() +
  geom_smooth() #GAM models used

# 5.5 2 categorical variables

# bar plot

## "stacked"
mpg %>% 
  ggplot(aes(x = manufacturer,
             fill = class)) +
  geom_bar(position = "stack",
           color = "black") +
  scale_fill_viridis_d()

# "dodged"
mpg %>% 
  ggplot(aes(x = manufacturer,
             fill = class)) +
  geom_bar(position = "dodge",
           color = "black") +
  scale_fill_viridis_d()

#3 "filled"
mpg %>% 
  ggplot(aes(x = manufacturer,
             fill = class)) +
  geom_bar(position = "fill",
           color = "black") +
  scale_fill_viridis_d()

# Scatter Plot

## No colors altered 
mpg %>% 
  ggplot(aes(x = manufacturer,
             y = class)) +
  geom_point()

#vs jitter
mpg %>% 
  ggplot(aes(x = manufacturer,
             y = class,
             color = class)) +
  geom_point(position = "jitter") + # or geom_jitter()
  scale_color_viridis_d()

# 5.6 continueous vs categorical variables 

## Box Plot
mpg %>% 
  ggplot(aes(x = class,
         y = hwy)) + 
  geom_boxplot()

diamonds %>% 
  ggplot(aes(x = color,
             y = price,
             fill = color))+
  geom_boxplot() +
  scale_fill_viridis_d() +
  scale_y_log10()

## tweak parameters of a boxplot
mpg %>% 
  ggplot(aes(x = class,
             y = hwy)) + 
  geom_boxplot(fill = "brown1",
               outlier.colour = "blue",
               outlier.size = 5,
               size = 1.4)

# violin plot
diamonds %>% 
  ggplot(aes(x = color,
             y = price,
             fill = color))+
  geom_violin() +
  scale_fill_viridis_d() +
  scale_y_log10()

## tweak parameters of violin
mpg %>% 
  ggplot(aes(x = class,
             y = hwy)) + 
  geom_violin(fill = "brown1",
               scale = "area") ##

#Modify theme layer
mpg %>% 
  ggplot(aes(x = class,
             y = hwy)) + 
  geom_boxplot() +
  theme_classic()

#custom theme
mpg %>% 
  ggplot(aes(x = class,
             y = hwy,
             fill = class)) + 
  geom_boxplot() +
  xlab("Class") + 
  ylab("Hwy consumptions") +
  ggtitle("the title") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 18),
        axis.title.x = element_text(margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(size = 25, face = "bold", hjust = .5),
        legend.background = element_rect(fill = "grey",
                                         color = "black",
                                         linetype = "dashed"))

# 5.7 Multiple variables on one plot 

# facets

# facet_wrap()
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_jitter() +
  facet_wrap(#vars(class),
             . ~ class,
             scales = "free")

# multiple variables
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_jitter() +
  facet_wrap(#vars(class),
    drv ~ class,
    scales = "free")

# facet_grid()
mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_jitter() +
  facet_grid(#vars(class),
    . ~ class,
    scales = "free")

mpg %>% 
  ggplot(aes(x = cty,
             y = hwy)) +
  geom_jitter() +
  facet_grid(#vars(class),
    drv ~ class,
    scales = "free")

# add multiple features

## 2 continueous vars + point size = cont.
mpg %>% 
  ggplot(aes(x=cty,
             y=hwy,
             size = cyl)) +
  geom_jitter() +
  scale_size(range = c(1,10))

#by color
mpg %>% 
  ggplot(aes(x=cty,
             y=hwy,
             color = cyl)) +
  geom_jitter() 

mpg %>% 
  ggplot(aes(x=cty,
             y=hwy,
             color = class)) +
  geom_jitter() 

# shape
mpg %>% 
  ggplot(aes(x=cty,
             y=hwy,
             shape = class)) +
  geom_jitter() 

## final plot
df.diamonds %>% 
  ggplot(aes(x = carat,
             y = price,
             color = cut)) +
  geom_jitter()+
  facet_grid(color ~ clarity,
             scales = "free",
             labeller = "label_both")

# 5.8 Time series

# line chart
economics %>% 
  ggplot(aes(x = date,
             y = unemploy)) +
  geom_line()

# multiple time series using wide table format
economics %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = unemploy), color = "red", size = 1.2, linetype = "dashed") +
  geom_line(aes(y = pce), color = "blue") +
  geom_line(aes(y = psavert), color = "black") +
  scale_y_log10()

#multiple time series - long table format
economics_long %>% 
  ggplot(aes(x = date,
             y = value,
             group = variable,
             color = variable)) + 
  geom_line() + scale_y_log10()

economics_long %>% 
  filter(variable != "pop") %>% 
  ggplot(aes(x = date,
             y = value,
             group = variable,
             color = variable)) + 
  geom_line() + 
  scale_x_date(date_breaks = "5 years", date_labels = "%Y-%m")

# 5.9 Heatmaps, maps, ordinary maps

# Heat map
mpg %>% 
  group_by(manufacturer, class) %>% 
  summarise(cars = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = class,
             y = manufacturer,
             fill = cars)) +
  geom_tile() + 
  scale_fill_viridis_c(
    option = "magma", direction = -1) #more black means more cars, more yellow means less cars

mpg %>% 
  group_by(manufacturer, class) %>% 
  summarise(hwy_mean = mean(hwy)) %>% 
  ungroup() %>% 
  ggplot(aes(x = class,
             y = manufacturer,
             fill = hwy_mean)) +
  geom_tile() + 
  scale_fill_viridis_c(
    option = "magma", direction = -1)

#word cloud
install.packages("ggwordcloud")
library(ggwordcloud)

df.cars <- mpg %>% 
  count(model, manufacturer)

set.seed(123)
df.cars %>% 
  ggplot(aes(label = model,
             size = n)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20)+
  theme_minimal()

## word rotations
df.cars <- df.cars %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(.7, .3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1, 1, 4, 1, 1)))

df.cars %>% 
  ggplot(aes(label = model,
             size = n,
             angle = angle1,
             color = manufacturer)) + 
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  scale_size_area(max_size = 20) +
  scale_color_viridis_d(option = "magma") +
  theme_minimal()

# Map

## Crime map data
df.crime <- USArrests %>% 
  mutate(region = str_to_lower(rownames(.))) %>% 
  left_join(x = .,
            y = map_data("state"), #splits the data up so it can be read in as a map
            by = "region")

## draw the map
df.crime %>% 
  ggplot(aes(x = long,
             y = lat,
             group = group))+
  geom_polygon(aes(fill = Assault),
               color = "white") +
  scale_fill_viridis_c(option = "magma") +
  theme_minimal()

# 5.10 Subplots - cowplot
#install.packages("cowplot")
library(cowplot)

# some smaller plots
p1 <- ggplot(mpg, aes(x = cty, y = hwy)) + geom_jitter()
p2 <- ggplot(mpg, aes(x = displ, y = hwy)) + geom_jitter()
p3 <- ggplot(mpg, aes(x = cyl, y = hwy)) + geom_jitter()
p4 <- ggplot(mpg, aes(x = drv, y = cty)) + geom_jitter()
p5 <- ggplot(mpg, aes(x = trans, y = hwy)) + geom_jitter()
p6 <- ggplot(mpg, aes(x = class, y = hwy)) + geom_jitter()

# create subplots
plot_grid(p1, p2, p3, p4, labels = "AUTO") # auto labels
plot_grid(p1, p2, p3, p4, labels = c("p1", "p2", "p3", "p4")) # custom lables

plot_grid(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2) #subplot dimensions
plot_grid(p1, p2, p3, p4, p5, p6, nrow = 6, ncol = 1)










