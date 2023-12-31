## Section 5 ggplot HW
library(hflights)
library(tidyverse)
library(ggwordcloud)

#Exercise 1
top4carriers <- hflights %>% 
  as_tibble() %>% 
  group_by(UniqueCarrier) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(n=4) %>%
  pull(UniqueCarrier)

hflights %>% 
  as_tibble() %>% 
  filter(UniqueCarrier %in% top4carriers) %>% 
  filter(Distance < 2000) %>% 
  ggplot(aes(x=Distance,
             fill = UniqueCarrier)) +
  geom_density(alpha = .5) +
  theme_minimal()


#Excercise 2
set.seed(123)
diamonds %>% 
  sample_n(., size = 10000, replace = F) %>% 
  mutate(volume = x*y*z) %>% 
  filter(carat < 2.5 &
           price < 15000 &
           volume < 600) %>% 
  ggplot(aes(x = carat, y = price, 
             size = volume, color = cut)) +
  geom_point() +
  scale_size(range = c(1,2)) +
  facet_wrap(. ~ color) + 
  theme_minimal() 

#Exercise 4
con <- unz("./data/corpus.zip", "corpus.txt")
corpus <- readLines(con)
close(con)
rm(con)

#create new object clean.corpus
corpus.clean <- corpus %>% 
  str_to_lower() %>% 
  str_remove_all("[:punct:]") %>% 
  str_remove_all("[:digit:]") %>%
  str_remove_all("[\<\>]") %>% 
  str_replace_all(pattern = "\t|\n", replacement = " ") %>% 
  str_trim(side = "both") %>% 
  str_replace_all(pattern = "\\s{2,}", replacement = " ")

## create a table called corpus.words with 1 column for words and one for freq
corpus.words <- corpus.clean %>% 
  str_split("[:blank:]") %>% 
  unlist() %>% 
  as_tibble() %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  rename("word" = "value", "count" = "n") %>% 
  mutate(`% coverage` = (count/sum(count))*100) %>% 
  mutate(group = case_when(str_starts(word, "[abcde]") ~ "a",
                            str_starts(word, "[fghij]") ~ "b",
                            str_starts(word, "[klmno]") ~ "c",
                            str_starts(word, "[pqrst]") ~ "d",
                            str_starts(word, "[uvwxyz]") ~ "e",
                            TRUE ~ "f"))%>% 
  head(n=200) %>% 
  mutate(angle = 90 * sample(c(0,1), n(), replace = T, prob = c(.7, .3))) %>% 
  mutate(angle1 = 45 * sample(c(-2:2), n(), replace = T, prob = c(1, 1, 4, 1, 1)))


corpus.words %>%  
  ggplot(aes(label = word,
             size = count,
             color = group,
             angle = angle1)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) +
  scale_color_viridis_d(option = "magma") +
  theme_minimal()


#Exercise 4






