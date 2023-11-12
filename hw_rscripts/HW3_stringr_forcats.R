# HW Section 3 stringr and forcats

rm(list = ls())
graphics.off()


library(tidyverse)
con <- unz("./data/corpus.zip", "corpus.txt")
corpus <- readLines(con)
close(con)
rm(con)

# Exercise 1

## Check the number of lines in corpus
length(corpus)

## Check number of charachters
sum(str_length(corpus))

## Print first and last 6 lines
corpus[c(1:6,(length(corpus)-5):length(corpus) )]
corpus %>% head()
corpus %>% tail()

## Count how many lines use at least one form of punctuation
sum(str_detect(corpus, "[:punct:]"))
corpus %>% str_detect("[:punct:]") %>% sum()

## Show first 20 lines without any puctnctuation
str_remove_all(corpus[1:20], "[:punct:]")
corpus %>% str_subset("[:punct:]", negate = T) %>% head(n=20)

## Inspect first 10 lines with digit present
corpus[str_which(corpus, "[:digit:]")[1:10]]

## find string patterns the resemble phone numbers
## search for pattern like ddd-dddd
str_which(corpus,"[:digit:]{3}\\-[:digit:]{4}")
str_view_all(corpus,"[:digit:]{3}\\-[:digit:]{4}", match = T)

## Find string patterns that resemble $
str_view_all(corpus,"\\$", match = T)

## How many lines start with the word "The"
str_sub(corpus,1,3) %>% 
  str_detect("The") %>% 
  sum()
str_view_all(corpus,"^The", match = T)


# Exercise 2

## Use corpus and try to figure out which words usually come before a comma
str_extract_all(corpus, "[:alpha:]+(?=,)") %>% unlist()
#str_view_all(corpus, "[:alpha:]+(?=,)", match = T)    #don't run this takes super long

## If you consider the first 5 letters at the beggining of each line, what are the 
## top patterns that the lines start with
str_sub(corpus, 1, 5) %>%
  str_to_lower() %>% 
  as_tibble() %>% 
  count(value) %>% 
  arrange(desc(n))

## find words where a vowel is followed by a vowel
str_split(corpus, "[:blank:]") %>% 
  unlist() %>% 
  str_subset("[aeiou](?=[aeiou])")

## find words where a vowel is followed by 2 or more vowels
str_split(corpus, "[:blank:]") %>% 
  unlist() %>% 
  str_subset("[aeiou]+(?=[aeiou]{2,})")

## find words where 2 vowels are not followed by a vowel
str_split(corpus, "[:blank:]") %>% 
  unlist() %>% 
  str_subset("[aeiou]{2}(?![aeiou])")

## Check occurrences of 'the', 'be', 'to', 'of', 'and'
str_split(corpus, "[:blank:]") %>% 
  unlist() %>% 
  str_remove_all("[^[:alpha:]]") %>% 
  str_to_lower() %>% 
  str_subset("^the$|^be$|^to$|^of$|^and$") %>% 
  as_tibble() %>% 
  count(value) %>% 
  arrange(desc(n))

## Most common word counts, sort by their frequency
str_split(corpus, "[:blank:]") %>% 
  unlist() %>% 
  str_remove_all("[^[:alpha:]]") %>% 
  str_to_lower() %>% 
  #str_subset("^the$|^be$|^to$|^of$|^and$") %>% 
  as_tibble() %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  print(n=50)

## Most common words before converting to lower case
str_split(corpus, "[:blank:]") %>% 
  unlist() %>% 
  str_remove_all("[^[:alpha:]]") %>% 
  #str_to_lower() %>% 
  #str_subset("^the$|^be$|^to$|^of$|^and$") %>% 
  as_tibble() %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  print(n=50)

## Top 3 most common words
top3 <- str_split(corpus, "[:blank:]") %>% 
  unlist() %>% 
  str_remove_all("[^[:alpha:]]") %>% 
  str_to_lower() %>% 
  #str_subset("^the$|^be$|^to$|^of$|^and$") %>% 
  as_tibble() %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  filter(value != "") %>% 
  slice_head(n=3) %>% 
  pull(value)

corpus_lower <- str_to_lower(corpus)
## Count the number of lines where only one word is present
x <- str_which(corpus_lower, "the")
y <- str_which(corpus_lower, "to")
z <- str_which(corpus_lower, "and")

only_the <- setdiff(x, c(y,z)) # has the only the and not to or and
only_to <- setdiff(y, c(x,z))
only_and <- setdiff(z, c(x,y))

num1 <- length(c(only_the, only_to, only_and))

## count the number of lines where two words are present
the_to <- setdiff(intersect(x,y),z)
the_and <- setdiff(intersect(x,z),y)
to_and <- setdiff(intersect(y,z),x)

num2 <- length(c(the_to, the_and, to_and))

## number of lines where all three words are present
num3 <- length(intersect(intersect(x,y),z))


#what are the percentages of each scenario
num1/50000
num2/50000
num3/50000
num1/50000 + num2/50000 + num3/50000

corpus.lower <- corpus %>% str_to_lower()

## MArkos Answer
tibble(text = corpus.lower) %>% 
  mutate(the = str_detect(text, pattern = "the"),
         and = str_detect(text, pattern = "and"),
         be = str_detect(text, pattern = "be"),
         sum_top_3 = the + and + be) %>% 
  group_by( sum_top_3, the, and, be) %>% 
  summarise(appearance = n()) %>% 
  ungroup() %>% select(sum_top_3, appearance) %>% 
  group_by(sum_top_3) %>% 
  summarise(appearance_ct = sum(appearance)) %>% 
  ungroup()


# Exercise 3

#create new object clean.corpus
corpus.clean <- corpus %>% 
  str_to_lower() %>% 
  str_remove_all("[:punct:]") %>% 
  str_remove_all("[:digit:]") %>% 
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
  mutate(`% coverage` = (count/sum(count))*100)

## How many different words were found in corpus
nrow(corpus.words)

## How much text is covered by the most frequent word
### 5.75%

## How much text is covered by the top 10 most frequent words
corpus.words %>% top_n(n=10) %>% 
  pull(`% coverage`) %>% 
  sum()
### 22.46%

## how many words to we need to cover 50% of corpus
per <- 0
i <- 1
while(per<=50){
  per <- per + corpus.words$`% coverage`[i]
  print(paste("the percentage is", per, "at word", i))
  i <- i+1
}

## how about 70% 
per <- 0
i <- 1
while(per<=70){
  per <- per + corpus.words$`% coverage`[i]
  print(paste("the percentage is", per, "at word", i))
  i <- i+1
}

## how about 90% 
per <- 0
i <- 1
while(per<=90){
  per <- per + corpus.words$`% coverage`[i]
  print(paste("the percentage is", per, "at word", i))
  i <- i+1
}

## Markos Answer
corpus.words
corpus.words %>% 
  mutate(`cum % coverage` = cumsum(`% coverage`)) %>% 
  print(n=100)


# Exercise 4

## Create table corpus.words.top100
corpus.words.top100 <- corpus.words %>% slice_head(n=100)

## Sample 10000 rows from the top 100 rows with replacement 
## and with the probability they appear in the data set
set.seed(123)
corpus.sample <- corpus.words.top100 %>% sample_n(size = 10000, weight = `% coverage`, replace = T)

## convert words in corpus.words into factor type
corpus.sample <- corpus.sample %>% 
  mutate_at(.vars = "word", .funs = as_factor)

## extract unique factor level occurrence using function from forcats package
corpus.sample %>% .$word %>% fct_unique()

## Count each factor level occurance using fucntion in forcats package
corpus.sample %>% .$word %>% fct_count() %>% 
  arrange(desc(n))

num2

