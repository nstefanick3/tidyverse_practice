#install.packages("tidyverse")
library(tidyverse)

str_length(c("pineapple", "pear", "apple", "banana"))

length("a'a'b")
length(c("a","b"))

rm(list = ls())
graphics.off()

#####################################################

# 3.1 strings inside tidyverse

s1 <- "string"
s2 <- 'string'
s3 <- 'st"ri"ng' #enters escape character
s4 <- "st'ri'ng" # does not input escape character

vec <- c("a", "b", "c")
vec

#charachter vector inside tibble
df <-  tibble(lett3rs = vec)
df

#special characters 
##escape characters

"\n" #newline
"\t" #tab
"\u03B1" # alpha

#see raw content of your string
s <- "string"
s
writeLines(s)

s <- "\""
writeLines(s)

fruit <- c("pineapple", "pear", "apple", "banana")
str_detect(fruit, "e")
str_which(fruit, "e") # list of locations in vector where string exists 
str_count(fruit, "e")
str_locate(fruit,"e")
str_locate_all(fruit,"e")


########################################################################

load("./data/strings/strings.Rdata") #load .Rdata from file saved in project
ls() # lists data in environment

# str_detect() - detect a pattern
# similar to base R: grepl()

## find a fruit containing letter "a" anywhere
fruit
sbset <- str_detect(fruit, "a")
fruit[sbset]

sbset <- str_which(fruit, "a")
fruit[sbset]

#old way 
grepl(pattern = "a", x = fruit)
fruit[grepl(pattern = "a", x = fruit)]

#find a fruit not containing letter a
sbset <- str_detect(fruit, "a")
fruit[!sbset]

#or
str_detect(fruit, "a", negate = T)

# in tibble detect and throw flag if fruit contains letter a
fruit.df %>% 
  mutate(flag = case_when(str_detect(fruit, "a") ~ "Contains a",
                          T ~ "Does not contain a"))

# str_which() - detects a pattern and returns index position
# base r: grep()
str_which(string = fruit, pattern = "a")

# str_count() - counts number of times pattern appears
str_count(fruit, "a")

fruit.df1 <- fruit.df %>% 
  mutate(count_a = str_count(fruit, "a")) %>% 
  arrange(desc(count_a))

#show me all fruits containing letter a 3 times
fruit.df1 <- fruit.df %>% 
  mutate(count_a = str_count(fruit, "a")) %>% 
  arrange(desc(count_a)) %>% 
  filter(count_a >= 3)

#str_locate() and str_locate_all()
#locate place of first postion of a
str_locate(fruit,"a")

fruit.df1 <- str_locate(fruit, "a") %>% 
  as_tibble() %>% 
  mutate(fruit = fruit) %>% 
  select(fruit, start, end)

#location of all postions of a's
str_locate_all(fruit, "a")

#moved develop further along

#moved develop further along again

#trying to push to remote develop through r

#now i fixed the connection between local develop and remote develop, lets
#make changes and push to remote develop

# 3.3 String subsetting

# str_sub()
# similar: substr()

##extract first 3 letters of a fruit 
library(tidyverse)
str_sub(fruit, start = 1, end = 3)

##extract  first letter of common word and count word freq
getwd()
load(file.path("./data/strings/strings.RData"))

## extract first letter of common word and count word frequency by first word letter
words %>% as_tibble() %>% 
  mutate(f_letter = str_sub(value, start = 1, end = 1)) %>% 
  group_by(f_letter) %>% 
  summarise(ct = n()) %>% 
  arrange(desc(ct)) %>% 
  print(n=2*26)

words.df %>% 
  mutate(flet = str_sub(word,1,1)) %>% 
  count(flet)

##extract middle part of the word 
str_sub(words, 3,5)

#extract word starting from the end
str_sub(fruit, -3, -1)

#str_subset()

## return fruit containing letter c

str_subset(fruit,"c")
#the above is and easier way of doing the below
fruit[str_detect(fruit,"c")]

#str_extract() and str_extract_all()
str_extract(fruit, pattern = "a")
str_extract_all(fruit, pattern = "a") %>% head()

#str_match() and str_match_all()
str_match_all(fruit, pattern = "a") %>% head()

str_trunc(fruit, 7, ellipsis = "..")

rm(list = ls())
load(file.path("./data/strings/strings.RData"))

##length of fruit names 
str_length(fruit)

##get fruits with 10 or more charachters in fruit name
fruit[str_length(fruit)>10]

##pad lenghts of stings with x's on the left side
str_pad(fruit,width=20,side="both",pad="x")

str_trunc(fruit, 7, ellipsis = "..")
str_trunc(fruit, 7, side = "center", ellipsis = "..")

ws <- c("  kjdsf  ",
        "fadf adf",
        " ljsdflk",
        "dfajdsf    ",
        "df kjdf ads h  ")
str_trim(ws,"left")
str_trim(ws,"both")


str_replace_all(fruit,"p","f")

#3.5 stings mutating

#str_sub()
## replace first three letters 
fru.sub <- fruit
str_sub(fru.sub, 1, 3) <- "F"
fru.sub

str_replace(fruit, "a", "A")
str_replace_all(fruit, "a", "A")

str_to_upper("theje ")
str_to_title("this is a tst string")

#' joining and splitting string 
#' str_c concatinates strings that are spleit up
#' str

#str_c() join multiple strings 
##split a vector fruit into 4 equal in sixe smaller vectors 
fruit1 <- fruit[1:20]
fruit2 <- fruit[21:40]
fruit3 <- fruit[41:60]
fruit4 <- fruit[61:80]

#create one vector using 4 smaller vectors
str_c(fruit1,fruit2,fruit3,fruit4, sep = "-")

#create a vector of alphabet letter
letters
Letters
str_c(letters, Letters, sep= " / ")

# repeat string multiple times 
stringss <- "dop"
str_dup(stringss, 5)

#repeat a vector of stings 2 times 
str_dup(fruit[1:5], 2)

#split a vector of strings into a matrix based on pattern you selected 

##split fruit by " " whitespace
str_split_fixed(fruit, pattern = " ", n = 2)
str_split_fixed(str_c(fruit, " jam"), pattern = " ",n = 2)
str_split_fixed(str_c(fruit, " jam"), pattern = " ",n = 3)

#split first five sentances by whitespaces 
sentences
str_split_fixed(sentences, pattern = " ", n = 5)


#split a vector of strings into a list or matrix based of substrings based on the pattern 

## split first five sentences by
str_glue("What is the value of sqrt(2), this is {round(sqrt(2), 3)}")

#merge fixed string 
name <- "nate"
str_glue("hi my name is {name}")

# use df list or environment to create strings from string expressions 

##merge string and values from a df
mtcars
str_glue_data(mtcars, "The car is {rownames(mtcars)}, hp: {hp}")

str_view_all(c("mary had a little lamb", "banana"),"a")

# 3.7 other string helper functions
#str_order()
#lets shuffle fruits
set.seed(123)
s_fruit <- sample(fruit,length(fruit),replace = F)
s_fruit
str_order(s_fruit)
#to sort
s_fruit[str_order(x=s_fruit)]

#same thing
str_sort(s_fruit)

num.s <- sample(1:200, size=20, replace = F)
num.s <- as.character(num.s)
num.s
str_sort(num.s, numeric = T)


str_view_all(fruit,"^a", html = T, match = T)



## 3.8 REGEX

# list of special characters
?"'"

# escaping paradox
string <- c("string", "word", "letter", "word.letter", "charachter\letter")

##match "tr"
str_view(string, "tr", html = T, match = T)

##match any charachter before t and after t
str_view(string, ".t.", html = T, match = T)

##match "." and not as a special charachter
## 1) wrong way : since . is interpreted as a special charachter
str_view(string, ".", html = T, match = T)

## 2) wrong way : use single back slash, but backslash is not escaped
str_view(string, "\.", html = T, match = T)

## 3) correct way : double backslash, escape both backslash and .
str_view(string, "\\.", html = T, match = T)

#Write backslash
writeLines("\\")

str_view("\\","\\\\") #double escaping is applied in the pattern ~four in total at the end 

#REGEX: special charachters and classes

#Digits
string <- c(letters, "123", "1-5-6", "598642")
string

#find a string with digits
str_subset(string, pattern = "\\d")
str_view_all(string, "\\d", match = T)

## find strings without digits
str_subset(string, pattern = "\\D")
str_view_all(string, "\\D", match = T) %>% print(n=length(string))

##strings with pattern <digit>-<digit>-<digit>
str_subset(string, pattern = "\\d-\\d-\\d")
str_view_all(string, "\\d-\\d-\\d", match = T) %>% print(n=length(string))

## locate whitespaces
set.seed(123)
string <- c(sample(sentences,5),
            sample(fruit, 5),
            sample(words,5),
            "This is a \nnewline",
            "string with a tab \t")
string
writeLines(string)

## locate string with white space(S) " "
str_subset(string, "\\s")
str_view_all(string, "\\s", match = T) %>% print(n=length(string))

#locate strings with newlines or tabs
str_subset(string, "\\n")
str_subset(string, "\\t")

##different classes 
string <- c("123abc", "abc", "123", ",.?", "ABC", "\nABC", "\tABC")

#digits
str_subset(string, "[:digit:]")

##letters
str_subset(string, "[:alpha:]")

# REGEX: alternate, anchors, and groups

# anchors

## word that starts with lowercase letter a
str_subset(words,"^a")
str_view_all(words, "^a", match = T)

#words that end with letter a
str_subset(words,"a$")
str_view_all(words, "a$", match = T)

#exact words
str_subset(words,"^actor$")
str_view_all(words, "^actor$", match = T)
str_subset(words,"^lemon")
str_view_all(words, "^lemon", match = T)

# alternates

## find words that start with "af" or "ag"
str_subset(words, "^af|^ag")
str_view_all(words, "^af|^ag", match = T)

#find words containing x or y or z
str_view_all(words, "[xyz]", match = T)

#find words not containing letters from a to y
str_view_all(words %>% str_to_lower(), "[^[a-y]]", match = T)

## find all country names beggining with letter "A" or "E"
str_view_all(countries, "^A|^E", match = T)

## find all countries that end with a or e
str_subset(countries, "a$|e$")


# Groups

## find all sentences that include the words: "the", "a" or "an"
str_view_all(sentences, "(\\sthe\\s)|(\\sa\\s)|(\\san\\s)", match = T)

## find words with repeated pairs of letters: use back references
str_view_all(words, "(..)\\1", match = T) #\1 means first group, double bakcslash is escape 

## more than one group in back reference 
string <- c("abc", "abcabc", "ababcc", "abababccc")

str_view_all(string, "(a)(b)", match = T)
str_view_all(string, "(a)(b)\\1", match = T)
str_view_all(string, "(a)(b)\\1\\2", match = T)
#above is equivolent to
str_view_all(string, "(a)(b)(a)(b)", match = T)

# REGEX: Lookarounds and quanitfiers

## find a word where letter w is followed by letter a
str_subset(words, "w(?=a)")
str_view_all(words, "w(?=a)", match = T) # matches just the w
str_view_all(words, "wa", match = T) #matches w and a

## find a word where letter w is NOT followed by letter a
str_subset(words, "w(?!a)")
str_view_all(words, "w(?!a)", match = T)

## find a word "a" where there is a preceding w "w"
str_subset(words, "(?<=w)a")
str_view_all(words, "(?<=w)a", match = T)

## find a word "a" where there is a NOT preceding w "w"
str_subset(words, "(?<!w)a")
str_view_all(words, "(?<!w)a", match = T)

#quantifiers
string <- " .A.AA.AAA.AAAA"

#0 or 1 capital A
str_view_all(string, "A?", match = T)

#0 or more A
str_view_all(string, "A*", match = T)

#one or more A
str_view_all(string, "A+", match = T)

#exactly 2 times A
str_view_all(string, "A{2}", match = T)

#2 or more times A
str_view_all(string, "A{2,}", match = T)

#2 or 3 times A
str_view_all(string, "A{2,3}", match = T)

#Exercise with sentances
##count the number of words in each sentance
##first remove punctuation and convert to lowercase 
##then count number of words to show results

sentences.df1 <- sentences.df %>% 
  mutate(sentence = str_remove_all(sentence, "[:punct:]"), #remove punctuation
         sentence = str_to_lower(sentence)) %>%  #
  mutate(num_words = str_count(string = sentence, pattern = "\\s+") + 1) # number of spaces plus 1 is number of words

sentences.df1 %>% count(num_words)

#countries with more than 3 words in the country name
countries.df %>% 
  mutate(num_words = str_count(country, "\\s") + 1 ) %>% 
  filter(num_words >3)
 

## 3.13 Forcats 
library(tidyverse)

# Convert some parts of the mpg dataframe into factors
mpg
df <- mpg %>% 
  mutate_at(.vars = c("manufacturer", "model", "trans", "class"), 
            .funs = forcats::as_factor)
df

#check levels
df$manufacturer %>% levels()





