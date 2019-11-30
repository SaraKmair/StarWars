---
  title: "StarWars"
author: "Sara Kmair"
date: "11/23/2019"
output:
  pdf_document: default
html_document: default
---
  

library(Rcpp)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(tidytext)
library(stringr)
library(ggthemes)



#reading the data 
eps4 <- read.table("SW_EpisodeIV.txt")
eps5 <- read.table("SW_EpisodeV.txt")
eps6 <-  read.table("SW_EpisodeVI.txt") 

str(eps4) 

str(eps5)

str(eps6)

#changing dialogue data type to character 

eps4$dialogue <- as.character(eps4$dialogue)
eps5$dialogue <- as.character(eps5$dialogue)
eps6$dialogue <- as.character(eps6$dialogue)
```


#Replacing character Threepio with C-3PO  
levels(eps4$character) <- c(levels(eps4$character), "C-3PO")
eps4$character[which(eps4$character == "THREEPIO")] <-  as.factor("C-3PO")

levels(eps5$character) <- c(levels(eps5$character), "C-3PO")
eps5$character[which(eps5$character == "THREEPIO")] <-  as.factor("C-3PO")

levels(eps6$character) <- c(levels(eps6$character), "C-3PO")
eps6$character[which(eps6$character == "THREEPIO")] <-  as.factor("C-3PO")



#eps6 has the highest number of characteres 60

starwars <- rbind(eps4, eps5, eps6)
str(starwars)
```


#The top ten character with highest count of scenes in all episodes 
top10 <- starwars %>%
  group_by(character) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  slice(1:10)  #order the first 10 in descinding order 
ggplot(top10, aes(reorder(character, +freq), y = freq)) +
  geom_col(fill = c("grey",  "grey","green", "grey", "grey", "red", "grey", "grey", "goldenrod2", "blue" ), col = "grey20") +
  coord_flip() +
  labs(x = "", y = "Frequency", title = "Top 10 characters") +
  theme_economist()+
    geom_text(aes(label = freq, y = freq / 2))



#The top ten character in episode 4
top4 <- eps4 %>%
  group_by(character) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  slice(1:10)  #order the first 10 in descinding order 
ggplot(top4, aes(reorder(character, +freq), y = freq)) +
  geom_col(fill = c("grey",  "grey","grey", "red", "grey", "grey", "grey", "grey", "blue", "goldenrod2" ) , col = "grey20") +
  coord_flip() +
  labs(x = "", y = "Frequency", title = "A new hope top 10 characters") +
  theme_economist() +
    geom_text(aes(label = freq, y = freq / 2))


#The top ten character in episode 5
top5 <- eps5 %>%
  group_by(character) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  slice(1:10)  #order the first 10 in descinding order 
ggplot(top5, aes(reorder(character, +freq), y = freq)) +
  geom_col(fill = c("grey",  "grey","grey", "green", "red", "grey", "goldenrod2", "grey", "blue", "grey" ) , col = "grey20") +
  coord_flip() +
  labs(x = "", y = "Frequency", title = "The Empire Strikes Back top 10 characters") +
  theme_economist() +
    geom_text(aes(label = freq, y = freq / 2))


#The top ten character in episode 6
top6 <- eps6 %>%
  group_by(character) %>%
  summarise(freq = n()) %>% 
  arrange(desc(freq)) %>% 
  slice(1:10)  #order the first 10 in descinding order 
ggplot(top6, aes(reorder(character, +freq), y = freq)) +
  geom_col(fill = c("grey",  "grey","grey", "grey", "grey", "red", "grey","goldenrod2", "blue", "grey" ) , col = "grey20") +
  coord_flip() +
  labs(x = "", y = "Frequency", title = "Return of the Jedi top 10 characters ") +
  theme_economist() +
    geom_text(aes(label = freq, y = freq / 2))

#Extracting the dialogue attribute 
dialogue <- starwars$dialogue
head(dialogue)

#changing it to corpus for easier text analysis 
dia.cor <- VCorpus(VectorSource(dialogue))
dia.cor <- tm_map(dia.cor, content_transformer(tolower))
dia.cor <- tm_map(dia.cor, removeWords, stopwords("english"))
dia.cor <- tm_map(dia.cor, removeWords, c("sir", "master", "force", "lord"))
dia.cor <- tm_map(dia.cor, removePunctuation) #remove the punctuations

dia.cor <- tm_map(dia.cor, stemDocument) #Stem words in a text document 
dia.cor <- tm_map(dia.cor, removeNumbers)
dia.cor <- tm_map(dia.cor, stripWhitespace)#remove white spaces at the beginning and end not spaces between words
#removing the stopwords

head(dia.cor)

#putting all our data in a matrix  
#and count how many times the word occure in each document 
td.mat <- TermDocumentMatrix(dia.cor)

#converting from term document matrix to matrix 
td.mat <- as.matrix(td.mat)


#finding the total frequency of a word
wordcount <- rowSums(td.mat)
wordcount <- sort(wordcount, decreasing = T) #The highest word frequency 

#top 50 wordcounts 
wc <- wordcount[1:50]

#converting it to dataframe because wordcloud2 only deals with dataframes 
wc <- as.data.frame(wc)
wc$count <- wc$wc
wc$word <- names(wordcount[1:50])
wc.df <- subset(wc, select = c(word, count))
str(wc.df)
wc.df$word <- as.factor(wc.df$word)

#wordcloud visualization 
wordcloud(wc.df$word, wc.df$count, max.words=100, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))


#converting to data frame word count 
wordcount.df <- as.data.frame(wordcount)

#saving star wars file 
file <- write.table(starwars, file = "starwars.txt")


#unnest_tokens Split a column into tokens using the tokenizers package, splitting the table into one-token-per-row.
tidy.starwars <- starwars %>%
  unnest_tokens(word, dialogue) %>% #Break the dialogue into individual word 
  filter(nchar(word) > 2, word != "lord", word != "sir", word !=  "master", word != "force") %>% #words that are less than 3 letterse like in, on.. and some other words I don't want to include in my analysis 
  anti_join(stop_words)



#after tokenizing it's time to count the words in the dialogue and get the sentiments of the words either they are negative or positive words (using bing)
bing.count <- tidy.starwars %>%
  inner_join(get_sentiments("bing")) %>%
  count( word, sentiment, sort = T) 


#Top 10 positive vs negative words in the dialogue 
bing.count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Top 10 positive vs negative words in the dialogue ",
       x = "") +
  coord_flip() +
  theme_economist()

#after tokenizing it's time to count the words in the dialogue and get the sentiments of the words either they are negative or positive  or other feelings (using nrc)
nrc.count <- tidy.starwars %>%
  inner_join(get_sentiments("nrc")) %>%
  count( word, sentiment, sort = T) %>%
  ungroup()

#Top 10 used words based on the feeling 
#positive/negative 
nrc.count %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "", y = "Top 10 positive vs negative words in the dialogue ") +
  coord_flip() +
  theme_economist()
#other emotions 
tidy.starwars %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = T, character) %>%
  mutate(word = reorder(word, n)) %>%
  filter(character %in% c("LUKE","HAN","THREEPIO", "LEIA", "VADER")) %>%
  ggplot(aes(sentiment, n, fill = character))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~character, scales = "free_y") +
  labs(x = "", y = "") +
  coord_flip() +
  theme_economist()


#positive vs negative based on the character 
tidy.starwars %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = T, character) %>%
  mutate(word = reorder(word, n)) %>%
  filter(character %in% c("LUKE","HAN","THREEPIO", "LEIA", "VADER")) %>%
  ggplot(aes(character, n, fill = sentiment)) +
  geom_col(show.legend = T, position = "dodge") +
  labs(x = "", y = "") +
  coord_flip() +
  theme_economist()

#other emotions based on the character 
tidy.starwars %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = T, character) %>%
  mutate(word = reorder(word, n)) %>%
  filter(character %in% c("LUKE","HAN","THREEPIO", "LEIA", "VADER", "BEN", "LANDO", "YODA", "EMPEROR", "RED LEADER")) %>%
  ggplot(aes(character, n, fill = sentiment)) +
  geom_col(show.legend = T) +
  labs(x = "", y = "") +
  coord_flip() +
  theme_economist()




