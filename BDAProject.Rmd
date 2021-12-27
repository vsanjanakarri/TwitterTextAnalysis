---
title: "Untitled"
author: "AU20110006_V Sanjana"
date: "09/04/2021"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(streamR)
#Sys.setenv(HADOOP_HOME="C:/hadoop-3/bin") #//Its hadooop path
#Sys.setenv(HADOOP_CMD="C:/hadoop-3/bin/hadoop.cmd") #//It's CMD path
#Sys.setenv(HADOOP_STREAMING="/home/hadoop/work/hadoop-1.1.2/contrib/streaming/hadoop-streaming-1.1.2.jar") #//It's streaming path
#Sys.setenv(JAVA_HOME="C:/Java/jdk1.8.0_251/bin") 
#Sys.setenv(JAVA_HOME="C:/Java/jdk1.8.0_251/jre")
library("devtools")
#library("rmr2")
#library("rJava")
library("HadoopStreaming")
#install.packages("C:/Users/91909/Desktop/rhdfs_1.0.8.tar.gz", repos=NULL, type="source")
#install.packages("C:/Users/91909/Desktop/rhbase_1.2.1.tar.gz", repos=NULL, type="source")
#http://www.stat.purdue.edu/~sguha/rhipe/dn/Rhipe_0.64.tar.gz
#library("Rhipe")
library("ROAuth")
#rhinit(TRUE, TRUE)
library(rtweet)
library(ggwordcloud)
```

```{r}
library(deeplr)
library(translateR)
library(dplyr)
library(tidytext)
library(ggplot2)
library(corpus)
library(tm)
library(wordcloud)
library(igraph)
library(ggraph)
```

```{r}
#> Loading required package: RCurl
#> Loading required package: bitops
#> Loading required package: rjson
#> Loading required package: ndjson

token = create_token(
  app = "AAAAAAAAAAAAAAAAAAAAAGwLNgEAAAAAFL%2FpfM1%2F0Kwzs8zGwj5xnaVqjNs%3DenVufwlGXZsgog4RPGiCkiPlV80xfXlXXbJ0T30KWSp3MyDkyu",
  consumer_key = "abb1ZpZSidfsDSJHOsPYRIYfj",
  consumer_secret = "KQkf8Xz8xMbInTT4tZO7mXGLQM3328sj2zsfPSs4lw6SYgi6BA",
  access_token = "1361707055992565761-MKBlCyDRRNjI8LlVWvEX9osJemPft2",
  access_secret = "sAvvcraZGZQGl6kcm71Pu4h4VPj9VJOMvxr78f7iVDCXa")

query <- "covid-19,corona,aid,virus,India,COVID,help,sos,shortage,crisis,shutdown,lockdown,lack"
#define stream period
streamtime <- 15 * 1
```

```{r}
#define storage file for streamed content
filename <- "stream.json"
streamdata <- stream_tweets(q = query, timeout = streamtime, file_name = filename)
#clean up links from the data
streamdata$clean_text <- gsub("http.*","",  streamdata$text)
streamdata$clean_text <- gsub("https.*","", streamdata$clean_text)
```
```{r}
#clean out punctuations 
streamdata_clean <- streamdata %>%
  dplyr::select(clean_text) %>%
  unnest_tokens(word, clean_text)
```

```{r}
streamdata_cleanwords <- streamdata_clean %>%
  anti_join(stop_words)
#count all the uniques words again 
nrow(streamdata_cleanwords)
nrow(streamdata_clean)
```


```{r}
streamdata_cleanwords %>%
  count(word,sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequency of Words",
       x = "words",
       title = "Top 10 most used words in tweets",
       subtitle = "Stop words have been removed")

```


```{r}
streamdata_cleanwords %>%
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(x = "Count",
       y = "Unique words",
       title = "Count of unique words found in tweets")
```

```{r}
bing_word_counts <- streamdata_cleanwords %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE)
bing_word_counts
```

```{r}
bing_word_counts %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Frequency of Words",
       x = "words",
       title = "Top 10 most used words in tweets")
```

```{r}
bing_word_counts %>%
  filter(n > 50) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "Contribution to sentiment")
```
```{r}
streamdata_clean %>%
  count(word) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  with(wordcloud(word, n, max.words = 100,min.freq = 10,
          random.order = FALSE,colors = brewer.pal(8, "Dark2")))

```

```{r}
sentiments_tweet <- streamdata_clean %>%  
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red2", "green3")) +
  facet_wrap(~sentiment, scales = "free_y") +
  ylim(0, 2500) +
  labs(y = NULL, x = NULL) +
  coord_flip() +
  theme_minimal()
sentiments_tweet
```
```{r}
positive <- get_sentiments("bing") %>%
  filter(sentiment == "positive")

positive
```
```{r}
streamdata_cleanwords %>%
  count(word) %>%
  inner_join(positive, by = "word") %>%
  with(wordcloud(word, n, max.words = 50,min.freq = 10,random.order = FALSE,colors = brewer.pal(8, "Dark2")))
```
```{r}
negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")
negative
```
```{r}
streamdata_clean %>%
  count(word) %>%
  inner_join(negative, by = "word") %>%
  with(wordcloud(word, n, max.words = 100,min.freq = 10,random.order = FALSE,colors = brewer.pal(8, "Dark2")))
```
```{r}
library(igraph)
library(ggraph)


bing_word_counts %>%
        filter(n >= 24) %>%
        graph_from_data_frame() %>%
        ggraph(layout = "fr") +
        # geom_edge_link(aes(edge_alpha = n, edge_width = n))
        # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
        geom_node_point(color = "darkslategray4", size = 3) +
        geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
        labs(title = "Word Network: Tweets using the hashtag - Covid",
             subtitle = "Text mining twitter data ",
             x = "", y = "")

```
```{r}
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiment during the COVID.",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
```




