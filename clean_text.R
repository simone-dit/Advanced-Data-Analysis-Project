library(dplyr)
library(magrittr)
library(openNLP)
library(stringr)
library(tm)

#reading tables
aliases = read.csv('Aliases.csv', stringsAsFactors=F)
email.receivers = read.csv('EmailReceivers.csv', stringsAsFactors=F)
emails = read.csv('Emails.csv', stringsAsFactors=F)
persons = read.csv('Persons.csv', stringsAsFactors=F)

clean.emails <- data.frame(ExtractedBodyText=emails$ExtractedBodyText)
tm.clean.emails <-
    Corpus(DataframeSource(clean.emails)) %>%
        tm_map(content_transformer(. %>% str_replace_all('\n', ' '))) %>%
        tm_map(content_transformer(. %>% str_replace_all('[^0-9a-zA-Z!@#$%^&*()_,.+=-\\[\\]<>:;\'\"~?/|{} ]', ''))) %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(stripWhitespace) %>% 
        tm_map(removeWords, stopwords("english")) %>%
        tm_map(stemDocument)
text <- lapply(1:nrow(clean.emails), . %>% `[[`(tm.clean.emails, . ) %>% as.character)
