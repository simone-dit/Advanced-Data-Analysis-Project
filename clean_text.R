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

sent.token.ann <- Maxent_Sent_Token_Annotator()
word.token.ann <- Maxent_Word_Token_Annotator()

# get rid of empty emails, only look at ID and extracted text
unclean.text <- filter(emails[,c("Id", "ExtractedBodyText")], nchar(ExtractedBodyText) > 0)

unclean.sample <- sample_n(unclean.text, 20)

sentence.cleaner <-
    . %>%
 tolower %>%
 removeWords(stopwords("english")) %>%
 gsub("[^[:alnum:] ]", "", .) %>%
 strsplit(x=., split=" ") %>% '[['(1) %>%
 Filter(function(x) { nchar(x) > 0 }, .) %>%
 stemDocument


split.sentences <- function(char.ls) {
    sent.ann.df <- as.data.frame(annotate(char.ls, sent.token.ann))
    sents <- c()
    for (i in 1:nrow(sent.ann.df)) {
        sents <- c(sents, substr(char.ls, sent.ann.df$start[i], sent.ann.df$end[i]))
    }
    sents
}

bow.union <- function(bow1, bow2) {
    
}

bow.intersect <- function(bow1, bow2) {
    
}

vec2bow <- function(word.vec) {
    
}
