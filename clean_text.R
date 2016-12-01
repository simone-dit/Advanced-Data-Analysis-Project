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

doc2wordvec <- function(str) { sapply(split.sentences(str), sentence.cleaner, simplify=F) } 

ex.doc <- "I hate running@@@@ for president!!!!!!. I love love running!"
ex.wordvecs <- doc2wordvec(ex.doc)

vec2bow <- function(word.vec) {
    print(word.vec)
    bow <- list()
    for (i in word.vec) {
        if (i %in% names(bow))
            bow[[i]] <- bow[[i]] + 1
        else
            bow[[i]] <- 1
    }
    bow
}

ex.bows <- sapply(ex.wordvecs, vec2bow)

bow.union <- function(bow1, bow2) {
    bow3 <- modifyList(bow1, bow2)
    common.words <- intersect(names(bow1), names(bow2))
    for (w in common.words) {
        bow3[[w]] <- bow1[[w]] + bow2[[w]]
    }
    bow3
}

bow.intersect <- function(bow1, bow2) {
    bow3 <- list()
    common.words <- intersect(names(bow1), names(bow2))
    for (w in common.words) {
        bow3[[w]] <- min(bow1[[w]], bow2[[w]])
    }
    bow3
}
