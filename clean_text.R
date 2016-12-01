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
 gsub("[^[:alnum:] ]", " ", .) %>%
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

vec2bow <- function(word.vec) {
    bow <- list()
    for (i in word.vec) {
        if (i %in% names(bow))
            bow[[i]] <- bow[[i]] + 1
        else
            bow[[i]] <- 1
    }
    bow
}

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

bow.similarity <- function(bow1, bow2) {
    numer <- bow.size(bow.intersect(bow1, bow2))
    denom <- log(1+bow.size(bow1)) + log(1+bow.size(bow2))
    numer/denom
}

bow.size <- function(bow) {
    Reduce("+", bow, 0)
}

get.transitions <- function(bows) {
    n.sent <- length(bows)
    transition.matrix <- matrix(0, nrow=n.sent, ncol=n.sent)
    for (i in 1:n.sent) {
        for (j in 1:n.sent) {
            transition.matrix[i,j] <- bow.similarity(bows[[i]], bows[[j]])
        }
    }
    normalized.transition.mat <- transition.matrix /
        matrix(rep(rowSums(transition.matrix), n.sent), nrow=n.sent, ncol=n.sent)
    normalized.transition.mat
}

pagerank.iterate <- function(transitions, damp, guess) {
    n <- nrow(transitions)
    e <- rep((1-damp)/n, n)
    damp * t(transitions) %*% guess + e
}

compute.pagerank <- function(transitions, damp, itrs) {
    n <- nrow(transitions)
    init.itr <- rep(1/n, n)
    next.itr <- pagerank.iterate(transitions, damp, init.itr)
    for (i in 1:itrs) {
        init.itr <- next.itr
        next.itr <- pagerank.iterate(transitions, damp, init.itr)
    }
    next.itr
}

rank.sentences <- function(text) {
    wordvecs <- doc2wordvec(text)
    bows <- sapply(wordvecs, vec2bow)
    probs <- compute.pagerank(get.transitions(bows), 0.85, 10)
    order(-as.vector(probs))
}

doc2bow <- function(text) {
    wordvecs <- doc2wordvec(text)
    sapply(wordvecs, vec2bow)
}

ex.doc <- paste(
    "Back when;;;:/ I first@@@@ started this series of _posts on stochastic calculus, the aim was to write up the notes which I began writing while learning the subject myself.",
    "The: idea behind these notes was to give a more intuitive and natural, yet fully rigorous,approach to stochastic integration- and semimartingales than the traditional method.",
    "The stochastic integral and related concepts were developed without requiring advanced results such as optional and predictable projection or the Doob-Meyer decomposition which are often used in traditional approaches.",
    "Then, the more advanced theory of semimartingales was developed after stochastic integration had already been established.")
