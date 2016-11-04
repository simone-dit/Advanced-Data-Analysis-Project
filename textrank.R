library(tm)
library(NLP)
library(openNLP)

example.string <- "Back when I first started this series of posts on stochastic calculus, the aim was to write up the notes which I began writing while learning the subject myself. The idea behind these notes was to give a more intuitive and natural, yet fully rigorous, approach to stochastic integration and semimartingales than the traditional method. The stochastic integral and related concepts were developed without requiring advanced results such as optional and predictable projection or the Doob-Meyer decomposition which are often used in traditional approaches. Then, the more advanced theory of semimartingales was developed after stochastic integration had already been established. This now complete! The list of subjects from my original post have now all been posted. Of course, there are still many important areas of stochastic calculus which are not adequately covered in these notes, such as local times, stochastic differential equations, excursion theory, etc. I will now focus on the projection theorems and related results. Although these are not required for the development of the stochastic integral and the theory of semimartingales, as demonstrated by these notes, they are still very important and powerful results invaluable to much of the more advanced theory of continuous-time stochastic processes. Optional and predictable projection are often regarded as quite advanced topics beyond the scope of many textbooks on stochastic calculus. This is because they require some descriptive set theory and, in particular, some understanding of analytic sets. The level of knowledge required for applications to stochastic calculus is not too great though, and I aim to give complete proofs of the projection theorems in these notes. However, the proofs of these theorems do require ideas which are not particularly intuitive from the viewpoint of stochastic calculus, and hence the desire to avoid them in the initial development of the stochastic integral. The theory of semimartingales and stochastic integration will not used at all in the series of posts on the projection theorems, and all that will be required from these stochastic calculus notes are the initial posts on filtrations and processes. I will also mention quasimartingales, although only the definition and very basic properties will be required."

example.string.short <- "Back when I first started this series of posts on stochastic calculus, the aim was to write up the notes which I began writing while learning the subject myself. The idea behind these notes was to give a more intuitive and natural, yet fully rigorous, approach to stochastic integration and semimartingales than the traditional method."

sent.token.ann <- Maxent_Sent_Token_Annotator()
word.token.ann <- Maxent_Word_Token_Annotator()
sentence.splitter <- function(str) {
    sent.ann <- annotate(str, sent.token.ann)
    word.ann <- annotate(str, word.token.ann, sent.ann)
    word.ann.df <- data.frame(type=word.ann$type, start=word.ann$start, end=word.ann$end)
    word.ann.df <- word.ann.df[order(word.ann.df$start),]
    #return(word.ann.df)
    sentences <- c()
    word.bags <- list()
    word.bag <- list()

    for (i in 1:nrow(word.ann.df)) {
        if (word.ann.df[i, "type"]=="sentence") {
            if (i != 1) {
                word.bags[[length(word.bags)+1]] <- word.bag
            }
            sentences <- c(sentences, substr(str, word.ann.df[i,"start"], word.ann.df[i,"end"]))
            word.bag <- list()
        } else {
            w <- substr(str, word.ann.df[i,"start"], word.ann.df[i, "end"])
            if (w %in% names(word.bag)) {
                word.bag[[w]] <- word.bag[[w]] + 1
            } else {
                word.bag[[w]] <- 1
            }
        }
    }
    word.bags[[length(word.bags)+1]] <- word.bag
    list(bags=word.bags, sent=sentences, word.ann=word.ann.df)
}

bag.similarity <- function(b1, b2) {
    numer <- length(intersect(names(b1), names(b2)))
    denom <- log(length(names(b1))) + log(length(names(b2)))
    numer/denom
}

get.transitions <- function(split) {
    n.sent <- length(split$bags)
    transition.matrix <- matrix(0, nrow=n.sent, ncol=n.sent)
    for (i in 1:length(split$bags)) {
        for (j in 1:length(split$bags)) {
            transition.matrix[i,j] <- bag.similarity(split$bags[[i]],
                                                     split$bags[[j]])
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

example.split <- sentence.splitter(example.string)
example.tm <- get.transitions(example.split)
damping.factor <- 0.85
guess <- rep(1/15, 15)
importance <- compute.pagerank(example.tm, damping.factor, 20)
summaries <- example.split$sent[order(-importance)]
