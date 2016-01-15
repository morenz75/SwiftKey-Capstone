#https://rpubs.com/yongjun21/CapstoneProjectMilestoneReport
#https://rstudio-pubs-static.s3.amazonaws.com/41119_680504f74ff3490d917feba5a4573381.html
#http://web.mit.edu/6.863/www/fall2012/readings/ngrampages.pdf
#https://class.coursera.org/nlp/lecture/129

load("../../data/train.Rdata")
load("../../data/final.Rdata")
#Dati senza ngram con 1 sola occorrenza
load("../../data/train1.Rdata")
setwd("~/GitHub/Data Science Capstone/code/raw")
head(df_unigrams[df_unigrams$topNextWord =="",])
require("tm")
#library("quanteda")
require("slam")
require("ggplot2")
require("RWeka")
require("stringr")

con <- file("../../data/final/en_US/en_US.blogs.txt", "r", blocking = FALSE)
blogs <- readLines(con = con, n = 150000, encoding = "UTF-8", skipNul = TRUE)
close(con)

con <- file("../../data/final/en_US/en_US.news.txt", "r", blocking = FALSE)
news <- readLines(con = con, n = 200000, encoding = "UTF-8", skipNul = TRUE)
close(con)

con <- file("../../data/final/en_US/en_US.twitter.txt", "r", blocking = FALSE)
twitter <- readLines(con = con, n = 400000, encoding = "ASCI", skipNul = TRUE)
close(con)
rm(con)

train <- c(blogs, news, twitter)
rm(blogs)
rm(news)
rm(twitter)
gc(reset=TRUE)

train <- gsub("don't", "do not", train, ignore.case = TRUE)
train <- gsub("doesn't", "does not", train, ignore.case = TRUE)
train <- gsub("can't", "cannot", train, ignore.case = TRUE)
train <- gsub("I'll", "I will", train, ignore.case = TRUE)
train <- gsub("won't", "will not", train, ignore.case = TRUE)
train <- gsub("didn't", "did not", train, ignore.case = TRUE)
train <- gsub("e's", "e is", train, ignore.case = TRUE)
train <- gsub("isn't", "is not", train, ignore.case = TRUE)
train <- gsub("i'm", "i am", train, ignore.case = TRUE)
train <- gsub("it's", "it is", train, ignore.case = TRUE)

vectorS <- VectorSource(train)
corpusT <- Corpus(vectorS)


myTokenizer <- function (x) {
    ret <- x$content
    #ret <- gsub("[âð¤'ãåïç]", " ", ret)
    ret <- unlist(strsplit(ret, " "))
    # remove non-printable char
    #ret <- gsub('[^[:print:]]|[[:blank:]]','', ret)
    #ret <- gsub("^[[:punct:]]|[[:punct:]]$", "", ret)
    # remove non-printable char and punctuation
    #ret <- gsub("[^[:print:]|[:print:]$]|[!\"#$%&()½¼¾*¿/+¬,-.ö¸//:;<=#>?@^_`{|}°~¯£¢]|\\[|\\]|[0-9]", "", ret)
    ret <- gsub("[^a-zA-zèòàùèéùì]|\\[|\\]|\\\\|_|\\^|-|`", "", ret)
    return(ret[which(ret != "")])
}

SimpleGT <- function(table_N){
    
    #Simple Good Turing Algorithm - Gale And Simpson
    #Good Turing Smoothing
    
    # table_U is a table of frequency of frequencies
    # The frequencies are stored as names of the list in the table structure
    # the values are the frequency of frequencies.
    # In Good Turing Smoothing, we are concerned with the frequency of frequencies
    # So, to extract the number of times that words of frequency n occur in the training set, we need to do:
    # table(freq_B)[[as.character(pairCount)]]
    # In a tables with a number of holes or non-contiguous sequence of frequency of words,
    # we can compute N(c+1) as the mean of all frequencies that occur more than Nc times
    # to do this, create a vector that is in the numerical form of the names of the table
    
    # create a data table
    # r is the frequencies of various trigrams
    # n is the frequency of frequencies
    
    SGT_DT <- data.frame(r=as.numeric(names(table_N)),n=as.vector(table_N),Z=vector("numeric",length(table_N)), 
                         logr=vector("numeric",length(table_N)),
                         logZ=vector("numeric",length(table_N)),
                         r_star=vector("numeric",length(table_N)),
                         p=vector("numeric",length(table_N)))
    #p=vector("numeric",length(table_N)),key="r")
    
    #str(SGT_DT)
    
    num_r <- nrow(SGT_DT)
    for (j in 1:num_r) {
        if(j==1) {r_i<-0} else {r_i <- SGT_DT$r[j-1]}
        if(j==num_r){r_k<-SGT_DT$r[j]} else {r_k <- SGT_DT$r[j+1]}
        SGT_DT$Z[j] <- 2*SGT_DT$n[j] / (r_k-r_i)
        #print(paste(r_i,j,r_k))
    }
    SGT_DT$logr <- log(SGT_DT$r)
    SGT_DT$logZ <- log(SGT_DT$Z)
    linearFit <- lm(SGT_DT$logZ ~ SGT_DT$logr)
    #print(linearFit$coefficients)
    c0 <- linearFit$coefficients[1]
    c1 <- linearFit$coefficients[2]
    
    #plot(SGT_DT$logr, SGT_DT$logZ)
    #abline(linearFit,col="red")
    
    use_y = FALSE
    for (j in 1:(num_r-1)) {
        r_plus_1 <- SGT_DT$r[j] + 1
        
        s_r_plus_1 <- exp(c0 + (c1 * SGT_DT$logr[j+1]))
        s_r <- exp(c0 + (c1 * SGT_DT$logr[j]))
        y<-r_plus_1 * s_r_plus_1/s_r
        
        if(use_y) {
            SGT_DT$r_star[j] <- y
        } else { 
            n_r_plus_1 <- SGT_DT$n[SGT_DT$r == r_plus_1]
            n_r <- SGT_DT$n[j]
            x<-(r_plus_1) * n_r_plus_1/n_r
            
            if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
                SGT_DT$r_star[j] <- x
            }else {
                SGT_DT$r_star[j] <- y
                use_y = TRUE
            }
        }
        if(j==(num_r-1)) {
            SGT_DT$r_star[j+1] <- y
        }
        
    }
    N <- sum(SGT_DT$n * SGT_DT$r)
    Nhat <- sum(SGT_DT$n * SGT_DT$r_star)
    SGT_DT$Po <- SGT_DT$n[1] / N
    SGT_DT$p <- (1-SGT_DT$Po) * SGT_DT$r_star/Nhat
    
    return(SGT_DT)
    
}


predict <- function(sentence) {
    # given a sentence/phrase, extract the last two words
    sl <- unlist(str_split(sentence," "))
    sl <- gsub("[^a-zA-zèòàùèéùì]|\\[|\\]|\\\\|_|\\^|-|`", "", sl)
    sl <- sl[which(sl != "")]
    len <- length(sl)
    print(sl)
    bigram <- paste(sl[len-1],sl[len])
    predictions <- vector()
    # get the subset of the trigram data table with a matching bigram start
    #swf_T <- wf_T[wf_T$start == bigram,]
    nextWords <- df_b[row.names(df_b) == bigram,]
    #check if bigram was found in the trigram table
    if(nrow(nextWords) > 0) {
        return(unlist(str_split(nextWords$topNextWord,",")))
    } else {
        #print(paste("No match for bigram",bigram,"in",sentence,"--looking for unigram match"))
        unigram <- sl[len]
        nextWords <- df_u[row.names(df_u) == unigram,]
        if(nrow(nextWords) > 0) {
            return(unlist(str_split(nextWords$topNextWord,",")))
        } else {
            return(head(row.names(df_u),3))
        }
    }   
}

unigramTokenizer <- function(x) {
    return(myTokenizer(x))
}

biGramTokenizer <- function(x) {
    return(NGramTokenizer(paste(myTokenizer(x), collapse = " "), Weka_control(min = 2, max = 2)))
}

triGramTokenizer <- function(x) {
    return(NGramTokenizer(paste(myTokenizer(x), collapse = " "), Weka_control(min = 3, max = 3)))
}

system.time(tdm_unigram <- TermDocumentMatrix(
    corpusT,
    control = list(
        #tokenize = scan_tokenizer,
        tokenize = myTokenizer,
        weight = weightTf,
        tolower=TRUE,
        removeNumbers = FALSE,
        wordLengths = c(1, Inf),
        removePunctuation = FALSE,
        stopwords = FALSE
    )))

unigram_freq <- row_sums(tdm_unigram, na.rm = TRUE)
unigram_freq <- sort(unigram_freq, decreasing=TRUE)
rm(tdm_unigram)
head(unigram_freq,5)
#head(which(frequency == 2),30)

barplot(head(unigram_freq/sum(unigram_freq), 10), horiz = TRUE, las = 1)

## Bigrams

#biGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
system.time(tdm_bigram <- TermDocumentMatrix(
    corpusT, 
    control = list(tokenize = biGramTokenizer,
                   weight = weightTf,
                   removeNumbers = FALSE,
                   wordLengths = c(1, Inf),
                   removePunctuation = FALSE,
                   stopwords = FALSE,
                   tolower = TRUE
    )))

bigram_freq <- row_sums(tdm_bigram, na.rm = TRUE)
bigram_freq <- sort(bigram_freq, decreasing=TRUE)
rm(tdm_bigram)
#head(bigram_freq)

barplot(head(bigram_freq/sum(bigram_freq), 10), horiz = TRUE, las = 1)

## Trigrams

#triGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
system.time(tdm_trigram <- TermDocumentMatrix(
    corpusT, 
    control = list(tokenize = triGramTokenizer,
                   weight = weightTf,
                   removeNumbers = FALSE,
                   wordLengths = c(1, Inf),
                   removePunctuation = FALSE,
                   stopwords = FALSE,
                   tolower = TRUE
    )))

trigram_freq <- row_sums(tdm_trigram, na.rm = TRUE)
trigram_freq <- sort(trigram_freq, decreasing=TRUE)
rm(tdm_trigram)
rm(corpusT)
gc()
#head(trigram_freq, 10)
barplot(head(trigram_freq/sum(trigram_freq), 10), horiz = TRUE, las = 1)


#load("train.Rdata")

getTopTrigrams <- function (bigram) {
    topTrigrams <- head(row.names(df_trigrams[df_trigrams$prior == bigram, ]),3)
    topTrigrams <- gsub(paste(bigram," ", sep = ""), "", topTrigrams)
    return(paste(topTrigrams, collapse=","))
}

getTopBigrams <- function (unigram) {
    topBigrams <- head(row.names(df_bigrams[df_bigrams$prior == unigram, ]),3)
    topBigrams <- gsub(paste(unigram," ", sep = ""), "", topBigrams)
    return(paste(topBigrams, collapse=","))
}

repTrigrams <- function (trigram) {
    ret <- unlist(strsplit(trigram, " "))
    bigram <- paste(ret[1],ret[2], sep = " ")
    nextWord <- ret[3]
    return(c(bigram, nextWord))
}

priorTrigrams <- function (trigram) {
    ret <- unlist(strsplit(trigram, " "))
    bigram <- paste(ret[1],ret[2], sep = " ")
    #nextWord <- ret[3]
    return(bigram)
}

priorBigrams <- function (bigram) {
    ret <- unlist(strsplit(bigram, " "))
    return(ret[1])
}
df_unigrams <- data.frame(word=names(unigram_freq), count = unigram_freq, stringsAsFactors = FALSE, row.names = NULL)

df_trigrams <- data.frame(row.names = names(trigram_freq),
                          #word=names(trigram_freq), 
                          count = trigram_freq, 
                          prior = sapply(names(trigram_freq), priorTrigrams, USE.NAMES = FALSE), 
                          stringsAsFactors = FALSE)

df_bigrams <- data.frame(row.names=names(bigram_freq), 
                         #word = names(bigram_freq), 
                         count = bigram_freq,
                         prior = sapply(names(bigram_freq), priorBigrams, USE.NAMES = FALSE), 
                         topNextWord = sapply(names(bigram_freq), getTopTrigrams, USE.NAMES = FALSE),
                         stringsAsFactors = FALSE)

df_unigrams <- data.frame(row.names=names(unigram_freq), 
                         #word = names(bigram_freq), 
                         count = unigram_freq,
                         topNextWord = sapply(names(unigram_freq), getTopBigrams, USE.NAMES = FALSE),
                         stringsAsFactors = FALSE)


uni_SGT_DT <- SimpleGT(table(unigram_freq))
bi_SGT_DT <- SimpleGT(table(bigram_freq))
tri_SGT_DT <- SimpleGT(table(trigram_freq))

rm(unigram_freq)
rm(bigram_freq)
rm(trigram_freq)
gc(reset=TRUE)

df_bigrams1 <- df_bigrams[df_bigrams$count > 1,]
df_trigrams1 <- df_trigrams[df_trigrams$count > 1,]



df_trigrams <- data.frame(word=names(trigram_freq), count = trigram_freq, prior = sapply(names(trigram_freq), priorTrigrams, USE.NAMES = FALSE),  stringsAsFactors = FALSE)
> head(df_trigrams)

for (i in 1:10000) {
    t <- repTrigrams(df_trigrams[i, 1])
    df_trigrams[i, "prior"] <- t[1]
    df_trigrams[i, "next"] <- t[2]
}

df_bigrams1$topNextWords <- lapply(df_bigrams1$word, getTopTrigrams)

for (i in 1060:1060) {
    tmp <- bigram_freq[(i*1000+1):((i+1)*1000)]
    df_bigrams1 <- data.frame(row.names=names(tmp), 
                              #word = names(bigram_freq), 
                              count = tmp,
                              prior = sapply(names(tmp), priorBigrams, USE.NAMES = FALSE), 
                              topNextWord = sapply(names(tmp), getTopTrigrams, USE.NAMES = FALSE),
                              stringsAsFactors = FALSE)
    df_bigrams <- rbind(df_bigrams, df_bigrams1)
}
system.time(df_bigrams1 <- data.frame(row.names=names(test), 
                         #word = names(bigram_freq), 
                         count = test,
                         prior = sapply(names(test), priorBigrams, USE.NAMES = FALSE), 
                         topNextWord = sapply(names(test), getTopTrigrams, USE.NAMES = FALSE),
                         stringsAsFactors = FALSE)
)

## 
## return perplexity of a given model
##
## x vector of ngram
## n N-gram model (1, 2, 3)
##

perplexity <- function (x, n) {
    p <- 1
    pt <- 1
    for (i in 1:length(x)) {
        swf_T <- df_trigrams[df_trigrams$word == x[i],]
        if (nrow(swf_T) > 0) {
            pt <- tri_SGT_DT$p[tri_SGT_DT$r==swf_T$count]
        } else {
            pt <- tri_SGT_DT$Po[1]
        }
        p <- p*pt
    }
    pp <- p^(-1/length(x))
    return(pp)
}

testphrase <- "In r/esponse to an : over-wh%elming $ nu\\mber of [ comments we s]at down and created a list of do (s) and don’t (s) – these recommendations are easy to follow and except for - adding some herbs to your rinse . So let’s get begin…"


train <- gsub("don't", "do not", testphrase, ignore.case = TRUE)
train <- gsub("doesn't", "does not", train, ignore.case = TRUE)
train <- gsub("can't", "cannot", train, ignore.case = TRUE)
train <- gsub("I'll", "I will", train, ignore.case = TRUE)
train <- gsub("won't", "will not", train, ignore.case = TRUE)
train <- gsub("didn't", "did not", train, ignore.case = TRUE)
train <- gsub("e's", "e is", train, ignore.case = TRUE)
train <- gsub("isn't", "is not", train, ignore.case = TRUE)
train <- gsub("i'm", "i am", train, ignore.case = TRUE)
train <- gsub("it's", "it is", train, ignore.case = TRUE)

vectorS <- VectorSource(train)
corpusT <- Corpus(vectorS)

trigram_test <- row_sums(tdm_trigram, na.rm = TRUE)
unigram_test <- row_sums(tdm_unigram, na.rm = TRUE)
bigram_test <- row_sums(tdm_bigram, na.rm = TRUE)

perplexity(names(trigram_test),3)

gsub("[^a-zA-zèòàùèéùì]|\\[|\\]|\\\\", "", testphrase)

#####
#####
#####
#####
barplot(cl$topWords/sum(cl$topWords),horiz=TRUE,las=1)


testf <- c("ciao `! 'paperino","pr'ova/\"j°s#","pipp[i è $ grande", "mio fratello è a bar8cellona e sta bene")
vectorS1 <- VectorSource(testf)
corpusT1 <- Corpus(vectorS1)

uniGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))


tdm_p <- TermDocumentMatrix(
    corpusT1,
    control = list(
        tokenize = triGramTokenizer,
        weight = weightTf,
        tolower=TRUE,
        removeNumbers = FALSE,
        wordLengths = c(1, Inf),
        removePunctuation = FALSE,
        stopwords = FALSE
    ))

freq_p <- row_sums(tdm_p, na.rm = TRUE)
freq_p





uniGramTokenizer(myTokenizer(corpusT1[[1]]))
uniGramTokenizer(corpusT1[[1]])


gsub("^['\"]|'$", "", word)
as.vector(unlist(sapply(corpusT, "[", "content")))
text <- unlist(sapply(corpusT, "[", "content"))
text["1.content"]

gsub("^[[:punct:]]", "", word)

#trainS <- paste(train, collapse = "\n")

#train <- enc2utf8(train)

system.time(tdm2 <- TermDocumentMatrix(
    corpusT,
    control = list(
        tokenize = scan_tokenizer,
        #tokenize = myTokenizer,
        weight = weightTf,
        tolower=TRUE,
        removeNumbers = TRUE,
        wordLengths = c(1, Inf),
        removePunctuation = TRUE,
        stopwords = FALSE
    )))



tdm_matrix <- as.matrix(tdm)
frequency <- rowSums(tdm_matrix)
frequency
gsub("[[:punct:]]", "", frase)
gsub("^['\"]|'$", "", frase)

frequency <- row_sums(tdm, na.rm = TRUE)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency,20)

frequency2 <- row_sums(tdm2, na.rm = TRUE)
frequency2 <- sort(frequency2, decreasing=TRUE)
head(frequency2,20)


system.time(corpusT <- tm_map(corpusT, content_transformer(tolower)))
system.time(corpusT <- tm_map(corpusT, stripWhitespace))
#system.time(corpusT <- tm_map(corpusT, function(x) iconv(x, to='UTF-8', sub='byte')))
#system.time(corpusT <- tm_map(corpusT, function(x) iconv(enc2utf8(x), sub = "byte")))
#system.time(corpusT <- tm_map(corpusT, tolower))
system.time(corpusT <- tm_map(corpusT, removePunctuation))
#system.time(corpusT <- tm_map(corpusT, removeWords, stopwords("english")))

system.time(dtm <- TermDocumentMatrix(corpusT))


#dtm2 <- as.matrix(dtm)
#frequency <- colSums(dtm2)
#frequency <- sort(frequency, decreasing=TRUE)
#head(frequency)

#dtm_part1 <- dtm[1:10000,]
#dtm_part1_m <- as.matrix(dtm_part1)
#num <- 100 # Show this many top frequent terms



freq <- findFreqTerms(tdm, 1)

inspect(dtm[100555:100565, 1:50])

saveRDS(corpusT,"corpusT.rds")
corpusT <- readRDS("corpusT.rds")


num <- 150009 # Show this many top frequent terms
findFrequency <- function (m) {
    rowSums(m)
}
freq <- vector(mode="numeric", length=150254)
for(i in 1:150254) {
    t <- as.matrix(tdm[i,])
    freq[rownames(t)] <- rowSums(t)
}

freq <- sort(freq, decreasing=TRUE)

uniGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm <- TermDocumentMatrix(
    corpusT, 
    control = list(tokenize = uniGramTokenizer,
                   weight = weightTf,
                   tolower=TRUE,
                   removeNumbers = TRUE,
                   minWordLength = 1,
                   removePunctuation = TRUE,
                   stopwords = FALSE,
                   tolower = TRUE
    ))

frequency <- row_sums(tdm, na.rm = TRUE)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency)


## Bigrams

biGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm2 <- TermDocumentMatrix(
                corpusT, 
                control = list(tokenize = biGramTokenizer,
                               weight = weightTf,
                               tolower=TRUE,
                               removeNumbers = FALSE,
                               minWordLength = 1,
                               removePunctuation = TRUE,
                               stopwords = FALSE,
                               tolower = TRUE
                               ))

frequency2 <- row_sums(tdm2, na.rm = TRUE)
frequency2 <- sort(frequency2, decreasing=TRUE)
head(frequency2)


## Trigrams

triGramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm3 <- TermDocumentMatrix(
    corpusT, 
    control = list(tokenize = triGramTokenizer,
                   weight = weightTf,
                   tolower=TRUE,
                   removeNumbers = FALSE,
                   minWordLength = 1,
                   removePunctuation = TRUE,
                   stopwords = FALSE,
                   tolower = TRUE
    ))

frequency3 <- row_sums(tdm3, na.rm = TRUE)
frequency3 <- sort(frequency3, decreasing=TRUE)
head(frequency3, 10)


