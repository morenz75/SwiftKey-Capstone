load("../../data/final.Rdata")
load("../../data/ngrams.Rdata")
require("stringr")

predictTest <- function(sentence) {
    ret <- FALSE
    # given a sentence/phrase, extract the last two words
    sl <- unlist(str_split(sentence," "))
    sl <- gsub("[^a-zA-zèòàùèéùì]|\\[|\\]|\\\\|_|\\^|-|`", "", sl)
    sl <- sl[which(sl != "")]
    len <- length(sl)
    
    bigram <- paste(sl[len-2],sl[len-1])
    predictions <- vector()
    # get the subset of the trigram data table with a matching bigram start
    #swf_T <- wf_T[wf_T$start == bigram,]
    nextWords <- df_b[row.names(df_b) == bigram,]
    #check if bigram was found in the trigram table
    if(nrow(nextWords) > 0) {
        #return(unlist(str_split(nextWords$topNextWord,",")))
        if (sl[len] %in% unlist(str_split(nextWords$topNextWord,","))) ret <- TRUE
    } else {
        #print(paste("No match for bigram",bigram,"in",sentence,"--looking for unigram match"))
        unigram <- sl[len-1]
        nextWords <- df_u[row.names(df_u) == unigram,]
        if(nrow(nextWords) > 0) {
            if (sl[len] %in% unlist(str_split(nextWords$topNextWord,","))) ret <- TRUE
        } else {
            if (sl[len] %in% head(row.names(df_u),3)) ret <- TRUE
        }
    }
    return(ret)
}

con <- file("../../data/testset/test.txt", "r", blocking = FALSE)
test <- readLines(con = con, encoding = "UTF-8", skipNul = TRUE)
close(con)

test <- gsub("don't", "do not", test, ignore.case = TRUE)
test <- gsub("doesn't", "does not", test, ignore.case = TRUE)
test <- gsub("can't", "cannot", test, ignore.case = TRUE)
test <- gsub("I'll", "I will", test, ignore.case = TRUE)
test <- gsub("won't", "will not", test, ignore.case = TRUE)
test <- gsub("didn't", "did not", test, ignore.case = TRUE)
test <- gsub("e's", "e is", test, ignore.case = TRUE)
test <- gsub("isn't", "is not", test, ignore.case = TRUE)
test <- gsub("i'm", "i am", test, ignore.case = TRUE)
test <- gsub("it's", "it is", test, ignore.case = TRUE)

res <- vector(mode="logical")

sapply(test, predictTest, USE.NAMES = FALSE)

save(df_u, df_b, file = "../../data/final.Rdata")

