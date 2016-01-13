library(shiny)
library(stringr)
load("final.Rdata")

predict <- function(sentence) {
    # given a sentence/phrase, extract the last two words
    sl <- unlist(str_split(sentence," "))
    sl <- gsub("[^a-zA-zèòàùèéùì]|\\[|\\]|\\\\|_|\\^|-|`", "", sl)
    sl <- sl[which(sl != "")]
    len <- length(sl)
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$predictWord <- renderUI({
        #output$distPlot <- renderDataTable({
        if (input$action==0) return(NULL)
        
        isolate({
            HTML(paste("<ul><li>",
                paste(predict(input$phrase), collapse = "</li><li>", sep = ""),
                "</li></ul>", sep="")
            )
        })
    })
})
