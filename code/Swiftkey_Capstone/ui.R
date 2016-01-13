library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("DSC - Swiftkey Capstone"),
    
    mainPanel(
        fluidRow(
            h4("Please enter your phrase and hit \"Predict next word\" button"),
            textInput("phrase", 
                      label = h3("Your phrase"), 
                      value = "Enter text...", 
                      width = "1200"),
            br(),
            actionButton("action", label = "Predict next word"),
            br(),br(),
            conditionalPanel(
                condition = "input.action != 0",
                htmlOutput("predictWord")
            )
        )
    )
    )
)