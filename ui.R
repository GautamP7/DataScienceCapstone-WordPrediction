library(shiny)

shinyUI(
    navbarPage("Data Science Capstone: Course Project",
        tabPanel("Predict the Next Word",
            sidebarLayout(
                sidebarPanel(
                    textInput("inputString", "Enter a phrase below:", value = "")
                ),
                mainPanel(
                    h3("Predicted Next Words are:"),
                    verbatimTextOutput("prediction"),
                    strong("Entered phrase is:"),
                    tags$style(type='text/css', '#text1 {background-color: rgb(192, 192, 192);}'), 
                    textOutput('text1')
                )
            )        
        ),
        tabPanel("Documentation",
            mainPanel(
                strong("Note:"),
                p("Given a phrase, the app gives 2-3 suggestions of the next word."),
                p("In case it's not able to come up with a suggestion, it does not return anything.")
            )
        )
    )
)