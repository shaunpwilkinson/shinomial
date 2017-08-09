library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel(h2("Exact binomial test", align="left")),
  
  
  sidebarLayout(
    sidebarPanel(
      br(),
      numericInput(inputId="trials", label="Number of trials", value=100, min=0),
      br(),
      uiOutput("successcontainer"),
      #numericInput(inputId="successes", label="Number of successes", value=50, min=0),
      #helpText("Note that the number of successes must not exceed the number of trials"),
      #numericInput(inputId="prob", label="Null probability of success", value=0.5, min=0, max=1),
      br(),
      radioButtons(inputId="hypothesis", 
                   label="Alternative hypothesis", 
                   choices=list("Probability of success < 0.5"="less", "Probability of success > 0.5"= "greater", "Two sided"= "two.sided"),
                   selected = "two.sided")#,
      #submitButton(text="Submit")
    ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",plotOutput("binomPDF")),
        tabPanel("Summary",
                 br(),
                 strong("Test output"),
                 textOutput("text1"),
                 textOutput("text2"),
                 textOutput("text3"),
                 textOutput("text4"),
                 textOutput("text5"),
                 textOutput("text6"),
                 textOutput("text7")
        )
      )
    )
  )
))






