library(shiny)

ui <- fluidPage(
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
)


server <- function(input, output){
  
  x <- reactive(binom.test(input$successes, input$trials, 0.5, alternative=input$hypothesis))
  
  output$successcontainer <- renderUI({
    sliderInput(inputId="successes", label="Number of successes", min=0, max=input$trials, value=input$trials/2, step=1)
    
  })
  
  output$binomPDF<-renderPlot({
    plot.x<-1:x()$parameter
    plot.y<-dbinom(plot.x,x()$parameter,0.5)
    plot.title<-paste("Binomial probability density function for",x()$parameter, "independent trials")
    plot(plot.x, plot.y, type="h", main=plot.title, 
         xlab="Number of successes (red marker shows observed value)", 
         ylab="Probability of occurrence",
         frame.plot=FALSE)
    axis(side=1)
    legend("topright",paste("p-value =", signif(x()$p.value, digits=3)) ,bty="o")
    points(xy.coords(x()$statistic,0), cex=1, pch=2, col="red")
  })
  
  output$text1 <- renderText({
    paste("Number of trials: ",x()$parameter)
  })
  
  output$text2 <- renderText({
    paste("Number of successes: ",x()$statistic)
  })
  
  output$text3 <- renderText({
    paste("Probability of success under null hypothesis: 0.5")
  })
  
  output$text4 <- renderText({
    paste("Alternative hypothesis: ",x()$alternative)
  })
  
  output$text5 <- renderText({
    paste("Point estimate for probability of success: ",signif(x()$estimate,3))
  })
  
  output$text6 <- renderText({
    paste("95% confidence interval for point estimate: [",signif(x()$conf.int[1],3), ",",signif(x()$conf.int[2],3),"]")
  })
  
  output$text7 <- renderText({
    paste("P-value: ",signif(x()$p.value, digits=3))
  })
  
}

shinyApp(ui = ui, server = server)