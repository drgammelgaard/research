#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
   
   # Application title
   titlePanel("Chi square test of HWE for bi- or triallelic systems"),
   
   sidebarLayout(
      sidebarPanel(
        
        # Input: Numeric entry for number of alleles ----
        selectInput(inputId = "ale",
                     label = "Number of alleles:",
                     choices=c(2,3)),
        
         numericInput(inputId = "pp",
                      label = "MM:",
                      value=NA),

         numericInput(inputId = "pq",
                      label = "MN:",
                      value=NA),

         numericInput(inputId = "qq",
                      label = "NN:",
                      value=NA),
        conditionalPanel(condition = "input.ale==3",

                         numericInput(inputId = "pr",
                                      label = "MO:",
                                      value=NA),

                         numericInput(inputId = "qr",
                                      label = "NO:",
                                      value=NA),

                         numericInput(inputId = "rr",
                                      label = "OO:",
                                      value=NA))

      ),
      
      mainPanel(
        
        # Output: Formatted text for caption ----
        h3(textOutput("obs.dist", container = span)),
        
        # Table output
         htmlOutput("obs.tbl", container = span),
        
        # Output: Formatted text for caption ----
        h3(textOutput("exp.dist", container = span)),
        
        # Table output
        htmlOutput("exp.tbl", container = span),
        
        # Output: Formatted text for caption ----
        h3(textOutput("chi", container = span)),
        
        # Table output
        htmlOutput("chi.val", container = span),
        
        # Output: Formatted text for caption ----
        h3(textOutput("p", container = span)),
        
        # Table output
        htmlOutput("p.val", container = span)
      )
   )
)

# Define server logic required to draw a histogram

source("https://raw.githubusercontent.com/drgammelgaard/research/master/hwe_geno.R")

server <- function(input, output) {
  
  cale <- reactive({
    as.numeric(input$ale)
  })
  
  cmm <- reactive({
    as.numeric(input$pp)
  })
  
  cmn <- reactive({
    as.numeric(input$pq)
  })
  
  cnn <- reactive({
    as.numeric(input$qq)
  })
  
  cmo <- reactive({
    as.numeric(input$pr)
  })
  
  cno <- reactive({
    as.numeric(input$qr)
  })
  
  coo <- reactive({
    as.numeric(input$rr)
  })
  
  hwe_p <- function() ({ hwe_geno(cmm(),cmn(),cnn(),cmo(),cno(),coo(),alleles=cale()) })
  
  output$obs.tbl <- renderTable({ hwe_p()$observed.dist })
  
  output$exp.tbl <- renderTable({ hwe_p()$expected.dist })
  
  output$chi.val <- renderTable({ hwe_p()$chi.value })
  
  output$p.val <- renderTable({ hwe_p()$p.value })
  
  output$obs.dist <- renderText({"Observed distribution"})
  
  output$exp.dist <- renderText({"Expected distribution"})
  
  output$chi <- renderText({"Chi square value"})
  
  output$p <- renderText({"P value"})
}
   
# Run the application 
shinyApp(ui = ui, server = server)

