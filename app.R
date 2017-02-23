devtools::install_github("msabr027/WatsonML")
library(WatsonMLScore)
library(shiny)
library(shinythemes)

urlscore = "https://ibm-watson-ml.mybluemix.net/pm/v1/score/"

urlmeta = "https://ibm-watson-ml.mybluemix.net/pm/v1/metadata/"

contextid = "custstat1.0"

accesskey = "UWOHg/cnH24KFcnLfIPB7jhNV+BOWKh6JggqzpsvL+cBUzLkamsrnfj16U89zw78HxGxQ3pIogjgEOjN0TGDTcL0h32gVzPkwMbmHXNpi+FQYUqQmv73SQJrb1WXWeZv"

tablename = "Input data"

header = c("customerID","gender",	"SeniorCitizen",	"Partner",	"Dependents",	"tenure",	"PhoneService",	"MultipleLines",	"InternetService","OnlineSecurity",	"OnlineBackup",	"DeviceProtection",	"TechSupport","StreamingTV",	"StreamingMovies"	,"Contract",	"PaperlessBilling",	"PaymentMethod",	"MonthlyCharges",	"TotalCharges",	"Churn",	"SampleWeight")

ui <-
navbarPage(
  title = "Watson Machine Learning",

  theme = shinytheme("flatly"),



  tabPanel(

    title =  "Home",
    sidebarPanel(
    inputPanel(fileInput('file1', 'Choose CSV File',
                         accept=c('text/csv',
                                  'text/comma-separated-values,text/plain',
                                  '.csv')),
               tags$hr(),
               checkboxInput('header', 'Header', TRUE),
               radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t'),
                            ',')))

  ),

  tabPanel("Model Info", fixedPage(fluidRow(column(width=4,offset=4,br(),br(), tableOutput('info'))))),
  tabPanel("Variables Names ",column(width=8,fluidRow(uiOutput('comparenamesresult',container = tags$h2)),fluidRow(tableOutput('tablecomparenames')))),
  tabPanel("Data Input", tableOutput('datapushed'),uiOutput('button')),
  tabPanel("Results", tableOutput('result'))
  )







  server <- function(input, output) {


    observeEvent(input$file1,{

      inFile1 <- input$file1

      if (is.null(inFile1))
        return(NULL)

      inFile <- read.csv(inFile1$datapath, header=input$header, sep=input$sep, stringsAsFactors=FALSE)



    output$datapushed=renderTable({
      inFile
    })

    output$info <- renderTable({
      datatype=WatsonMLScore::modelinfo(contextid,urlmeta,accesskey)
    })


    output$button <- renderUI({
      actionButton("sendrequest", label="Score the data")
      })

   comparenames= WatsonMLScore::validnames(contextid,urlmeta,accesskey,datajust=inFile)

    output$tablecomparenames <- renderTable({
    comparenames$table
    })
    output$comparenamesresult <- renderUI({
    strong(comparenames$info)
    })

    })


    observeEvent(input$sendrequest,{



      inFile <- read.csv(input$file1$datapath, header=input$header, sep=input$sep, stringsAsFactors=FALSE)



      datajust = inFile

     output$result <- renderTable({
       realtime_score(tablename,header,datajust,contextid,urlscore,accesskey)
     })

    })

  }

  shinyApp(ui = ui, server = server)



