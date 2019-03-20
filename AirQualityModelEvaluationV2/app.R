## app.R ##
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "AQ Model Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("CSV uploads",   tabName = "csv_upload", icon = icon("dashboard")),
      menuItem("Graph outputs", tabName = "graphs", icon = icon("th"))
    )
  ),
  
  ## Start of the body
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidPage(
        column(4,
             fileInput("file1", "Choose CSV File",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
             
             # Horizontal line ----
             tags$hr(),
             
             # Input: Checkbox if file has header ----
             checkboxInput("header1", "Header", TRUE),
             
             # Input: Select separator ----
             radioButtons("sep1", "Separator",
                          choices = c(Comma = ",",
                                      Semicolon = ";",
                                      Tab = "\t"),
                          selected = ","),
             
             # Input: Select quotes ----
             radioButtons("quote1", "Quote",
                          choices = c(None = "",
                                      "Double Quote" = '"',
                                      "Single Quote" = "'"),
                          selected = '"'),
             
             # Horizontal line ----
             tags$hr(),
             
             # Input: Select number of rows to display ----
             radioButtons("disp1", "Display",
                          choices = c(Head = "head",
                                      All = "all"),
                          selected = "head")#,
             
             #tableOutput("contents1")
      ),
      column(4,
             fileInput("file2", "Choose CSV File",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
             
             # Horizontal line ----
             tags$hr(),
             
             # Input: Checkbox if file has header ----
             checkboxInput("header2", "Header", TRUE),
             
             # Input: Select separator ----
             radioButtons("sep2", "Separator",
                          choices = c(Comma = ",",
                                      Semicolon = ";",
                                      Tab = "\t"),
                          selected = ","),
             
             # Input: Select quotes ----
             radioButtons("quote2", "Quote",
                          choices = c(None = "",
                                      "Double Quote" = '"',
                                      "Single Quote" = "'"),
                          selected = '"'),
             
             # Horizontal line ----
             tags$hr(),
             
             # Input: Select number of rows to display ----
             radioButtons("disp2", "Display",
                          choices = c(Head = "head",
                                      All = "all"),
                          selected = "head")#,
             
             #tableOutput("contents2")
      )
      )
    
  )
  ) ## End of the body

server <- function(input, output) {
  
  output$contents1 <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df1 <- read.csv(input$file1$datapath,
                       header = input$header1,
                       sep = input$sep1,
                       quote = input$quote1)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
    if(input$disp1 == "head") {
      return(head(df1))
    }
    else {
      return(df1)
    }
    
    
    
  })
  
  output$contents2 <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file2)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    
    tryCatch(
      {
        df2 <- read.csv(input$file2$datapath,
                        header = input$header2,
                        sep = input$sep2,
                        quote = input$quote2)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
    if(input$disp2 == "head") {
      return(head(df2))
    }
    else {
      return(df2)
    }
    
    
    
  })
  
}

shinyApp(ui, server)