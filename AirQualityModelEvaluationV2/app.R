library(shinydashboard)
library(dplyr)


ui <- dashboardPage(skin='red', # Start dashboardpage
  
  dashboardHeader( # Start dashboardheader
    
  ), # end dashboardheader
  
  dashboardSidebar( # start dashboardsidebar
    
    ## start Sidebar content
    dashboardSidebar(
      sidebarMenu(
        menuItem("Model 1 CSV", tabName = "csvupload1", icon = icon("dashboard")),
        menuItem("Model 2 CSV", tabName = "csvupload2", icon = icon("dashboard")),
        menuItem("Results", tabName = "results", icon = icon("th"))
      )
    ) # End sidebar content
    
  ), # End dashboard sidebar
  
  dashboardBody( # start dashboardbody
    
    
    tabItems( # Start tab structure
      
      tabItem(tabName = "csvupload1", # start First tab content
              h2('Model 1 CSV'),
              
              fluidRow( # start of row
              
                box( # start of box
                ##### Start of CSV1 upload #####
              fileInput("file1", "Choose CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              
              
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
                           selected = '"')
                ), # end of box
              box(tableOutput("contents1"))
              ##### end of CSV1 upload #####
              
              ) # end of  row
              
      ), # end first tab content
      
      tabItem(tabName = "csvupload2", # start second tab content
              h2('Model 2 CSV'),
              
              fluidRow( #start of row
              
              ##### Start of CSV2 upload #####
              box(fileInput("file2", "Choose CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              
              
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
                           selected = '"')
              ), # end of box
              box(tableOutput("contents2"))
              ##### end of CSV2 upload #####
              
              ) ## End of row
              
      ), # end second tab content
      
      tabItem(tabName = "results", # start third tab content
              h2("Model comparisons"),
              
              fluidRow( # Start of a row
                column(width=6,
                box(width = NULL, title = 'Model Controls 1', status = 'primary', solidHeader = T),
                box(width = NULL, title = 'Model Controls 2', status = 'primary', solidHeader = T)),
                column(width=6,
                box(width = NULL, title = "Model 1 Graph"),
                box(width = NULL, offset = 6, title = 'Model 2 Graph')
                )
              ) # End of a row
              
      ) # end third tab content
    ) # End tab structure
    
  ) # End dashboardbody
  
) # End dashboardheader

server <- function(input, output) {
  
  output$contents1 <- renderTable({ # Start of uploading file 1
    
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

    df1
    
  }) # End of uploading file 1

  output$contents2 <- renderTable({ # Start of uploading file 2
    
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
    
    df2
    
  }) # End of uploading file 2
    
  ## Sort out the files for input to the graphs
  
    model1modelled <- reactive({ input$file1 %>%
      select(-SiteID, -month, -Year, -Type, -SiteName,- MeasuredNOX, -MeasuredNO2, -MeasuredPM10, -MeasuredPM25, -MeasuredO3) %>% 
      rename_all(tolower) %>%
      gather(key = 'pollutant', value = 'modelled', 3:7) %>%
      mutate(sitetype = tolower(sitetype)) %>%
      mutate(pollutant = gsub('model', '', pollutant),
            sitetype  = case_when(grepl('heathrow'          , sitetype) ~ 'Heathrow',
                                  grepl('traffic'           , sitetype) ~ 'Kerbside',
                                  grepl('kerbside'          , sitetype) ~ 'Kerbside',
                                  grepl('industrial'        , sitetype) ~ 'Industrial',
                                  grepl('roadside'          , sitetype) ~ 'Roadside',
                                  grepl('suburban'          , sitetype) ~ 'Suburban background',
                                  grepl('urban background'  , sitetype) ~ 'Urban background'))
    })
    
    model1measured <- reactive({input$file1 %>% 
      select(-SiteID, -month, -Year, -Type, -SiteName,- ModelNOx, -ModelNO2, -ModelPM10, -ModelPM25, -ModelO3) %>% 
      rename_all(tolower) %>%
      gather(key = 'pollutant', value = 'measured', 3:7) %>%
      mutate(sitetype = tolower(sitetype)) %>%
      mutate(pollutant = gsub('measured', '', pollutant),
             sitetype  = case_when(grepl('heathrow'          , sitetype) ~ 'Heathrow',
                                   grepl('traffic'           , sitetype) ~ 'Kerbside',
                                   grepl('kerbside'          , sitetype) ~ 'Kerbside',
                                   grepl('industrial'        , sitetype) ~ 'Industrial',
                                   grepl('roadside'          , sitetype) ~ 'Roadside',
                                   grepl('suburban'          , sitetype) ~ 'Suburban background',
                                   grepl('urban background'  , sitetype) ~ 'Urban background'))
    })
    
    data1     <- reactive({left_join(model1modelled, model1measured) %>% filter(modelled != 0 & measured != 0)})
    
    table_data <- reactive({input$file1 %>% rename_all(tolower) %>% select(-siteid, -month, -year, -type, -sitename)})
  
  output$graph2 <- reactive({
    
    model2modelled <- input$file2 %>%
      select(-SiteID, -month, -Year, -Type, -SiteName,- MeasuredNOX, -MeasuredNO2, -MeasuredPM10, -MeasuredPM25, -MeasuredO3) %>% 
      rename_all(tolower) %>%
      gather(key = 'pollutant', value = 'modelled', 3:7) %>%
      mutate(sitetype = tolower(sitetype)) %>%
      mutate(pollutant = gsub('model', '', pollutant),
             sitetype  = case_when(grepl('heathrow'          , sitetype) ~ 'Heathrow',
                                   grepl('traffic'           , sitetype) ~ 'Kerbside',
                                   grepl('kerbside'          , sitetype) ~ 'Kerbside',
                                   grepl('industrial'        , sitetype) ~ 'Industrial',
                                   grepl('roadside'          , sitetype) ~ 'Roadside',
                                   grepl('suburban'          , sitetype) ~ 'Suburban background',
                                   grepl('urban background'  , sitetype) ~ 'Urban background'))
    
    model2measured <- input$file2 %>% 
      select(-SiteID, -month, -Year, -Type, -SiteName,- ModelNOx, -ModelNO2, -ModelPM10, -ModelPM25, -ModelO3) %>% 
      rename_all(tolower) %>%
      gather(key = 'pollutant', value = 'measured', 3:7) %>%
      mutate(sitetype = tolower(sitetype)) %>%
      mutate(pollutant = gsub('measured', '', pollutant),
             sitetype  = case_when(grepl('heathrow'          , sitetype) ~ 'Heathrow',
                                   grepl('traffic'           , sitetype) ~ 'Kerbside',
                                   grepl('kerbside'          , sitetype) ~ 'Kerbside',
                                   grepl('industrial'        , sitetype) ~ 'Industrial',
                                   grepl('roadside'          , sitetype) ~ 'Roadside',
                                   grepl('suburban'          , sitetype) ~ 'Suburban background',
                                   grepl('urban background'  , sitetype) ~ 'Urban background'))
    
    
    data2     <- left_join(model2modelled, model2measured) %>% filter(modelled != 0 & measured != 0)
    
    rm(model2modelled, model2measured)
    
    table_data <- input$file2 %>% rename_all(tolower) %>% select(-siteid, -month, -year, -type, -sitename)
    
  })
  
}

shinyApp(ui, server)