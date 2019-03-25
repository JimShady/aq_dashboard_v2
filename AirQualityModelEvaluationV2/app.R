library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(openair)


ui <- dashboardPage(skin='red', # Start dashboardpage
  
  dashboardHeader( # Start dashboardheader
    title = ("ERG Model Evaluator")
    
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
              
              helpText('This app expects a CSV file formatted in a specific way. For an example please ',
                       a("download this file", href="http://raw.githubusercontent.com/JimShady/simple_aq_dashboard/master/test_data.csv")),
              
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
                )#, # end of box
              #box(tableOutput("contents1"))
              ##### end of CSV1 upload #####
              
              ) # end of  row
              
      ), # end first tab content
      
      tabItem(tabName = "csvupload2", # start second tab content
              h2('Model 2 CSV'),
              
              helpText('This app expects a CSV file formatted in a specific way. For an example please ',
                       a("download this file", href="http://raw.githubusercontent.com/JimShady/simple_aq_dashboard/master/test_data.csv")),
              
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
              )#, # end of box
              #box(tableOutput("contents2"))
              ##### end of CSV2 upload #####
              
              ) ## End of row
              
      ), # end second tab content
      
      tabItem(tabName = "results", # start third tab content
              
              fluidRow( # Start of a row
                
                column(width=2,
                box(width = NULL, title = 'Model Controls 1', status = 'primary', solidHeader = T,
                    uiOutput('file1pollutantselector'),
                    uiOutput('file1sitetypeselector'),
                    uiOutput('file1sitecodeselector'))
                ),
                
                column(width=5,
                box(width = NULL,  
                    plotOutput('graph1',
                    click = "plot_click1",
                    dblclick = "plot_dblclick1",
                    hover = "plot_hover1",
                    brush = "plot_brush1",
                    height = "350px"))
                ),
                
                column(width = 5,
                       box(width = NULL, title = "Model 1 Stats", status = "warning", solidHeader = TRUE, tableOutput('table1')),
                       box(width = NULL, title = "Model 1 Selection", status = "warning", solidHeader = TRUE, tableOutput('info1'))
                       )
              ), # End of a row
              fluidRow(
                column(width=2,
                box(width = NULL, title = 'Model Controls 2', status = 'primary', solidHeader = T,
                    uiOutput('file2pollutantselector'),
                    uiOutput('file2sitetypeselector'),
                    uiOutput('file2sitecodeselector'))
                ),
                column(width=5,
                box(width = NULL,
                    plotOutput('graph2',
                               click = "plot_click2",
                               dblclick = "plot_dblclick2",
                               hover = "plot_hover2",
                               brush = "plot_brush2",
                               height = "350px"))
                ),
                column(width = 5,
                box(width = NULL, title = 'Model 2 Stats', status = "warning", solidHeader = TRUE, tableOutput('table2')),
                box(width = NULL, title = "Model 2 Selection", status = "warning", solidHeader = TRUE, tableOutput('info2'))
                )
                
              )
      ) # end third tab content
    ) # End tab structure
    
  ) # End dashboardbody
  
) # End dashboardheader

server <- function(input, output) {
  
  dataframe1 <- reactive({
    req(input$file1)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        read.csv(input$file1$datapath,
                        header = input$header1,
                        sep = input$sep1,
                        quote = input$quote1,
                        stringsAsFactors = F)
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  output$contents1 <- renderTable({ dataframe1() }) # End of uploading file 1

  
  dataframe2 <- reactive({
    req(input$file2)
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        read.csv(input$file2$datapath,
                 header = input$header2,
                 sep = input$sep2,
                 quote = input$quote2,
                 stringsAsFactors = F)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
  })
  
  output$contents2 <- renderTable({ dataframe2() }) # End of uploading file 1
    
  ## Sort out the files for input to the graphs
  
    model1modelled <- reactive({ dataframe1() %>%
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
    
    model1measured <- reactive({ dataframe1() %>% 
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
    
    data1     <- reactive({left_join(model1modelled(), model1measured()) %>% filter(modelled != 0 & measured != 0)})
    
    table_data1 <- reactive({dataframe1() %>% rename_all(tolower) %>% select(-siteid, -month, -year, -type, -sitename)})
    
    output$file1pollutantselector <- renderUI({
      selectInput("pollutant1", "pollutant:",  sort(unique(as.character(data1()$pollutant))))
    })
    
    output$file1sitetypeselector <- renderUI({
      pickerInput(input="sitetype1",
                  label="sitetype:",
                  choices=unique(data1()$sitetype),
                  options = list(`actions-box` = TRUE),
                  multiple = T,
                  selected = unique(data1()$sitetype))
    })
    
    output$file1sitecodeselector <- renderUI({
      pickerInput(input="sitecode1",
                  label="sitecode:",
                  choices=unique(data1()$sitecode),
                  options = list(`actions-box` = TRUE),
                  multiple = T,
                  selected = unique(data1()$sitecode))
    })
    
    ## Make the stats summary table
    output$table1 <-renderTable({
      filter(
        data1() , 
             pollutant == input$pollutant1, sitetype %in% input$sitetype1 & sitecode %in% input$sitecode1) %>%
        modStats(mod = 'modelled',
                 obs = 'measured') %>%
        as_tibble() %>% select(-default)
      
    })
    
    cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    
    output$graph1 <- renderPlot(
      {
        ggplot(filter(data1(), pollutant == input$pollutant1, sitetype %in% input$sitetype1 & sitecode %in% input$sitecode1),
               aes(x = measured, y = modelled, label = sitecode, colour = sitetype)) +
          geom_point() +
          scale_colour_manual(values=cbbPalette) +
          coord_fixed() +
          xlab('Measured') +
          ylab('Modelled') +
          xlim(-5,
               max(c(filter(data1(), pollutant == input$pollutant1, sitetype %in% input$sitetype1 & sitecode %in% input$sitecode1)$modelled,
                    filter(data1(), pollutant == input$pollutant1, sitetype %in% input$sitetype1)$measured))) +
          ylim(-5,
               max(c(filter(data1(), pollutant == input$pollutant1, sitetype %in% input$sitetype1 & sitecode %in% input$sitecode1)$modelled,
                     filter(data1(), pollutant == input$pollutant1, sitetype %in% input$sitetype1)$measured))) +
          theme(legend.title     = element_blank(),
                panel.border     = element_blank(),
                axis.title       = element_text(size=14, colour = 'black'),
                axis.text        = element_text(size=14, colour = 'black'),
                legend.text      = element_text(size=12, colour = 'black'),
                legend.position  = 'right',
                legend.spacing.x = unit(0.7, 'cm'),
                plot.margin      = unit(c(0,0,0,0), "cm"),
                panel.spacing     = unit(c(-1,-1,-1,-1), "cm"),
                axis.line        = element_line(colour='black'),
                panel.background = element_rect(fill = 'white'),
                panel.grid.major = element_line(colour = 'grey'),
                panel.grid.minor = element_blank()) +
          geom_abline(intercept = 0, slope = 1, colour='red') + 
          geom_abline(intercept = 0, slope = 2, linetype="dashed", colour='red') +
          geom_abline(intercept = 0, slope = 0.5, linetype="dashed", colour='red') +
          guides(guide_legend(ncol = 4))
        
      })
    
    output$info1 <- renderTable({
      # With base graphics, need to tell it what the x and y variables are.
      clicked_data <- brushedPoints(filter(data1(), pollutant == input$pollutant1, sitetype %in% input$sitetype1 & sitecode %in% input$sitecode1),
                                    input$plot_brush1)
      select(clicked_data, sitecode, sitetype, modelled, measured)
      # nearPoints() also works with hover and dblclick events
    })
    
    ### Now the same for file 2
    
    model2modelled <- reactive({ dataframe2() %>%
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
    
    model2measured <- reactive({ dataframe2() %>% 
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
    
    data2     <- reactive({left_join(model2modelled(), model2measured()) %>% filter(modelled != 0 & measured != 0)})
    
    table_data2 <- reactive({dataframe2() %>% rename_all(tolower) %>% select(-siteid, -month, -year, -type, -sitename)})
  
    output$file2pollutantselector <- renderUI({
      selectInput("pollutant2", "pollutant:",  sort(unique(as.character(data2()$pollutant))))
    })
    
    output$file2sitetypeselector <- renderUI({
      pickerInput(input="sitetype2",
                  label="sitetype:",
                  choices=unique(data2()$sitetype),
                  options = list(`actions-box` = TRUE),
                  multiple = T,
                  selected = unique(data2()$sitetype))
    })
    
    output$file2sitecodeselector <- renderUI({
      pickerInput(input="sitecode2",
                  label="sitecode:",
                  choices=unique(data2()$sitecode),
                  options = list(`actions-box` = TRUE),
                  multiple = T,
                  selected = unique(data2()$sitecode))
    })
    
    ## Make the stats summary table
    output$table2 <-renderTable({
      filter(
      data2() , 
        pollutant == input$pollutant2, sitetype %in% input$sitetype2 & sitecode %in% input$sitecode2) %>%
        modStats(mod = 'modelled',
                 obs = 'measured') %>%
        as_tibble() %>% select(-default)
      
    }, width='3cm')
    
    output$graph2 <- renderPlot(
      {
        ggplot(filter(data2(), pollutant == input$pollutant2, sitetype %in% input$sitetype2 & sitecode %in% input$sitecode2),
               aes(x = measured, y = modelled, label = sitecode, colour = sitetype)) +
          geom_point() +
          scale_colour_manual(values=cbbPalette) +
          coord_fixed() +
          xlab('Measured') +
          ylab('Modelled') +
          xlim(-5,
               max(c(filter(data2(), pollutant == input$pollutant2, sitetype %in% input$sitetype2 & sitecode %in% input$sitecode2)$modelled,
                     filter(data2(), pollutant == input$pollutant2, sitetype %in% input$sitetype2)$measured))) +
          ylim(-5,
               max(c(filter(data2(), pollutant == input$pollutant2, sitetype %in% input$sitetype2 & sitecode %in% input$sitecode2)$modelled,
                     filter(data2(), pollutant == input$pollutant2, sitetype %in% input$sitetype2)$measured))) +
          theme(legend.title     = element_blank(),
                panel.border     = element_blank(),
                axis.title       = element_text(size=14, colour = 'black'),
                axis.text        = element_text(size=14, colour = 'black'),
                legend.text      = element_text(size=12, colour = 'black'),
                legend.position  = 'right',
                legend.spacing.x = unit(0.7, 'cm'),
                plot.margin      = unit(c(0,0,0,0), "cm"),
                panel.spacing     = unit(c(-1,-1,-1,-1), "cm"),
                axis.line        = element_line(colour='black'),
                panel.background = element_rect(fill = 'white'),
                panel.grid.major = element_line(colour = 'grey'),
                panel.grid.minor = element_blank()) +
          geom_abline(intercept = 0, slope = 1, colour='red') + 
          geom_abline(intercept = 0, slope = 2, linetype="dashed", colour='red') +
          geom_abline(intercept = 0, slope = 0.5, linetype="dashed", colour='red') +
          guides(guide_legend(ncol = 4))
        
      })
    
    output$info2 <- renderTable({
      # With base graphics, need to tell it what the x and y variables are.
      clicked_data <- brushedPoints(filter(data2(), pollutant == input$pollutant2, sitetype %in% input$sitetype2 & sitecode %in% input$sitecode2),
                                    input$plot_brush2)
      select(clicked_data, sitecode, sitetype, modelled, measured)
      # nearPoints() also works with hover and dblclick events
    })
  
  
}

shinyApp(ui, server)