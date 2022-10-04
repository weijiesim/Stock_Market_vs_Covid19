library(shiny)
library(shinythemes)
library(bslib)
library(readr)
library(dplyr)
library(dygraphs)
library(semantic.dashboard)
library(lubridate)
library(quantmod)
library(xts)
library(DT)

# COVID Data -----------------------------------------------------------------------------------
covid_df <-  read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv",
                      na.strings = "",
                      fileEncoding = "UTF-8-BOM") # reading covid data

covid_df <- covid_df %>%
    select(continent, location, date, total_cases, total_deaths, new_cases_smoothed) %>% # selecting relevant data
    mutate(date = as_date(date), new_cases_smoothed = new_cases_smoothed/1000) %>% # changing to date format 
    rename("New Cases Smoothed" = new_cases_smoothed)

covid_df$total_deaths[is.na(covid_df$total_deaths)] <- 0 # changing NAs to 0 in total_deaths col
covid_df$total_cases[is.na(covid_df$total_cases)] <- 0 # changing NAs to 0 in total_cases col

# ui -------------------------------------------------------------------------------------------

ui <- navbarPage(
    theme = bs_theme(
        bootswatch = "flatly",
        base_font = font_google("Economica"),
        heading_font = font_google("Economica"),
        font_scale = 1.2
    ),
    
    title = "Stock Market vs Covid-19",
    
    main_page <- tabPanel(
        title = "Visualisation",
        titlePanel("Visualisation"),
        sidebarLayout(
            sidebarPanel(
                title = "Inputs",
                strong("Type in a stock ticker and select a country's covid data to see if they're correlated!",
                       style = "font-size: 30px;"),
                textInput("symb", strong("Ticker:", style = "font-size: 22px;"), "SPY"),
                selectInput("country",
                            strong("Choose a Country for New COVID Cases:", style = "font-size: 22px;"),
                            unique(covid_df$location),
                            selected = "World"),
                strong("Additional information:", style = "font-size: 17px;"),
                tags$ul(
                    tags$li("Stock tickers must match that of", tags$a(href = "https://sg.finance.yahoo.com/","Yahoo Finance.")),
                    tags$li("Click and drag on chart to zoom in. Double click to zoom out."),
                    style = "font-size: 16px;"
                ),
            ),
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        title = "Plot",
                        dygraphOutput(outputId = "plot")
                    ),
                    tabPanel(
                        title = "Table",
                        shiny::tags$h3(textOutput("stock_dt_title")),
                        DT::dataTableOutput("stock_dt"),
                        shiny::tags$h3(textOutput("covid_dt_title")),
                        DT::dataTableOutput("covid_dt")
                    )
                )
            )
        )
    ),
    
    about_page <- tabPanel(title = "About",
                           titlePanel("About"),
                           "This is a simple app that visualises stock market performance against Covid-19 cases from different countries.",
                           br(),
                           br(),
                           "Created by " ,tags$a(href = "https://github.com/weijiesim/Stock_Market_vs_Covid19" ,"Wei Jie"), " with R Shiny",
                           br(),
                           br(),
                           "Source: Our World in Data, Yahoo Finance"
    )
)

# server ---------------------------------------------------------------------------------------

server <- function(input, output){
    dataInput <- reactive({
        
        stock_data <- getSymbols(input$symb, src = "yahoo",
                                 auto.assign = FALSE)
        
        covid_data <- covid_df %>%
            filter(location == input$country) %>% # allow users to filter country covid data
            select(`date`, `New Cases Smoothed`) # selecting New Cases column
        
        covid_data_ts <- xts(covid_data, order.by = covid_data$date)
        
        merge.xts(last(stock_data[, 4], '3 years'), covid_data_ts[, 2], join = 'left')
        
    })
    
    start_of_covid <- reactive({
        dates <- covid_df %>%
            filter(location == input$country, `New Cases Smoothed` > 0) %>%
            mutate(date = as.character(date)) %>%
            mutate(date = as.POSIXct(date)) %>%
            select(date)
        
        dates[1,1]
    })
    
    sdt_title <- reactive({
        paste(input$symb, " Stock Data Table")
    })
    
    cdt_title <- reactive({
        paste(input$country, " Covid Data Table")
    })
    
    stockDataTableInput <- reactive({
        stock_data <- getSymbols(input$symb, src = "yahoo",
                                 auto.assign = FALSE)
        stock_data_df <- data.frame(date = index(last(stock_data, '3 years')), coredata(last(stock_data, '3 years')))
    })
    
    covidDataTableInput <- reactive({
        covid_data <- covid_df %>%
            filter(location == input$country) %>%
            select(date, total_cases, total_deaths, `New Cases Smoothed`)
    })
    
    graph_title <- reactive({
        title <- paste(input$symb, " Price vs New Covid Cases (000's) in ", input$country)
    })
    
    graph_axis <- reactive({
        title <- paste(input$symb, " Price")
    })
    
    output$stock_dt_title <- renderText({
        sdt_title()
    })
    
    output$covid_dt_title <- renderText({
        cdt_title()
    })
    
    output$stock_dt <- renderDataTable({
        DT::datatable(stockDataTableInput(), options = list(pageLength =5))
    })
    
    output$covid_dt <- renderDataTable({
        DT::datatable(covidDataTableInput(), options = list(pageLength =5))
    })
    
    output$plot <- renderDygraph({
        dygraph(dataInput(), main = graph_title()) %>%
            dyAxis("y", label = graph_axis(), axisLabelColor = "#1DAD93") %>%
            dyAxis("y2", label = "New COVID Cases (000's)") %>%
            dySeries("New.Cases.Smoothed", axis = "y2", label = "New Covid Cases") %>%
            dyEvent(start_of_covid(), "Start of Covid-19", labelLoc = "bottom", strokePattern = "dotted") %>%
            dyOptions(drawGrid = FALSE,
                      digitsAfterDecimal = 0,
                      colors = c("#1DAD93", "#2C3E4F"))
    })
    
}


shinyApp(ui = ui, server = server)