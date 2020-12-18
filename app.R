#*********************************************************************
# Purpose: COVID-19 Trends
# Date: 13/5/2020
# Author: LNN
# Acknowledge: Taryn Morris (R-Ladies - Intro to shiny)
#*********************************************************************

rm(list = ls(all = TRUE))

# package load
suppressMessages({
    library(tidycovid19)
    library(tidyverse)
    library(extrafont)
    library(ggthemes)
    library(plotly)
    library(fcuk)
    library(shiny)
    library(shinyWidgets)
    library(rsconnect)
})

# import data
suppressWarnings({
    covdata <-
        download_jhu_csse_covid19_data()
    # saveRDS(covdata, "covdata.rds")
    # readRDS("data//covdata.rds")
})


# Define UI for application that draws the graph
ui <- fluidPage(
    
    # Application title
    titlePanel("Covid-19 Trends"),
    
    # Sidebar with a slider input for choosing country
    sidebarLayout(
        sidebarPanel(
            pickerInput(
                inputId = "country", label = "Choose your country (s)",
                choices = unique(covdata$country),
                selected = "Kenya",
                multiple = TRUE
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("covid19Trend"),
            plotlyOutput("Cov19deaths"),
            plotlyOutput("Cov19recover")
        )
    )
)

# Define server logic to show Covid-19 trends
server <- function(input, output) {
    output$covid19Trend <- plotly::renderPlotly({
        theme_set(theme_tufte())
        validate(need(input$country != "","Please, choose your country (s)."))
        covid.trend <- ggplot(
            covdata %>% filter(country == input$country),
            aes(x = date, colour = country)
        ) +
            geom_line(aes(y = confirmed)) +
            geom_point(aes(y = confirmed, x = date)) +
            labs(
                x = "Date", y = "Confirmed Cases",
                title = "Covid-19 Confirmed Cases Trends"
            ) +
            theme(
                plot.title = element_text(
                    family = "Times New Romans", size = rel(1.2),
                    hjust = 0.5,
                    face = "bold"
                ),
                axis.line = element_line(colour = "black", size = 0.5),
                axis.text.x = element_text(
                    family = "Times New Romans", size = rel(1.0),
                    hjust = 0.5
                ),
                axis.text.y = element_text(
                    family = "Times New Romans", size = rel(1.0),
                    hjust = 0.5
                )
            )
        ggplotly(covid.trend)
    })
    
    output$Cov19deaths <- renderPlotly({
        validate(need(input$country != "","Please, choose your country (s)."))
        covid.deaths <- ggplot(
            covdata %>% filter(country == input$country),
            aes(x = date, colour = country)
        ) +
            geom_line(aes(y = deaths)) +
            geom_point(aes(y = deaths, x = date)) +
            labs(
                x = "Date", y = "Deaths",
                title = "Covid-19 Death Trends"
            ) +
            theme(
                plot.title = element_text(
                    family = "Times New Romans", size = rel(1.2),
                    hjust = 0.5,
                    face = "bold"
                ),
                axis.line = element_line(colour = "black", size = 0.5),
                axis.text.x = element_text(
                    family = "Times New Romans", size = rel(1.0),
                    hjust = 0.5
                ),
                axis.text.y = element_text(
                    family = "Times New Romans", size = rel(1.0),
                    hjust = 0.5
                )
            )
        ggplotly(covid.deaths)
    })
    
    output$Cov19recover <- renderPlotly({
        validate(need(input$country != "","Please, choose your country (s)."))
        covid.recover <- ggplot(
            covdata %>% filter(country == input$country),
            aes(x = date, colour = country)
        ) +
            geom_line(aes(y = recovered)) +
            geom_point(aes(y = recovered, x = date)) +
            labs(
                x = "Date", y = "Recovered",
                title = "Covid-19 Recovered Trends"
            ) +
            theme(
                plot.title = element_text(
                    family = "Times New Romans", size = rel(1.2),
                    hjust = 0.5,
                    face = "bold"
                ),
                axis.line = element_line(colour = "black", size = 0.5),
                axis.text.x = element_text(
                    family = "Times New Romans", size = rel(1.0),
                    hjust = 0.5
                ),
                axis.text.y = element_text(
                    family = "Times New Romans", size = rel(1.0),
                    hjust = 0.5
                )
            )
        ggplotly(covid.recover)
    })
}

# Run the application
shinyApp(ui = ui, server = server)

deployApp("C:\\Users\\NYAGAH\\Documents\\covid19trends\\trial12covidtrends")