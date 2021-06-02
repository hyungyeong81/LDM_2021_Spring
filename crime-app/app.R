library(shiny)
library(tidyverse)
library(data.table)
library(raster)
setwd("/Users/hyungyeonghong/Desktop/LDM_project/crime-app")
nyc_crimes <- fread("data/data_preprocessed_3.csv")
precinct_idx <- nyc_crimes %>% filter(is.na(precinct) ==  FALSE) %>% dplyr::select(precinct) %>% sapply(unique) %>% sort %>% as.character
nyc_crimes <- nyc_crimes %>% mutate(id = as.character(precinct))
nypp <- shapefile("data/nypp.shp") %>% spTransform(CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>% fortify(nypp, region = "Precinct")


ui <- fluidPage(
  titlePanel("Crimes of NYC Precincts"),
  sidebarLayout(
    sidebarPanel(
      helpText("Please enter date range and your precinct number to see the result."),
      
      dateRangeInput(inputId = "daterange", label = "Date Range",
                     start = "2006-01-01",
                     end = "2020-12-31",
                     min = "2006-01-01",
                     max = "2020-12-31"),
    
      selectInput(inputId = "precinctnum", label = "Your Precinct",
                  choices = precinct_idx, selected = NULL)
      ),
    
    mainPanel(
      plotOutput("map"),
      plotOutput("barplot")
    )
  )
)

server <- function(input, output){
  DateInput <- reactive({
    nyc_crimes %>% filter(date >= input$daterange[1], date <= input$daterange[2]) %>% dplyr::select(id) %>% group_by(id) %>% summarise(count = n()) %>% merge(nypp, ., by = "id")
  })
  
  output$map <- renderPlot({
    ggplot(data = DateInput()) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = count), color = "white") +
      scale_fill_gradient(low = "#FFFFCC", high = "#FF0000") +
      theme_minimal() +
      ggtitle("NYC Precinct Map in Total Number of Crimes") +
      xlab("Longitude") +
      ylab("Latitude") +
      theme(
        panel.grid = element_line(color = "#F5F5F5"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10)
      )
  }, width = 800)
  
  PrecinctInput <- reactive({
    nyc_crimes %>%
      filter(id == input$precinctnum) %>% filter(date >= input$daterange[1], date <= input$daterange[2]) %>%
      dplyr::select(description) %>% filter(is.na(description) == FALSE) %>% group_by(description) %>% summarise(count = n())
  })
  
  output$barplot <- renderPlot({
    PrecinctInput()[(nrow(PrecinctInput())-9) : nrow(PrecinctInput()), ] %>% 
      ggplot(aes(x = reorder(description, count), y = count)) +
      geom_bar(stat = "identity", fill = "#FF9999") +
      coord_flip() +
      theme_minimal() +
      ggtitle(label = sprintf("Top 10 Types of Crimes in Precinct %s", input$precinctnum),
              subtitle = sprintf("From %s to %s", as.character(input$daterange[1]), as.character(input$daterange[2]))) +
      xlab("Types of Crimes") +
      ylab("Number of Crimes") +
      theme(
        panel.grid = element_line(color = "#F5F5F5"),
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        legend.position = "none"
      )
  }, width = 800)
}

shinyApp(ui = ui, server = server)

runApp("crime-app")
