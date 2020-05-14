
#Load libraries and necessary packages
library(shiny)
library(leaflet)
library(magrittr)
library(tidyverse)
library(dplyr)
library(DT)
library(forecast)
library(ggplot2)
library(plotly)
library(summarytools)
library(xtable)
library(shinyWidgets)

# Define UI 
ui <- fluidPage(

    mainPanel(width = "100%"),
    navbarPage("Trends in coronavirus testing in Maryland",
               
               tabPanel(h4("This app visualizes trends in coronavirus data in Maryland"),
                        h4("I have chosen Maryland as a point of interest, as I would like to learn more about local coronavirus trends"),
                        h4("and I feel more comfortable working with a smaller dataset rather than a global one")),
                        h4("This project is available at", a(href="https://github.com/ds4ph-bme/project-awkywoo/tree/master", "Github Project.", target = "_blank"))
               ),
    
                #####Start of zipcode page-----------------    
                
                tabPanel("By Zipcode",
                    fluidRow(
                        column(12, "On this page, you can observe the number of cases by zipcodes on different dates in Maryland"),
                        fluidRow(column(2,
                            pickerInput(inputId = "zipcode", label = "Zipcode", choices = unique("MDZip$zipcode"), selected = "MDZip$zipcode", multiple = TRUE)
                        )),
                        column(2, dateInput("MDZip$date", label = "date", format = "yyyy-mm-dd", startview = "month")),
                        column(2, uiOutput("MDZip$count")),
                        fluidRow(
                          dataTableOutput("renderedMDZip"),
                          plotOutput("zipplot"),
                        )
                    )
                ),
    
                ### Start of gender page-----------------
              
                mainPanel(plotOutput("genderplot")),
                
    ###         Start of race page-----------------
                mainPanel(plotOutput("raceplot")),
                
)

# Define server logic 
server <- function(input, output) {
  MDRace <- read.table("race.txt", header = TRUE, sep = " ", colClasses = c("Date", "numeric", "numeric", "numeric", "numeric", "numeric"))
  output$renderedMDRace <- renderDataTable (MDRace)
  MDZip <- read.table("zipcode.txt", header = TRUE, sep = " ", colClasses = c("Date", "numeric", "numeric"))
  output$renderedMDZip <- renderDataTable (MDZip)
  MDGender <- read.table("gender.txt", header = TRUE, sep = " ", colClasses = c("Date", "character", "numeric"))
  output$MDGender <- renderDataTable(MDGender)
  
  output$zipplot <- renderPlot({ggplot(MDZip, aes(x = date, y = count, group = zipcode)) +
    geom_line(aes(color = zipcode)) +
    theme(legend.position = "bottom") +
    labs(title = "Coronavirus cases by zip codes in Maryland(april-may 2020)",
         x = "Date",
         y = "Cumulative case counts")
    })
  
  output$zip0 <- renderUI ({
    req(input$zipcode)
    
    zipcode <- MDZip %>% filter(zipcode %in% input$zipcode)%>% pull(zipcode) %>% sort()
    
    pickerInput(inputId = "zip0",
                label = "zipcode",
                choices = zipcode,
                selected = c("21201"),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)
  })
  
  output$genderplot <- renderPlot({ggplot(MDGender, aes(x = date, y = count, group = gender)) +
      geom_line(aes(color = gender)) +
      theme(legend.position = "bottom") +
      labs(title = "Coronavirus cases by gender in Maryland, USA (april-may 2020)",
           x = "Date",
           y = "Cumulative Case counts")
  })
  
  # Group races together
  cols2pivot <- colnames(MDRace)
  cols2pivot <- c("date", "african american", "asian", "white", "hispanic", "other")
  colnames(MDRace) <-cols2pivot
  cols2pivot <- cols2pivot[2:6]
  
  raceData <- pivot_longer(data=MDRace, cols = cols2pivot, names_to = "Races", values_to = "Count")
  
  output$raceplot <- renderPlot({ggplot(raceData, aes(x = date, y = Count, group = Races)) +
      geom_line(aes(color = Races), lwd = 1) +
      geom_point(aes(color = Races, shape = Races)) +
      theme(legend.position = "bottom") +
      labs(title = "Coronavirus cases by race in Maryland(april-may2020)",
           x = "Date",
           y = "Cumulative case counts")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

#End of Program