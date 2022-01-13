library(shiny)
library(lubridate)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Qualtrics Data Cleaner"),

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Header", TRUE)
      ),
      
      
      #show the tables, stats
      mainPanel(
        tabsetPanel(type='tabs',
                    tabPanel("Instruction",textOutput("instruction_stats"),tableOutput("instruction_data")),
                    tabPanel("Outreach",textOutput("outreach_stats"),tableOutput("outreach_data")),
                    tabPanel("Consults",textOutput("consults_stats"), plotlyOutput("consults_graph")),
                    tabPanel("About",textOutput("about_text"), textOutput("dev_text")))
        
        
      )
    )
))
