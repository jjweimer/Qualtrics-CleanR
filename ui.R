library(shiny)
library(lubridate)
library(dplyr)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Qualtrics Data Cleaner"),

    # Sidebar with a slider input for number of bins
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
      #show the imported CSV
      mainPanel(
        tableOutput("instruction_data"),
        textOutput("instruction_stats"),
        tableOutput("outreach_data"),
        textOutput("outreach_stats")
      )
    )
))
