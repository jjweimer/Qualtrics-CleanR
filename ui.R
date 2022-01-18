library(shiny)
library(lubridate)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cosmo"),
                  
    #navbar
    navbarPage("Qualtrics Data Cleaner"),
                  
    # Alternative way to render title
    #titlePanel("Qualtrics Data Cleaner"),

    # Sidebar
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        helpText("This is an applet to clean user uploaded Qualtrics data.
                  Upload a Qualtrics .csv file using the button above. 
                  Summary statistics, plots and tables will be generated for
                  you automatically."),
        tags$hr() #,
        #helpText("Developed by Joshua Weimer for the UCSD Data and GIS Lab.")
        
      ), #end sidebar
      
      #Main Panel
      mainPanel(
        tabsetPanel(type='tabs',
                    tabPanel("Instruction",
                             #tableOutput("instruction_data"),
                             textOutput("instruction_stats"),
                             tags$hr(),
                             plotlyOutput("instruction_time_plot")),
                    tabPanel("Outreach",
                             tableOutput("outreach_data"),
                             textOutput("outreach_stats")),
                    tabPanel("Consults",
                             textOutput("consults_stats"),
                             tags$hr(),
                             plotlyOutput("consults_graph"))
                    )
      ) #end main panel
    
))
