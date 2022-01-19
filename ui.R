library(shiny)
library(lubridate)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cosmo"),
                  
    #navbar
    navbarPage("Qualtrics Consults/Instruction/Outreach Data Cleaner"),
                  
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
        selectInput("quarter", "Choose A Quarter:", #this is our selector
                    c("All" = "All",
                      "Winter" = "WI",
                      "Spring" = "SP",
                      "Summer" = "SU",
                      "Fall" = "FA",
                      "Break" = "Break")),
        selectInput("year", "Choose A Year:", #this is our selector
                    c("All" = "All",
                      "2022" = "2022",
                      "2021" = "2021",
                      "2020" = "2020",
                      "2019" = "2019")),
        tags$hr(),
        helpText("This is an applet to clean user uploaded service stats Qualtrics
                  data.
                  Upload a Qualtrics .csv file using the button above. 
                  Summary statistics, plots and tables will be generated for
                  you automatically."),
        helpText("To subset your data by Quarter or Year 
                  (or both) you can use the \"Choose a Quarter\" and 
                 \"Choose a Year\" boxes."),
        helpText("Tip: Plots are interactive! Hover your mouse over observations
                 to learn more about the data."),
        tags$hr() #,
        #helpText("Developed by Joshua Weimer for the UCSD Data and GIS Lab.")
        
      ), #end sidebar
      
      #Main Panel
      mainPanel(
        tabsetPanel(type='tabs',
                    tabPanel("Consults",
                             tags$hr(),
                             textOutput("consults_stats"),
                             
                             tags$hr(),
                             numericInput("n","Minimum Count of Department:"
                                          ,2,min = 1),
                             plotlyOutput("consults_graph"),
                             tags$hr(),
                             plotlyOutput("consults_per_week"),
                             tags$hr()
                    ),
                    tabPanel("Instruction",
                             tags$hr(),
                             textOutput("instruction_stats"),
                             tags$hr(),
                             plotlyOutput("instruction_time_plot"),
                             tags$hr(),
                             tableOutput("instruction_data")
                             ),
                    tabPanel("Outreach",
                             tags$hr(),
                             textOutput("outreach_stats"),
                             tags$hr(),
                             tableOutput("outreach_data"),
                             )
                    )
      ) #end main panel
    
))
