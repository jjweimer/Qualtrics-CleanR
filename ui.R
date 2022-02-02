library(shiny)
library(lubridate)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinythemes)
library(DT) #for better tables

#max file size 30mb for upload
options(shiny.maxRequestSize = 30*1024^2)


# Define UI for application that draws a histogram
shinyUI(fixedPage(theme = shinytheme("cosmo"),
                  
    #navbar
    navbarPage("Qualtrics Service Statistics Cleaner"),
                  
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
                      "2019" = "2019",
                      "2018" = "2018")),
        tags$hr(),
        # Button
        downloadButton("downloadConsults", "Download Consults"),
        helpText("To download clean Consults data"),
        downloadButton("downloadInstruction", "Download Instruction"),
        helpText("To download clean Instruction data"),
        downloadButton("downloadOutreach", "Download Outreach"),
        helpText("To download clean Outreach data"),
        downloadButton("downloadInfo_RAD", "Download Info/RAD"),
        helpText("To download clean Info / Research Assistance Desk Data"),
        #downloadDataGISLab
        downloadButton("downloadDataGISLab", "Download Data & GIS"),
        helpText("To download clean Data & GIS Lab Data"),
        tags$hr(),
        helpText("This is an applet to clean user uploaded service stats Qualtrics
                  data. Upload a Qualtrics .csv file using the button above. 
                  Summary statistics, plots and tables will be generated for
                  you automatically."),
        helpText("To subset your data by Quarter or Year 
                  (or both) you can use the \"Choose a Quarter\" and 
                 \"Choose a Year\" boxes."),
        helpText("Furthermore, the download buttons can be used to easily export
                 cleaned versions of your uploaded service stats. These data
                 exports are reactive to the selected quarter/year, so you can
                 filter as you wish, or set both options to \'All\' for all 
                 observations"),
        helpText("Tip: Plots are interactive! Hover your mouse over observations
                 to learn more about the data. You may also use the toolbars in 
                 the top-right corner of each plot to save the plot as an image
                 to your computer."),
        tags$hr() #,
        #helpText("Developed by Joshua Weimer for the UCSD Data and GIS Lab.")
        
      ), #end sidebar
      
      #Main Panel
      mainPanel(
        tabsetPanel(type='tabs',
                    tabPanel("Consults",
                             #tags$hr(),
                             h3("Consults Statistics"),
                             textOutput("consults_stats"),
                             tags$hr(),
                             plotlyOutput("intra_quarter_consults"),
                             tags$hr(),
                             plotlyOutput("consults_over_time"),
                             selectInput("consults_scale", "Choose an Aggregation Scale:", #this is our selector
                                         c("Weekly" = "Weekly",
                                           "Daily" = "Daily")
                             ),
                             tags$hr(),
                             plotlyOutput("consults_graph"),
                             numericInput("n","Minimum Count of Department:"
                                          ,100,min = 1),
                             tags$hr(),
                             #this plot is shy ??
                             #plotlyOutput("consult_locations"),
                             #tags$hr(),
                             plotlyOutput("consult_categories"),
                             numericInput("n_category","Minimum Count of Category:"
                                          ,100,min = 1),
                             tags$hr(),
                             DT::dataTableOutput("consults_DT")
                             ),
                    tabPanel("Instruction",
                             #tags$hr(),
                             h3("Instruction Statistics"),
                             textOutput("instruction_stats"),
                             tags$hr(),
                             plotlyOutput("intra_quarter_instruction"),
                             tags$hr(),
                             plotlyOutput("instruction_time_plot"),
                             selectInput("instruction_scale", "Choose an Aggregation Scale:", #this is our selector
                                         c("Weekly" = "Weekly",
                                           "Daily" = "Daily")
                             ),
                             tags$hr(),
                             DT::dataTableOutput("instruction_DT")
                             ),
                    tabPanel("Outreach",
                             #tags$hr(),
                             h3("Outreach Statistics"),
                             textOutput("outreach_stats"),
                             tags$hr(),
                             DT::dataTableOutput("outreach_DT"),
                             ),
                    tabPanel("Info / RAD",
                             #tags$hr(),
                             h3("RAD / Info Desk Statistics"),
                             textOutput("info_stats"),
                             tags$hr(),
                             plotlyOutput("info_time_plot"),
                             selectInput("info_scale", "Choose an Aggregation Scale:", #this is our selector
                                         c("Weekly" = "Weekly",
                                           "Daily" = "Daily")
                             ),
                             tags$hr(),
                             DT::dataTableOutput("serv_counts_DT"),
                             tags$hr(),
                             DT::dataTableOutput("info_DT")
                             ),
                    tabPanel("Data & GIS Lab",
                             #tags$hr(),
                             h3("Data & GIS Lab Statistics"),
                             textOutput("gis_stats"),
                             tags$hr(),
                             plotlyOutput("week_of_quarter_gis_lab"),
                             tags$hr(),
                             plotlyOutput("gis_lab_per_week"),
                             tags$hr(),
                             DT::dataTableOutput("data_gis_DT"))
                    )
      ) #end main panel   
    
))
