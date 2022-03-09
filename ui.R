library(shiny)
library(lubridate)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinythemes)
library(DT) #for better tables
library(rintrojs) #js library for intro
library(shinycssloaders)

#max file size 30mb for upload
options(shiny.maxRequestSize = 30*1024^2)
#spinner options
options(spinner.type = 3,
        spinner.color.background  = "#ffffff")

#UI
shinyUI(fixedPage(introjsUI(),
                  theme = shinytheme("cosmo"),
                  
    #navbar
    introBox(
      navbarPage("Qualtrics Service Stats CleanR"),
      data.step = 1,
      data.intro = "This is an app for cleaning the 
                    Qualtrics Service Stats Export."
    ),
    
    # Sidebar
      sidebarPanel(
        actionButton("help","Press here for Tutorial"), #tutorial button
        tags$hr(),
        introBox(
          fileInput("file1", "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
          data.step = 2,
          data.intro = "Upload your Qualtrics .csv export here."
        ),
        introBox(
          selectInput("quarter", "Quarter:", #this is our selector
                      c("All" = "All",
                        "Winter" = "WI",
                        "Spring" = "SP",
                        "Summer" = "SU",
                        "Fall" = "FA",
                        "Break" = "Break")
          ),
          selectInput("year", "Year:", #this is our selector
                      c("All" = "All",
                        "2022" = "2022",
                        "2021" = "2021",
                        "2020" = "2020",
                        "2019" = "2019",
                        "2018" = "2018")
          ),
          #introbox args
          data.step = 3,
          data.intro = "Use these dropdowns to filter data by quarter or year."
        ),
        tags$hr(),
        introBox(
          h3("Data Export"),
          downloadButton("downloadConsults", "Download Consults"),
          helpText("Clean Consults data"),
          downloadButton("downloadInstruction", "Download Instruction"),
          helpText("Clean Instruction data"),
          downloadButton("downloadOutreach", "Download Outreach"),
          helpText("Clean Outreach data"),
          downloadButton("downloadInfo_RAD", "Download Info/RAD"),
          helpText("Clean Info / RAD data"),
          downloadButton("downloadDataGISLab", "Download Data & GIS"),
          helpText("Clean Data & GIS Lab Data"),
          
          #introbox args
          data.step = 5,
          data.intro = "Use these buttons to export cleaned version of the data.
          These exports are reactive to the quarter and year filters above."
        )
        
        
      ), #end sidebar
      
      #Main Panel
      mainPanel(
          tabsetPanel(type='tabs',
                      tabPanel("Consults",
                               #tags$hr(),
                               introBox(
                                 h3("Consults Statistics"),
                                 textOutput("consults_stats"),
                                 tags$hr(),
                                 plotlyOutput("intra_quarter_consults") %>% withSpinner(),
                                 selectInput("consults_position", "Position:",
                                             c("Stacked" = "stack",
                                               "Dodged" = "dodge")
                                 ),
                                 tags$hr(),
                                 #introbox args
                                 data.step = 4,
                                 data.intro = "These are the tabs where plots and statistics will be generated.
                                               They are seperated by service category for easier viewing."
                               ),
                               plotlyOutput("consults_over_time") %>% withSpinner(),
                               selectInput("consults_scale", "Aggregation Scale:",
                                           c("Weekly" = "Weekly",
                                             "Daily" = "Daily")
                               ),
                               tags$hr(),
                               plotlyOutput("consults_graph") %>% withSpinner(),
                               numericInput("n","Top n Departments:"
                                            ,10,min = 1),
                               selectInput("is_fuzzy", "Select Matched or Raw Departments:", 
                                           c("Matched" = "Matched",
                                             "Raw" = "Raw")
                               ),
                               tags$hr(),
                               #this plot is shy ??
                               #plotlyOutput("consult_locations"),
                               #tags$hr(),
                               plotlyOutput("consult_categories") %>% withSpinner(),
                               numericInput("n_category","Top n Categories:"
                                            ,10,min = 1),
                               tags$hr(),
                               DT::dataTableOutput("consults_DT") %>% withSpinner()
                               ),
                      tabPanel("Instruction",
                               #tags$hr(),
                               h3("Instruction Statistics"),
                               textOutput("instruction_stats"),
                               tags$hr(),
                               plotlyOutput("intra_quarter_instruction") %>% withSpinner(),
                               selectInput("instruction_position", "Position:",
                                           c("Stacked" = "stack",
                                             "Dodged" = "dodge")
                               ),
                               tags$hr(),
                               plotlyOutput("instruction_time_plot") %>% withSpinner(),
                               selectInput("instruction_scale", "Aggregation Scale:", 
                                           c("Weekly" = "Weekly",
                                             "Daily" = "Daily")
                                           ),
                               selectInput("instruction_format_selector_2", "Format:",
                                           c("All Formats" = "All",
                                             "Online" = "Online only",
                                             "In Person" = "In-person only",
                                             "Hybrid" = "Hybrid (partially online, partially in-person)")
                                           ),
                               tags$hr(),
                               plotlyOutput("instruction_num_people_time") %>% withSpinner(),
                               selectInput("instruction_format_selector", "Format:",
                                           c("All Formats" = "All",
                                             "Online" = "Online only",
                                             "In Person" = "In-person only",
                                             "Hybrid" = "Hybrid (partially online, partially in-person)")
                                           ),
                               tags$hr(),
                               DT::dataTableOutput("instruction_DT") %>% withSpinner()
                               ),
                      tabPanel("Outreach",
                               #tags$hr(),
                               h3("Outreach Statistics"),
                               textOutput("outreach_stats"),
                               tags$hr(),
                               DT::dataTableOutput("outreach_DT") %>% withSpinner(),
                               ),
                      tabPanel("Info / RAD",
                               #tags$hr(),
                               h3("RAD / Info Desk Statistics"),
                               textOutput("info_stats"),
                               tags$hr(),
                               plotlyOutput("intra_quarter_RAD") %>% withSpinner(),
                               selectInput("info_position", "Position:",
                                           c("Stacked" = "stack",
                                             "Dodged" = "dodge")
                               ),
                               tags$hr(),
                               plotlyOutput("info_time_plot") %>% withSpinner(),
                               selectInput("info_scale", "Aggregation Scale:", 
                                           c("Weekly" = "Weekly",
                                             "Daily" = "Daily")
                               ),
                               tags$hr(),
                               h3("Service Type Counts"),
                               DT::dataTableOutput("serv_counts_DT") %>% withSpinner(),
                               tags$hr(),
                               DT::dataTableOutput("info_DT") %>% withSpinner()
                               ),
                      tabPanel("Data & GIS Lab",
                               #tags$hr(),
                               h3("Data & GIS Lab Statistics"),
                               textOutput("gis_stats"),
                               tags$hr(),
                               plotlyOutput("gis_lab_hourly") %>% withSpinner(),
                               tags$hr(),
                               plotlyOutput("week_of_quarter_gis_lab") %>% withSpinner(),
                               selectInput("gis_position", "Position:",
                                           c("Stacked" = "stack",
                                             "Dodged" = "dodge")
                               ),
                               tags$hr(),
                               plotlyOutput("gis_lab_per_week") %>% withSpinner(),
                               tags$hr(),
                               plotlyOutput("gis_lab_departments") %>% withSpinner(),
                               numericInput("n_gis","Top n Departments:"
                                            ,10,min = 1),
                               selectInput("is_fuzzy_gis", "Select Matched or Raw Departments:", 
                                           c("Matched" = "Matched",
                                             "Raw" = "Raw")
                               ),
                               tags$hr(),
                               DT::dataTableOutput("data_gis_DT") %>% withSpinner()
                               )
          ), #end tabs
      ) #end main panel   
    
))
