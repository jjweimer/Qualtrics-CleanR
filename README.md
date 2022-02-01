# Qualtrics-Cleaner
This is a Shiny app to automate the process of data cleaning and summary statistics for the UCSD Library's Service Stats Qualtrics export

- server.R: all of the server code
- ui.R: UI code for server elements
- app.R: not necessary to run the app, but is an alternative method. Sources the ui and server files.
- dictionary.md: data dictionary for qualtrics export
- fuzzy_match.R: fuzzy matching for department names (WIP)
- week_quarter_helper.R: hlper functions for converting between weeks of year / qaurter / defining quarters
