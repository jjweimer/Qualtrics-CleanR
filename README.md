# Qualtrics-Cleaner
This is a Shiny app for automated personal insights from the UCSD Library's Service Statistics Qualtrics export.

## Use and Access
The app can be accessed by visiting http://lib-shiny.ucsd.edu/qualtrics-personal-insights/ (May require UCSD VPN). Usage requires access to the service statistics qualtrics export. Documentation for acquiring this export can be found within the app. 

## Dependencies
- shiny
- lubridate
- dplyr
- plotly
- gpglot2
- DT
- rintrojs
- shinycssloaders
- bslib
- stringdist

To install all dependencies:
```
install.packages(c("shiny","lubridate","dplyr","plotly","ggplot2","DT",
                   "rintrojs","shinycssloaders","bslib","stringdist"))
```
