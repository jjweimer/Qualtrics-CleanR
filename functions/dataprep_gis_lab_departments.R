dataprep_gis_lab_departments <- function(df,is_fuzzy,n_gis){
  gis_lab <- df #this should always be Sortie_data_gis()
  user_choice <- is_fuzzy #this should always be input$is_fuzzy_gis
  user_n <- n_gis #always input$n_gis
  if(user_choice == "Matched"){
    gis_lab <- gis_lab[!is.na(gis_lab$fuzzy_department),]
    ##get dept counts
    dept_counts <- gis_lab %>% group_by(fuzzy_department) %>%
      count(fuzzy_department) %>% arrange(-n)
    colnames(dept_counts) <- c("department","n")
    #filter for n 
    dept_counts <- dept_counts[1:user_n,]
  } else if (user_choice == "Raw") {
    gis_lab <- gis_lab[!is.na(gis_lab$department),]
    ##get dept counts
    dept_counts <- gis_lab %>% group_by(department) %>%
      count(fuzzy_department) %>% arrange(-n)
    #filter for n 
    dept_counts <- dept_counts[1:user_n,]
  }
  return(dept_counts)
}