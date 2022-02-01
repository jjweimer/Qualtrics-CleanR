library(stringdist)
library(dplyr)

#define department names to match to
department_names <- c("Aerospace Engineering",
                      "Anthropology",
                      "Bioengineering",
                      "Biological Sciences",
                      "Biochemistry",
                      "Chemistry",
                      "Chinese Studies",
                      "Classical Studies",
                      "Cognitive Science",
                      "Communication",
                      "Conputer Science",
                      "Critical Gender Studies",
                      "Dance",
                      "Data Science",
                      "Economics",
                      "Education Studies",
                      "Electrical & Computer Engineering",
                      "English",
                      "Environmental Systems",
                      "Ethnic Studies",
                      "Geosciences",
                      "German Studies",
                      "Global Health",
                      "Global South Studies",
                      "Public Health",
                      "History",
                      'Human Development Sciences',
                      "International Studies",
                      "Italian Studies",
                      "Japanese Studies",
                      "Jewish Studies",
                      "Latin American Studies",
                      "Linguistics",
                      "Literature",
                      "Mathematics",
                      "Mechanical Engineering",
                      "Music",
                      "Nanoengineering",
                      "Philosiphy",
                      "Physics",
                      "Political Science",
                      "Psychology",
                      "Rady School of Management",
                      "Religion",
                      "Russian and Soviet Studies",
                      "Scripps Institute of Oceanography",
                      "Sociology",
                      "Structural Engineering",
                      "Theatre and Dance",
                      "Urban Studies and Planning",
                      "Visual Arts"
                      )


fuzzy_match <- function(typo_list){
  
  #amatch from stringdist::
  indices <- amatch(x = typo_list, #things we want matched / regularized
                    table = department_names, #table of correct namse
                    method = "jw", #jaro winkler mathodology
                    maxDist = 5)
  
  ## assign to new obj
  matched_departments <- department_names[indices]
  
  return(matched_departments)
  
}

#now lets test how well it works
my_list <- c("econoMic", "Poli Science", "Psycology", "Dat SciENCE", "Busines", "Bio","physicSS", "Aero Engineering", "Rady")
fuzzy_match(my_list)

