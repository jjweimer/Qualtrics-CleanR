library(stringdist)
library(dplyr)

#define department names to match to
department_names <- c("Aerospace Engineering",
                      "Academic Internship Program",
                      "Alumni",
                      "Anthropology",
                      "Archaeology",
                      "Bioengineering",
                      "Biological Sciences",
                      "Biochemistry",
                      "CAT (Sixth College)",
                      "Chemistry",
                      "Chinese Studies",
                      "Classical Studies",
                      "Cognitive Science",
                      "Communication",
                      "Computer Science",
                      "Critical Gender Studies",
                      #"Dean",
                      "Dance",
                      "Data Science",
                      "Economics",
                      "Education Studies",
                      "Electrical & Computer Engineering",
                      "English",
                      "Environmental Systems",
                      "Ethnic Studies",
                      'Extension',
                      "First Year Experience",
                      "Geosciences",
                      "German Studies",
                      "Global Health",
                      "Global Policy and Strategy",
                      "GPS",
                      "Public Health",
                      "History",
                      'Human Development Sciences',
                      "HUM",
                      "International Studies",
                      "Italian Studies",
                      "Japanese Studies",
                      "Jewish Studies",
                      "Latin American Studies",
                      "Library staff",
                      "Linguistics",
                      "Literature",
                      "Mathematics",
                      "MCWP",
                      "MMW",
                      "Medicine",
                      "Mechanical Engineering",
                      "Music",
                      "Nanoengineering",
                      "Nursing",
                      "Office for Equity, Diversity & Inclusion",
                      "Pharmacy",
                      "Philosiphy",
                      "Physics",
                      "Political Science",
                      "Psychology",
                      "Rady School of Management",
                      "Religion",
                      "Russian and Soviet Studies",
                      "Scripps Institute of Oceanography",
                      "School of Medicine",
                      "SIO",
                      "Sociology",
                      "Structural Engineering",
                      "Teaching and Learning Commons",
                      "Theatre and Dance",
                      "UCSD Health",
                      "Urban Studies and Planning",
                      "Visual Arts",
                      
                      #other
                      
                      "Off Campus",
                      "non-UCSD"
                      )


fuzzy_match <- function(typo_list){
  
  #amatch from stringdist::
  indices <- amatch(x = typo_list, #things we want matched / regularized
                    table = department_names, #table of correct namse
                    method = "jw", #jaro winkler mathodology
                    maxDist = 0.25)
  
  ## assign to new obj
  matched_departments <- department_names[indices]
  
  return(matched_departments)
  
}

#now lets test how well it works
my_list <- c("econoMic", "Poli Science", "Psycology", "Dat SciENCE", "Busines", "Bio","physicSS", "Aero Engineering", "Rady")
fuzzy_match(my_list)

