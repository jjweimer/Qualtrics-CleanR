fuzzy_match <- function(typo_list){
  #takes in column vector of department names
  #outputs column vector of matched names
  #crosswalks dept codes to dept names
  
  #define names to match to
  department_names <- c( 
    #UCSD departments
    "Academic Internship Program",
    "Alumni",
    "Anthropology",
    "Archaeology",
    "Art History",
    "Bioengineering",
    "Biological Sciences",
    "Biology",
    "Biochemistry",
    "CAT (Sixth College)",
    "Chemistry",
    "Chinese Studies",
    "Classical Studies",
    "Cognitive Science",
    "Communication",
    "Computer Science",
    "Critical Gender Studies",
    "Dance",
    "Data Science",
    "Economics",
    "Education Studies",
    "Electrical & Computer Engineering",
    "English",
    "Engineering",
    "Environmental Systems",
    "Ethnic Studies",
    'Extension',
    "First Year Experience",
    "Geosciences",
    "General",
    "German Studies",
    "Global Health",
    "Global Policy and Strategy",
    "Public Health",
    "History",
    'Human Development Sciences',
    "International Studies",
    "Italian Studies",
    "Japanese Studies",
    "Jewish Studies",
    "Latin American Studies",
    "Library staff",
    "Linguistics",
    "Literature",
    "Mathematics",
    "Medicine",
    "Mechanical & Aerospace Engineering",
    "Music",
    "Music Department",
    "Nanoengineering",
    "Office for Equity, Diversity & Inclusion",
    "Pharmacy",
    "Philosophy",
    "Physics",
    "Political Science",
    "Psychology",
    "Pyschiatry",
    "Rady School of Management",
    "Religion",
    "Russian and Soviet Studies",
    "Scripps Institute of Oceanography",
    "School of Medicine",
    "Sociology",
    "Spanish",
    "Structural Engineering",
    "Teaching and Learning Commons",
    "Theatre and Dance",
    "Urban Studies and Planning",
    "Visual Arts",
    
    #dept codes
    "ANTH",
    "BENG",
    "BIOL",
    "CAT",
    "CENG",
    "CGS",
    "CHEM",
    "CHIN",
    "COGS",
    "COMM",
    "CONT",
    "CSE",
    "DOC",
    "DSC",
    "ECE",
    "ECON",
    "EDS",
    "ENVR",
    "ERC",
    "ESYS",
    "ETHN",
    "FILM",
    "FPMU",
    "GPS",
    "HDP",
    "HIST",
    "HMNR",
    "HUM",
    "ICAM",
    "INTL",
    "JAPN",
    "JUDA",
    "LATI",
    "LAWS",
    "LING",
    "LIT",
    "MAE",
    "MATH",
    "MMW",
    "MUIR",
    "MCWP",
    "MUS",
    "MGMT",
    "NENG",
    "NANO",
    "POLI",
    "PHIL",
    "PHYS",
    "PSYC",
    "RELI",
    "REV",
    "RSM",
    "SE",
    "SIO",
    "SOC",
    "SOE",
    "STPA",
    "SXTH",
    "SUNY",
    "THEA",
    "TMC",
    "TWS",
    "USP",
    "VIS",
    "WARR",
    "WCWP",
    
    #UCSD colleges
    "Muir",
    "Marshall",
    "Warren",
    "Revelle",
    "ERC", "Roosevelt", 
    "Sixth",
    "Seventh",
    
    #institutions
    "SDSU",
    "SJSU",
    "USD",
    "UCSD Extension",
    "UCLA",
    "SDSC",
    
    #Health
    "Family Medicine",
    "Gastroenterology",
    "Health Strategy",
    "Neurology",
    "Neurosciences",
    "Nursing",
    "Pathology",
    "Radiology",
    "Shiley Eye Institute",
    "UCSD Health",
    "VA Nurse",
    "Infectious disease",
    
    #other (these are not ordered)
    "MPP", #master public policy
    "Off Campus",
    "non-UCSD",
    "San Diego Supercomputer Center",
    "SDCWA", #San Diego County Water Authority
    "Other",
    "NOAA - Southwest Fisheries Science Center",
    "NOAA - SWFSC", #same as previous obs
    "Center for Gender Equity & Health",
    "Triton Tree Trust",
    "Campus Planning",
    "HDSI",
    "HDS", #Human Development Sciences 
    "ACTRI",
    "Calit2",
    "JSOE",
    "Career Center",
    "HILA",
    "SSPPS",
    "Public",
    "Statistics",
    "GLBH",
    "ANP",
    "Korea",
    "LAS",
    "Community Member",
    "Center for U.S.-Mexican Studies",
    "DOC Writing Program",
    "Visiting Scholar"
  )
  
  indices <- stringdist::amatch(x = typo_list, #things we want matched 
                    table = department_names, #table of correct names
                    method = "jw", #jaro winkler methodology
                    maxDist = 0.3 #unsure ideal dist.
                    #strict seems better since our dict is so big 
                    )
  ## assign to new obj
  matched_departments <- department_names[indices]
  #crosswalk dept codes to dept names
  codes <- c("ANTH","BENG","BIOL",
             "CHEM","COGS","COMM","CSE","CENG",
             "DOC","DSC","ECE","ECON",
             "GPS","HDS","HDSI",
             "HIST","MATH","Music Department",
             "MAE","NANO","NENG", #NANO and NENG are both nanoengineering
             "SIO","USP","VIS","POLI")
  names <- c("Anthropology","Bioengineering","Biology",
            "Chemistry","Cognitive Science","Communication", "Computer Science", "Chemical Engineering",
            "DOC Writing Program","Data Science","Electrical & Computer Engineering","Economics",
            "Global Policy and Strategy","Human Development Sciences","Data Science",
            "History","Mathematics","Music",
            "Mechanical & Aerospace Engineering","Nanoengineering","Nanoengineering", #NANO and NENG are both nanoengineering
            "Scripps Institute of Oceanography","Urban Studies and Planning", "Visual Arts","Political Science")
  
  #loop through both lists and crosswalk
  for(i in 1:length(codes)){
    matched_departments[matched_departments == codes[i]] <- names[i]
  }
  #abbreviations to their full names
  matched_departments[matched_departments == "NOAA - SWFSC"] <- "NOAA - Southwest Fisheries Science Center"
  matched_departments[matched_departments == "SDSC"] <- "San Diego Supercomputer Center"
  matched_departments[matched_departments == "Extension"] <- "UCSD Extension"
  return(matched_departments)
}
