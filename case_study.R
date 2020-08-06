#install needed packages
if(!require("install.load")){
  install.packages("install.load")
  }
library('install.load')
install_load("stringr",
             "dplyr", 
             "data.table", 
             "tidyverse",
             "plotly")

#function to get the lineseperator 
get_linesep <- function(x) {
  #define two string variables, which used to find the line seperator
  regex_linesep1 <- "[\a\b\f\v\r]" 
  regex_linesep2 <- "[\t]"
  #check if one of the regex are used in the string file
  if(str_detect(x, regex_linesep1)) {
    linesep <- str_extract(x, regex_linesep1)
  } else if(str_detect(x, regex_linesep2)){
    linesep <- str_extract(x, regex_linesep2)
  }  else {
    linesep <- " "
  }
  linesep
}
#function to read the textfile x
read_textfile <- function(x, nrows = Inf) {
  #replace the unicode characters by 
  content <- read_file(file = x) %>%
    str_replace_all(" \\| \\| ", "\\|") %>%
    str_replace_all("\u0020{2}", "!") %>%
    str_replace_all("I{2}", "!")
  linesep <- get_linesep(content)
  
  end_header <- str_locate(content, "(\"1\")")[1]
  str_header <- str_sub(content, start = 1, end = end_header - 2)
  
  regex_colsep <- "\\\\|\\||!|\t"
  colsep <- str_extract(str_header, regex_colsep)
  if (is.na(colsep)) {
    colsep <- str_extract(str_header, "[:blank:]")
    }
  if (colsep == linesep) {
    linesep <- " "
    }
  if (colsep == "|") {
    colsep <- "\\|"
    }
  if (colsep == "\\") {
    colsep <- "\\\\"
    }
  
  content <- str_replace_all(content, linesep, "\n") %>%
    str_replace_all("\"\"", "\"\n\"") %>%
    str_replace_all("NA\"", "NA\n\"") %>%
    str_replace_all(colsep, ",")
  
  write_file(content, path = file.path(getwd(), "current_file.txt"),append = F)
  df <- fread("current_file.txt", nrows = nrows)
  df
}
#read files  
T2 <- read_textfile("Data/Data/Einzelteil/Einzelteil_T02.txt")
Bestandteile_Typ_12 <- read.csv("Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv",sep=";") 
Fahrzeuge_Typ_12 <- read.csv("Data/Data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv",sep=";")

#edit T2
T_2 <-T2 %>% mutate(
  ID = coalesce(ID_T02.x,ID_T02.y),
  Produktionsdatum = coalesce(Produktionsdatum.x,Produktionsdatum.y),
  Herstellernummer= coalesce(Herstellernummer.x,Herstellernummer.y),
  Werksnummer = coalesce(Werksnummer.x,Werksnummer.y),
  Fehlerhaft = coalesce(Fehlerhaft.x,Fehlerhaft.y),
  Fehlerhaft_Datum = coalesce(Fehlerhaft_Datum.x,Fehlerhaft_Datum.y),
  Fehlerhaft_Fahrleistung = coalesce(Fehlerhaft_Fahrleistung.x,Fehlerhaft_Fahrleistung.y)
  ) 
  #select(ID,Produktionsdatum,Herstellernummer,Werksnummer,Fehlerhaft==1,Fehlerhaft_Datum,Fehlerhaft_Fahrleistung)


