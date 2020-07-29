if(!require("install.load")){
  install.packages("install.load")
}
library('install.load')

install_load("stringr","dplyr", "data.table", "tidyverse","plotly")

#read first 50 lines
#Teil_2 <- readLines("Data/Data/Einzelteil/Einzelteil_T02.txt",n=2,warn=FALSE)
#print(Teil_2[1:50])

#str_replace_all(. , " \", ";") %>%




Teil_2 <- read.csv("Data/Data/Einzelteil/Einzelteil_T02.txt",nrows=50,sep="\ ")
Bestandteile_Typ_12 <- read.csv("Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv",nrows=50,sep=";") 
Fahrzeuge_Typ_12 <- read.csv("Data/Data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv",nrows=50,sep=";")
