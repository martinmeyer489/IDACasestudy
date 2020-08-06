library(dplyr)
library(tidyverse)

# Dataset T2 because it is the affected.
# colClasses must be looked at again I think. which ones need to be factor? maybe change format of dates?
t02 <- read.table(colClasses = c("character","integer","factor","Date","factor","factor","factor","Date","numeric","factor","Date","factor","factor","factor","Date","numeric"), text = gsub("\t", "\n", readLines('./Data/Einzelteil/Einzelteil_T02.txt')))

# selected the vehicles which were stated in the text.
bestand_fzg_oem1_typ11 <- read.csv2(row.names = 1, './Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv')
bestand_fzg_oem1_typ12 <- read.csv2(row.names = 1, './Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv')
fzg_oem1_typ11 <- read.csv(row.names = 1, './Data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv')
fzg_oem1_typ12 <- read.csv2(row.names = 1, './Data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv')

# imported all components which included T2 parts (which had a column 'T2').
bestand_k1be1 <- read.csv2(row.names = 1, './Data/Komponente/Bestandteile_Komponente_K1BE1.csv')
bestand_k1be2 <- read.csv2(row.names = 1, './Data/Komponente/Bestandteile_Komponente_K1BE2.csv')
bestand_k1di1 <- read.csv2(row.names = 1, './Data/Komponente/Bestandteile_Komponente_K1DI1.csv')
bestand_k1di2 <- read.csv2(row.names = 1, './Data/Komponente/Bestandteile_Komponente_K1DI2.csv')
k1be1 <- read.csv(row.names = 1, './Data/Komponente/Komponente_K1BE1.csv')
k1be2 <- read.csv2(row.names = 1, './Data/Komponente/Komponente_K1BE2.csv')
k1di1 <- read.csv(row.names = 1, './Data/Komponente/Komponente_K1DI1.csv')
k1di2 <- read.table(sep = "\\", text = gsub("\t", "\n", readLines('./Data/Komponente/Komponente_K1DI2.txt')))

#tidy data?
#the following filter shows that each row contains only values for x or for y, not both.
t02 %>%
  filter(is.na(ID_T02.x) & is.na(ID_T02.y))

#that means we can unite the x and y columns and remove the "NA"s.
t02 <- t02 %>% mutate(
  ID = coalesce(ID_T02.x,ID_T02.y),
  Produktionsdatum = coalesce(Produktionsdatum.x,Produktionsdatum.y),
  Herstellernummer= coalesce(Herstellernummer.x,Herstellernummer.y),
  Werksnummer = coalesce(Werksnummer.x,Werksnummer.y),
  Fehlerhaft = coalesce(Fehlerhaft.x,Fehlerhaft.y),
  Fehlerhaft_Datum = coalesce(Fehlerhaft_Datum.x,Fehlerhaft_Datum.y),
  Fehlerhaft_Fahrleistung = coalesce(Fehlerhaft_Fahrleistung.x,Fehlerhaft_Fahrleistung.y)
)


#what about the parts which are already labelled as "fehlerhaft"?

#create dataset of T2 that only contains the defected parts
t02_defected <- t02 %>%
  filter(Herstellernummer == 202 & Werksnummer == 2022 & Produktionsdatum > "31.04.2009" & Produktionsdatum < "31.11.2014")