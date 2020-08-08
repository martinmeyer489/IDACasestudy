library(dplyr)
library(tidyverse)

# Dataset T2 because it is the affected.
# colClasses must be looked at again I think. which ones need to be factor? maybe change format of dates?
t02 <- read.table(text = gsub("\t", "\n", readLines('./Data/Einzelteil/Einzelteil_T02.txt')))
#the import of t02 said unvollständige letzte Zeile but we checked that manually and the last row was included

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
#the import of k1di2 said unvollständige letzte Zeile but we checked that manually and the last row was included

#tidy data?
#the following filter shows that each row contains only values for x or for y, not both.
t02 %>%
  filter(is.na(ID_T02.x) & is.na(ID_T02.y))

#that means we can unite the x and y columns and remove the "NA"s.
t02 <- t02 %>%
  unite("ID_T02",ID_T02.x,ID_T02.y, na.rm = TRUE) %>%
  unite("Produktionsdatum",Produktionsdatum.x,Produktionsdatum.y, na.rm = TRUE) %>%
  unite("Herstellernummer",Herstellernummer.x,Herstellernummer.y, na.rm = TRUE) %>%
  unite("Werksnummer",Werksnummer.x,Werksnummer.y, na.rm = TRUE) %>%
  unite("Fehlerhaft",Fehlerhaft.x,Fehlerhaft.y, na.rm = TRUE) %>%
  unite("Fehlerhaft_Datum",Fehlerhaft_Datum.x,Fehlerhaft_Datum.y, na.rm = TRUE) %>%
  unite("Fehlerhaft_Fahrleistung",Fehlerhaft_Fahrleistung.x,Fehlerhaft_Fahrleistung.y, na.rm = TRUE)

#change formats:
t02$Produktionsdatum <- as.Date(t02$Produktionsdatum)
t02$Herstellernummer <- as.factor(t02$Herstellernummer)
t02$Werksnummer <- as.factor(t02$Werksnummer)
t02$Fehlerhaft <- as.integer(t02$Fehlerhaft)
t02$Fehlerhaft_Datum <- as.Date(t02$Fehlerhaft_Datum)
t02$Fehlerhaft_Fahrleistung <- as.numeric(t02$Fehlerhaft_Fahrleistung)

#create ID_number that contains the last part of the IDs as integer:
t02 <- mutate(t02,ID_number = as.integer(gsub("^.*-", "", ID_T02)))

#what about the parts which are already labelled as "fehlerhaft"?

#create dataset of T2 that only contains the defected parts. changed dates from 31 to 30 because april and november both only have 30 days.
t02_defected <- t02 %>%
  filter((Herstellernummer == 202 & Werksnummer == 2022 & Produktionsdatum >= "2009-04-30" & Produktionsdatum <= "2014-11-30") | (Herstellernummer == 201 & Werksnummer == 2011 & ID_number >= 1250 & ID_number <= 19500))

#In the next step we want to integrate the component data. Therefore we need to tidy it before.
#Data set k1di1 has the same columns for x, y and without an index. The following filters show that each object has only a value in one of the three ID columns.
k1di1 %>%
  filter(is.na(ID_Motor.x) & is.na(ID_Motor.y) & is.na(ID_Motor))
k1di1 %>%
  filter(!is.na(ID_Motor.x) & !is.na(ID_Motor.y))
k1di1 %>%
  filter(!is.na(ID_Motor.x) & !is.na(ID_Motor))
k1di1 %>%
  filter(!is.na(ID_Motor) & !is.na(ID_Motor.y))

#k1di1 three columns can be united to one:
k1di1 <- k1di1 %>%
  unite("ID_Motor",ID_Motor,ID_Motor.x,ID_Motor.y, na.rm = TRUE) %>%
  unite("Produktionsdatum",Produktionsdatum,Produktionsdatum.x,Produktionsdatum.y, na.rm = TRUE) %>%
  unite("Herstellernummer",Herstellernummer,Herstellernummer.x,Herstellernummer.y, na.rm = TRUE) %>%
  unite("Werksnummer",Werksnummer,Werksnummer.x,Werksnummer.y, na.rm = TRUE) %>%
  unite("Fehlerhaft",Fehlerhaft,Fehlerhaft.x,Fehlerhaft.y, na.rm = TRUE) %>%
  unite("Fehlerhaft_Datum",Fehlerhaft_Datum,Fehlerhaft_Datum.x,Fehlerhaft_Datum.y, na.rm = TRUE) %>%
  unite("Fehlerhaft_Fahrleistung",Fehlerhaft_Fahrleistung,Fehlerhaft_Fahrleistung.x,Fehlerhaft_Fahrleistung.y, na.rm = TRUE)

k1be1 <- k1be1 %>% rename(Produktionsdatum=origin,Origin=Produktionsdatum_Origin_01011970)
k1be2 <- k1be2 %>% rename(Produktionsdatum=origin,Origin=Produktionsdatum_Origin_01011970)
k1di2 <- k1di2 %>% rename(Produktionsdatum=origin,Origin=Produktionsdatum_Origin_01011970)