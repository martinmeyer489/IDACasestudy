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

#change formats (function becuase we need to do the same later:
set_formats <- function(x){
  x %>% mutate(
    Produktionsdatum = as.Date(Produktionsdatum),
    Herstellernummer = as.factor(Herstellernummer),
    Werksnummer = as.factor(Werksnummer),
    Fehlerhaft = as.factor(Fehlerhaft),
    Fehlerhaft_Datum = as.Date(Fehlerhaft_Datum),
    Fehlerhaft_Fahrleistung = as.numeric(Fehlerhaft_Fahrleistung)
  )
}
t02 <- set_formats(t02)

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

k1be1 <- set_formats(k1be1)
k1be2 <- set_formats(k1be2)
k1di1 <- set_formats(k1di1)
k1di2 <- set_formats(k1di2)
#did not work for k1di1 because of empty strings in Fehlerhaft_Datum. Therefore replace them with NAs:
k1di1 <- k1di1 %>%
  mutate(Fehlerhaft_Datum = if_else(Fehlerhaft_Datum == "",NA_character_ ,Fehlerhaft_Datum))

k1di1 <- set_formats(k1di1)

#remove unnecessary columns (parts that are not T2)
bestand_k1be1[c(1,3:4)] <- list(NULL)
bestand_k1be2[c(1,3:4)] <- list(NULL)
bestand_k1di1[c(1,3:4)] <- list(NULL)
bestand_k1di2[c(1,3:4)] <- list(NULL)

#join defected parts with components:
t02_defected_components <- t02_defected %>%
  select(ID_T02) %>%
  left_join(bestand_k1be1,by=c("ID_T02" = "ID_T2")) %>%
  left_join(bestand_k1be2,by=c("ID_T02" = "ID_T2")) %>%
  left_join(bestand_k1di1,by=c("ID_T02" = "ID_T2")) %>%
  left_join(bestand_k1di2,by=c("ID_T02" = "ID_T2"))

t02_defected_components <- t02_defected_components %>%
  unite("ID_Component", ID_K1BE1, ID_K1BE2, ID_K1DI1, ID_K1DI2, na.rm = TRUE)

bestand_fzg_oem1_typ11[c(1:3)] <- list(NULL)
bestand_fzg_oem1_typ12[c(1:3)] <- list(NULL)

t02_defected_vehicles <- t02_defected_components %>%
  left_join(bestand_fzg_oem1_typ11,by=c("ID_Component" = "ID_Motor")) %>%
  left_join(bestand_fzg_oem1_typ12,by=c("ID_Component" = "ID_Motor"))

t02_defected_vehicles <- t02_defected_vehicles %>%
  unite("ID_Vehicle", ID_Fahrzeug.x, ID_Fahrzeug.y, na.rm = TRUE)
  
#I think for tidy data we must check earlier that Herstellernummer and first three digits of Werksnummer are equal. Or is that only for parts, not components?
#also we dont need most of the columns! Maybe unnecessary to put them in the right format.
