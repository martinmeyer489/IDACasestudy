library(dplyr)
library(tidyverse)

# Dataset T2 because it is the affected.
# colClasses must be looked at again I think. which ones need to be factor? maybe change format of dates?
t02 <- read.table(text = gsub("\t", "\n", readLines('./Data/Einzelteil/Einzelteil_T02.txt')))
#the import of t02 said unvollständige letzte Zeile but we checked that manually and the last row was included

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

# selected the vehicles which were stated in the text.
bestand_fzg_oem1_typ11 <- read.csv2(row.names = 1, './Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv')
bestand_fzg_oem1_typ12 <- read.csv2(row.names = 1, './Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv')
fzg_oem1_typ11 <- read.csv(row.names = 1, './Data/Fahrzeug/Fahrzeuge_OEM1_Typ11.csv')
fzg_oem1_typ12 <- read.csv2(row.names = 1, './Data/Fahrzeug/Fahrzeuge_OEM1_Typ12.csv')

#next we have to import the Zulassungen of the vehicles:
zulassungen <- read.csv2(row.names = 1, './Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv')

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

#tidy up zulassungen: column zulassung as date:
zulassungen <- zulassungen %>%
  mutate(Zulassung = as.Date(Zulassung))



#what about the parts which are already labelled as "fehlerhaft"?

#create dataset of T2 that only contains the defected parts. changed dates from 31 to 30 because april and november both only have 30 days.
t02_affected <- t02 %>%
  filter((Herstellernummer == 202 & Werksnummer == 2022 & Produktionsdatum >= "2009-04-30" & Produktionsdatum <= "2014-11-30") | (Herstellernummer == 201 & Werksnummer == 2011 & ID_number >= 1250 & ID_number <= 19500))

#join affected parts with components:
t02_affected_components <- t02_affected %>%
  select(ID_T02) %>%
  left_join(bestand_k1be1,by=c("ID_T02" = "ID_T2")) %>%
  left_join(bestand_k1be2,by=c("ID_T02" = "ID_T2")) %>%
  left_join(bestand_k1di1,by=c("ID_T02" = "ID_T2")) %>%
  left_join(bestand_k1di2,by=c("ID_T02" = "ID_T2"))

t02_affected_components <- t02_affected_components %>%
  unite("ID_Component", ID_K1BE1, ID_K1BE2, ID_K1DI1, ID_K1DI2, na.rm = TRUE)

bestand_fzg_oem1_typ11[c(1:3)] <- list(NULL)
bestand_fzg_oem1_typ12[c(1:3)] <- list(NULL)

#join affected vehicles, unite ID_columns from Type11 and Type 12 and change format to factors to see if every part is in one component and every component in one vehicle:
t02_affected_vehicles <- t02_affected_components %>%
  left_join(bestand_fzg_oem1_typ11,by=c("ID_Component" = "ID_Motor")) %>%
  left_join(bestand_fzg_oem1_typ12,by=c("ID_Component" = "ID_Motor")) %>%
  unite("ID_Vehicle", ID_Fahrzeug.x, ID_Fahrzeug.y, na.rm = TRUE) %>%
  mutate(ID_T02 = as.factor(ID_T02)) %>%
  mutate(ID_Component = as.factor(ID_Component)) %>%
  mutate(ID_Vehicle = as.factor(ID_Vehicle))

summary(t02_affected_vehicles)
#summary shows that 405621 components could not be found in any vehicle of type 11 or type 12. why?? >> k1be2 and k1di2 are not in vehicles from OEM1, only OEM2!

#the following filter shows that all components with 1 at the end were found in a vehicle.
t02_affected_vehicles %>%
  filter(str_detect(ID_Component, "K1BE1|K1DI1")) %>%
  summary()

t02_affected_zulassungen <- t02_affected_vehicles %>%
  left_join(zulassungen, by = c("ID_Vehicle" = "IDNummer")) %>%
  mutate(ID_Vehicle = if_else(ID_Vehicle == "",NA_character_ ,ID_Vehicle)) %>%
  mutate(ID_Vehicle = as.factor(ID_Vehicle))

#the following filter shows that each vehicle was connected to a gemeinde-
t02_affected_zulassungen %>%
  filter(!is.na(ID_Vehicle) & is.na(Gemeinden))


#I think for tidy data we must check earlier that Herstellernummer and first three digits of Werksnummer are equal. Or is that only for parts, not components? ...but it seems to be alright because every part is in ine component and every component is in one vehicle.
#also we dont need most of the columns! Maybe unnecessary to put them in the right format.
