if(!require("install.load")){
  install.packages("install.load")
}
library('install.load')

install_load("stringr", "data.table", "tidyverse","plotly", "fitdistrplus", "tidyr")


#read in files
Production_K7 <- fread(file="Data/Data/Logistikverzug/Komponente_K7.csv") %>%
  as_tibble() %>%
  mutate(Produktionsdatum=as.Date(Produktionsdatum))%>%
  rename(production_date=Produktionsdatum) %>%
  rename(IDNumber=IDNummer)
head(Production_K7)

Delivery_K7 <- fread(file="Data/Data/Logistikverzug/Logistics_delay_K7.csv") %>%
  as_tibble() %>%
  mutate(Wareneingang=as.Date(Wareneingang)) %>%
  rename(good_arrrival=Wareneingang) %>%
  rename(IDNumber=IDNummer)
head(Delivery_K7)



#Filter for relevant columns for the analysis
Production_K7 <-Production_K7[,c("IDNumber","production_date")]

Delivery_K7 <-Delivery_K7[,c("IDNumber","good_arrrival")]

#join both datasets
Prod_Deli_join <- Production_K7 %>%
  left_join(Delivery_K7,by="IDNumber")
head(Prod_Deli_join)


#calculate difference between delivery and produciton
difference <- as.numeric(difftime(Prod_Deli_join$good_arrrival, Prod_Deli_join$production_date, units = "days"))

# add difference colomn to joined dataset 
Logistics_delay <- Prod_Deli_join %>%
  mutate(difference=difference)
head(Logistics_delay)

#plot emperical distrubtion
plotdist(difference, histo = TRUE, demp = T)

#gain overview with cullen and frey graph
descdist(difference, discrete = T)

#fit different distrubtions to the data 
fit_w  <- fitdist(difference, "weibull")
fit_g  <- fitdist(difference, "gamma")
fit_ln <- fitdist(difference, "lnorm")


#plot distrubtions
par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")

denscomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)
qqcomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)
cdfcomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)
ppcomp(list(fit_w, fit_ln, fit_g), legendtext = plot.legend)
gofstat(list(fit_w, fit_g, fit_ln, fit_n))


#determine  mean, max and min of logistics delay
print(summary(Logistics_delay$difference))


#plot hist and density function with normalization??
#fig <- plot_ly(x=difference, type = "histogram", histnorm = "probability")
#fig


#task 3
#create list of file correspoding to the parts for the components
components_parts <- list.files("Data/Data/Komponente", full.names = T, pattern = "Bestandteile")
head(components_parts)

#create function to find the compoments which use part 4
task_3_1 <- function(x){
  df <- fread(file = x, header = T) #read file 
  if (any(str_detect(colnames(df),"T4$")==TRUE)){
    df %>%
      dplyr::select(ID_T4, contains("K")) # contains k 
  }
}

b <- bind_rows(lapply(components_parts,task_3_1))
  
#read in files 
OEM1_11 <- fread(file = "Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv",header= T) %>%
  as_tibble() %>%
  filter(str_detect(ID_Motor,"^K1BE1")) %>%
  dplyr::select(ID_Motor,ID_Fahrzeug) %>%
  rename(ID_Engine=ID_Motor) %>%
  rename(ID_Vehicle=ID_Fahrzeug)


OEM1_12 <- fread(file = "Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv",header= T) %>%
  as_tibble() %>%
  filter(str_detect(ID_Motor,"^K1BE1")) %>%
  dplyr::select(ID_Motor,ID_Fahrzeug) %>%
  rename(ID_Engine=ID_Motor) %>%
  rename(ID_Vehicle=ID_Fahrzeug)



vehicles <- bind_rows(OEM1_11,OEM1_12)  

#read in 
registration <-fread(file="Data/Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv",header = T) %>%
  as_tibble() %>%
  mutate(Zulassung = as.Date(Zulassung)) %>%
  rename(registration_date=Zulassung) %>%
  rename(IDNumber=IDNummer)%>%
  rename(municipalities=Gemeinden)

head(registration)


vehicles_dortmund <- registration %>%
  filter(str_detect(municipalities,"DORTMUND")) %>%
  semi_join(vehicles, by=c("IDNumber"="ID_Vehicle"))

head(vehicles_dortmund)
nrow(vehicles_dortmund)




#task 6

#read compoments table for OEM 2 type 21
OEM2_21 <- fread(file = "Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv",header= T) %>%
  as_tibble()

#read compoments table for OEM 2 type 212
OEM2_22 <- fread(file = "Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv",header= T) %>%
  as_tibble()

#bind the rows of the files together
total_vehicles <- bind_rows(OEM2_21,OEM2_22) %>%
  rename(ID_Engine=ID_Motor) %>%
  rename(ID_Vehicle=ID_Fahrzeug)


#find vehicle with Engine ID
driver <- total_vehicles%>%
  filter(str_detect(ID_Engine,"K1DI2-103-1031-21")) %>%
  right_join(registration, by= c("ID_Vehicle"="IDNumber"))
