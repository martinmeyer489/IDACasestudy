if(!require("install.load")){
  install.packages("install.load")
}
library('install.load')

install_load("stringr", "data.table", "tidyverse","plotly", "fitdistrplus", "tidyr")


#fread erlaubt??
Production_K7 <- fread(file="Data/Data/Logistikverzug/Komponente_K7.csv")
head(Production_K7)

Delivery_K7 <- fread(file="Data/Data/Logistikverzug/Logistics_delay_K7.csv")
head(Delivery_K7)



#Filter for relevant columns for the analysis
#Q: V1 relevant ??
Production_K7 <- Production_K7[,c("IDNummer","Produktionsdatum")]
head(Production_K7)

Delivery_K7 <- Delivery_K7[,c("IDNummer","Wareneingang")]
head(Delivery_K7)

#join both datasets
Prod_Deli_join <- Production_K7 %>%
  left_join(Delivery_K7,by="IDNummer")
head(Prod_Deli_join)


#calculate difference between delivery and produciton
#Q:You can assume that the produced goods are issued one day after production date.
difference <- as.numeric(difftime(Prod_Deli_join$Wareneingang, Prod_Deli_join$Produktionsdatum, units = "days"))

# add difference colomn to joined dataset 
Logistics_delay <- Prod_Deli_join %>%
  mutate(difference=difference)
head(Logistics_delay)

#plot emperical distrubtion
plotdist(difference, histo = TRUE, demp = F)

#gain overview with cullen and frey graph
descdist(difference, discrete = FALSE)

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
fahrzeug_komponente <- list.files("Data/Data/Fahrzeug", full.names = T, pattern = "Bestandteile")
komponente_teil <- list.files("Data/Data/Komponente", full.names = T, pattern = "Bestandteile")


task_3_2 <- function(x){
  df <- fread(file = x, header = T) #%>%
  if (any(str_detect(colnames(df),"T4$")==TRUE)){
    df %>%
      dplyr::select(ID_T4, contains("K")) # contains k 
  }
}

b <- bind_rows(lapply(komponente_teil,task_3_2))
  
OEM1_11 <- fread(file = "Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv",header= T) %>%
    as_tibble() %>%
    filter(str_detect(ID_Motor,"^K1BE1")) %>%
    dplyr::select(ID_Motor,ID_Fahrzeug)

OEM1_12 <- fread(file = "Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ12.csv",header= T) %>%
  as_tibble() %>%
  filter(str_detect(ID_Motor,"^K1BE1")) %>%
  dplyr::select(ID_Motor,ID_Fahrzeug)

vehicles <- bind_rows(OEM1_11,OEM1_12)  

registration <-fread(file="Data/Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv",header = T) %>%
    as_tibble() %>%
    mutate(Zulassung = as.Date(Zulassung))

head(registration)

vehicles_dortmund <- registration %>%
  filter(str_detect(Gemeinden,"DORTMUND")) %>%
  semi_join(vehicles, by=c("IDNummer"="ID_Fahrzeug"))
    
nrow(vehicles_dortmund)




#task 6

OEM2_21 <- fread(file = "Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ21.csv",header= T) %>%
  as_tibble()

OEM2_22 <- fread(file = "Data/Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM2_Typ22.csv",header= T) %>%
  as_tibble()

total_vehicles <- bind_rows(OEM2_21,OEM2_22)

driver <- total_vehicles%>%
  filter(str_detect(ID_Motor,"K1DI2-103-1031-21")) %>%
  right_join(registration, by= c("ID_Fahrzeug"="IDNummer"))
