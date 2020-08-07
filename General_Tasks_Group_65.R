if(!require("install.load")){
  install.packages("install.load")
}
library('install.load')

install_load("stringr", "data.table", "tidyverse","plotly", "fitdistrplus")

#fread erlaubt??
Production_K7 <- fread(file="Data/Data/Logistikverzug/Komponente_K7.csv")
head(Production_K7)

Delivery_K7 <- fread(file="Data/Data/Logistikverzug/Logistics_delay_K7.csv")
head(Delivery_K7)
#Q: herstellnummer 113 aber in ID 112 ??


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

#gain overview with cullen and frey graph
descdist(difference, discrete = FALSE)

#fit different distrubtions to the data 
fit_w  <- fitdist(difference, "weibull")
plot(fit_w)
fit_g  <- fitdist(difference, "gamma")
fit_ln <- fitdist(difference, "lnorm")
fit_n <- fitdist(difference, "norm")
gofstat(list(fit_w, fit_g, fit_ln, fit_n))

plotdist(difference, histo = TRUE, demp = TRUE, breaks=0:15)


#determine  mean, max and min of logistics delay
print(summary(Logistics_delay$difference))


#plot hist and density function with normalization??
fig <- plot_ly(x=difference, type = "histogram", histnorm = "probability")
fig


#open T4 
T04 <- fread(file="Data/Data/Einzelteil/Einzelteil_T04.csv")
head(T04)

