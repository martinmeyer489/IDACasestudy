if(!require("install.load")){
  install.packages("install.load")
}
library('install.load')

install_load("stringr", "data.table", "tidyverse","plotly")

Production <- fread(file="Data/Data/Logistikverzug/Komponente_K7.csv")
head(Production)

Delivery <- fread(file="Data/Data/Logistikverzug/Logistics_delay_K7.csv")
head(Delivery)

#Q: herstellnummer 113 aber in ID 112 ??
# check for duplicates




#join both datasets
Prod_Deli_join <- Production %>%
  left_join(Delivery,by="IDNummer")

# add another colomn with difference between delivery and produciton
Logistics_delay <- Prod_Deli_join %>%
  mutate(diff=difftime(Wareneingang, Produktionsdatum, units = "days"))
  




#determine  mean, max and min of logistics delay
abcd <- Logistics_delay %>%
  summarise(mean=mean(diff),max=max(diff),min=min(diff))

#plot hist and density function with normalization??
fig <- plot_ly(x=Logistics_delay$diff, type = "histogram", histnorm = "probability")
fig


