if(!require("install.load")){
  install.packages("install.load")
}
library('install.load')

install_load("stringr", "data.table", "tidyverse","plotly")


#read first 50 lines
#CheckFile <- readLines("Data/Data/Komponente/Komponente_K7.txt",50) 

#print(CheckFile)

#DataImported <- read_file("Data/Data/Komponente/Komponente_K7.txt") %>%
#  str_replace_all(. , " \t\ ", ";") %>%
#  str_replace_all(. , " ", "\n") %>%
#  fread(text= .) %>%
#  as_tibble()
    
Production <- fread(file="Data/Data/Logistikverzug/Komponente_K7.csv")
print(Production)

Delivery <- fread(file="Data/Data/Logistikverzug/Logistics_delay_K7.csv")
print(Delivery)

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


