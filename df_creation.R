#Import necessary packages 
library(tidyverse)
library(readxl)

#Import dataset; iOS script below 
micecytodatav1 <- read_excel("data/micecytodatav.xls",
                             sheet = "manip", skip = 8)

#Create two different site-specific datasets
lungdf<-micecytodatav1 %>% 
        filter(Site == "Lung")
plasmadf<-micecytodatav1 %>% 
        filter(Site == "Plasma")


#Rename _df$Regimen from numeric values to characters 
lungdf<- lungdf %>% 
        mutate(Regimen=replace(Regimen, Regimen == 0, "Control"),
               Regimen=replace(Regimen, Regimen == 1, "SIGMA_DEX"),
               Regimen=replace(Regimen, Regimen == 2, "VET_DEX"),
               Regimen=replace(Regimen, Regimen == 3, "Phage_PK"))
plasmadf<-plasmadf %>% 
        mutate(Regimen=replace(Regimen, Regimen == 0, "Control"),
               Regimen=replace(Regimen, Regimen == 1, "SIGMA_DEX"),
               Regimen=replace(Regimen, Regimen == 2, "VET_DEX"),
               Regimen=replace(Regimen, Regimen == 3, "Phage_PK"))

#export dataframes from R into Excel files 
install.packages("writexl")               
write_xlsx(lungdf,"/Users/eriche/Library/Mobile Documents/com~apple~CloudDocs/UNC PKPD/cytokine_raj/heatmap/heatmap//lungdf.xlsx",
           col_names = TRUE)
write_xlsx(plasmadf,
           "/Users/eriche/Library/Mobile Documents/com~apple~CloudDocs/UNC PKPD/cytokine_raj/heatmap/heatmap//plasmadf.xlsx",
           col_names = TRUE) 
