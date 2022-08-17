#Import necessary packages 
library(tidyverse)
library(readxl)

#Import lung df (windows script)
plasmadf <- read_excel("C:/Users/erhe/Desktop/cytokine_raj/data/plasmadf.xlsx")

#remove character '*' from all values within df 
plasmadf$`Mo IL-1a (53)`<-str_replace(plasmadf$`Mo IL-1a (53)`,
                                    "\\*","")
plasmadf$`Mo IL-1b (19)`<-str_replace(plasmadf$`Mo IL-1b (19)`,
                                      "\\*","")
plasmadf$`Mo IL-2 (36)`<-str_replace(plasmadf$`Mo IL-2 (36)`,
                                      "\\*","")
#str_replace(vector,pattern to replace, replacement)
plasmadf$`Mo IL-3 (18)`<-str_replace(plasmadf$`Mo IL-3 (18)`,
                                   "\\*","")
plasmadf$`Mo IL-4 (39)`<-str_replace(plasmadf$`Mo IL-4 (39)`,
                                   "\\*","")
plasmadf$`Mo IL-5 (52)`<-str_replace(plasmadf$`Mo IL-5 (52)`,
                                     "\\*","")
plasmadf$`Mo IL-6 (38)`<-str_replace(plasmadf$`Mo IL-6 (38)`,
                                     "\\*","")
plasmadf$`Mo IL-9 (33)`<-str_replace(plasmadf$`Mo IL-9 (33)`,
                                     "\\*","")
plasmadf$`Mo IL-10 (56)`<-str_replace(plasmadf$`Mo IL-10 (56)`,
                                    "\\*","")
plasmadf$`Mo IL-13 (37)`<-str_replace(plasmadf$`Mo IL-13 (37)`, 
                                    "\\*","")        
#mutate all columns except the first 2 columns and the last 3 columns to numeric 
plasmadf1 <- plasmadf %>% 
        mutate(across( "Mo IL-1a (53)":"Mo TNF-a (21)", as.numeric))
#subset df into df with only control values 
plasmacontrol<-plasmadf1 %>% 
        filter(Regimen == "Control") 
#Create vector for the control w respect to each cytokine, and then calculate the mean
#This value will be subtracted from each experimental value and then % chg will be calculated 
cp_1a <- plasmacontrol$`Mo IL-1a (53)` #extracts values for IL-1a for control
cp_1a_mean<-mean(cp_1a) #returns numeric value of 1a for control
cp_1b <- plasmacontrol$`Mo IL-1b (19)`
cp_1b_mean <- mean(cp_1b)
cp_2 <- plasmacontrol$`Mo IL-2 (36)`
cp_2_mean <- mean(cp_2)
cp_3 <- plasmacontrol$`Mo IL-3 (18)`
cp_3_mean <- mean(cp_3)
cp_4 <- plasmacontrol$`Mo IL-4 (39)`
cp_4_mean <- mean(cp_4)
cp_5 <- plasmacontrol$`Mo IL-5 (52)`
cp_5_mean <- mean(cp_5)
cp_6 <- plasmacontrol$`Mo IL-6 (38)`
cp_6_mean <- mean(cp_6)
cp_9 <- plasmacontrol$`Mo IL-9 (33)`
cp_9_mean <- mean(cp_9)
cp_10 <- plasmacontrol$`Mo IL-10 (56)`
cp_10_mean <- mean(cp_10)
cp_12_p40 <- plasmacontrol$`Mo IL-12(p40) (76)`
cp_12_p40_mean <- mean(cp_12_p40)
cp_12_p70 <- plasmacontrol$`Mo IL-12(p70) (78)`
cp_12_p70_mean <- mean(cp_12_p70)
cp_13 <- plasmacontrol$`Mo IL-13 (37)`
cp_13_mean <- mean(cp_13)
cp_17a <- plasmacontrol$`Mo IL-17A (72)`
cp_17a_mean <- mean(cp_17a)
cp_eotaxin <- plasmacontrol$`Mo Eotaxin (74)`
cp_eotaxin_mean <- mean(cp_eotaxin)
cp_GCSF <- plasmacontrol$`Mo G-CSF (54)`
cp_GCSF_mean <- mean(cp_GCSF)
cp_GMCSF <- plasmacontrol$`Mo GM-CSF (73)`
cp_GMCSF_mean <- mean(cp_GMCSF)
cp_IFNg <- plasmacontrol$`Mo IFN-g (34)`
cp_IFNg_mean <- mean(cp_IFNg)
cp_KC <- plasmacontrol$`Mo KC (57)`
cp_KC_mean <- mean(cp_KC)
cp_MCP1 <- plasmacontrol$`Mo MCP-1 (51)`
cp_MCP1_mean <- mean(cp_MCP1)
cp_MIP1a <- plasmacontrol$`Mo MIP-1a (77)`
cp_MIP1a_mean <- mean(cp_MIP1a)
cp_MIP1b <- plasmacontrol$`Mo MIP-1b (75)`
cp_MIP1b_mean <- mean(cp_MIP1b)
cp_RANTES <- plasmacontrol$`Mo RANTES (55)`
cp_RANTES_mean <- mean(cp_RANTES)
cp_TNFa <- plasmacontrol$`Mo TNF-a (21)`
cp_TNFa_mean <- mean(cp_TNFa)

#mutate each column and perform value - c_x_mean / value * 100 to obtain percent change for each column  
plasmadf2 <- plasmadf1 %>% 
        filter(Regimen != "Control") #removed control wells from this df bc this df will be used to generate heatmap 
plasmadf2 <- plasmadf2 %>% 
        mutate(`Mo IL-1a (53)` = ((plasmadf2$`Mo IL-1a (53)`- cp_1a_mean)/plasmadf2$`Mo IL-1a (53)`)*100,
               'Mo IL-1b (19)' = ((plasmadf2$`Mo IL-1b (19)`- cp_1b_mean)/plasmadf2$`Mo IL-1b (19)`)*100,
               'Mo IL-2 (36)' = ((plasmadf2$`Mo IL-2 (36)`- cp_2_mean)/plasmadf2$`Mo IL-2 (36)`)*100,
               'Mo IL-3 (18)' = ((plasmadf2$`Mo IL-3 (18)`- cp_3_mean)/plasmadf2$`Mo IL-3 (18)`)*100,
               'Mo IL-4 (39)' = ((plasmadf2$`Mo IL-4 (39)`- cp_4_mean)/plasmadf2$`Mo IL-4 (39)`)*100,
               'Mo IL-5 (52)' = ((plasmadf2$`Mo IL-5 (52)`- cp_5_mean)/plasmadf2$`Mo IL-5 (52)`)*100,
               'Mo IL-6 (38)' = ((plasmadf2$`Mo IL-6 (38)`- cp_6_mean)/plasmadf2$`Mo IL-6 (38)`)*100,
               'Mo IL-9 (33)' = ((plasmadf2$`Mo IL-9 (33)`- cp_9_mean)/plasmadf2$`Mo IL-9 (33)`)*100,
               'Mo IL-10 (56)' = ((plasmadf2$`Mo IL-10 (56)`- cp_10_mean)/plasmadf2$`Mo IL-10 (56)`)*100,
               'Mo IL-12(p40) (76)' = ((plasmadf2$`Mo IL-12(p40) (76)`- cp_12_p40_mean)/plasmadf2$`Mo IL-12(p40) (76)`)*100,
               'Mo IL-12(p70) (78)' = ((plasmadf2$`Mo IL-12(p70) (78)`- cp_12_p70_mean)/plasmadf2$`Mo IL-12(p70) (78)`)*100,
               'Mo IL-13 (37)' = ((plasmadf2$`Mo IL-13 (37)`- cp_13_mean)/plasmadf2$`Mo IL-13 (37)`)*100,
               'Mo IL-17A (72)' = ((plasmadf2$`Mo IL-17A (72)`- cp_17a_mean)/plasmadf2$`Mo IL-17A (72)`)*100,
               'Mo Eotaxin (74)' = ((plasmadf2$`Mo Eotaxin (74)`- cp_eotaxin_mean)/plasmadf2$`Mo Eotaxin (74)`)*100,
               'Mo G-CSF (54)' = ((plasmadf2$`Mo G-CSF (54)`- cp_GCSF_mean)/plasmadf2$`Mo G-CSF (54)`)*100,
               'Mo GM-CSF (73)' = ((plasmadf2$`Mo GM-CSF (73)`- cp_GMCSF_mean)/plasmadf2$`Mo GM-CSF (73)`)*100,
               'Mo IFN-g (34)' = ((plasmadf2$`Mo IFN-g (34)`- cp_IFNg_mean)/plasmadf2$`Mo IFN-g (34)`)*100,
               'Mo KC (57)' = ((plasmadf2$`Mo KC (57)`- cp_KC_mean)/plasmadf2$`Mo KC (57)`)*100,
               'Mo MCP-1 (51)' = ((plasmadf2$`Mo MCP-1 (51)`- cp_MCP1_mean)/plasmadf2$`Mo MCP-1 (51)`)*100,
               'Mo MIP-1a (77)' = ((plasmadf2$`Mo MIP-1a (77)`- cp_MIP1a_mean)/plasmadf2$`Mo MIP-1a (77)`)*100,
               'Mo MIP-1b (75)' = ((plasmadf2$`Mo MIP-1b (75)`- cp_MIP1b_mean)/plasmadf2$`Mo MIP-1b (75)`)*100,
               'Mo RANTES (55)' = ((plasmadf2$`Mo RANTES (55)`- cp_RANTES_mean)/plasmadf2$`Mo RANTES (55)`)*100,
               'Mo TNF-a (21)' = ((plasmadf2$`Mo TNF-a (21)`- cp_TNFa_mean)/plasmadf2$`Mo TNF-a (21)`)*100)
plasmadf3<- plasmadf2 %>%
        arrange(Regimen, Time_hrs) #sorts the df to display regimen and then time-hrs in ascending order 
#Transforming dataframe into a df identical to example shown here: https://jcoliver.github.io/learn-r/006-heatmaps.html
dfplasma <- pivot_longer(data = plasmadf3,
                       cols = -c("Type","Well","Site","Regimen","Time_hrs"),
                       names_to = "Cytokine_Type",
                       values_to = "Concentration") 
#heatmap of plasma data
ggplot(data=dfplasma, mapping = aes(
        x = Time_hrs, y = Cytokine_Type, fill = Concentration
)) + 
        geom_tile()+
        facet_grid(~Regimen)+
        theme_bw()+
        scale_x_continuous(breaks=c(0,10,24,36,48,192,360))

ggplot(data=dfplasma, mapping = aes(
        x = Regimen, y = Cytokine_Type, fill = Concentration
)) +
        geom_tile()+
        facet_grid(~Time_hrs)+
        theme_bw()
