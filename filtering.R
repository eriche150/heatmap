#Import necessary packages 
library(tidyverse)
library(readxl)

#Import lung df (windows script)
lungdf <- read_excel("C:/Users/erhe/Desktop/cytokine_raj/data/lungdf.xlsx")

#remove character '*' from all values within df 
lungdf$`Mo IL-1b (19)`<-str_replace(lungdf$`Mo IL-1b (19)`,
                                    "\\*","")
#str_replace(vector,pattern to replace, replacement)
lungdf$`Mo IL-3 (18)`<-str_replace(lungdf$`Mo IL-3 (18)`,
                                   "\\*","")
lungdf$`Mo IL-4 (39)`<-str_replace(lungdf$`Mo IL-4 (39)`,
                                   "\\*","")
lungdf$`Mo IL-10 (56)`<-str_replace(lungdf$`Mo IL-10 (56)`,
                                   "\\*","")
lungdf$`Mo IL-13 (37)`<-str_replace(lungdf$`Mo IL-13 (37)`, 
                                    "\\*","")        

#mutate all columns except the first 2 columns and the last 3 columns to numeric 
lungdf1 <- lungdf %>% 
        mutate(across( "Mo IL-1a (53)":"Mo TNF-a (21)", as.numeric))

#subset df into df with only control values 
lungcontrol<-lungdf1 %>% 
        filter(Regimen == "Control") 
#Create vector for the control w respect to each cytokine, and then calculate the mean
#This value will be subtracted from each experimental value and then % chg will be calculated 
c_1a <- lungcontrol$`Mo IL-1a (53)` #extracts values for IL-1a for control
c_1a_mean<-mean(c_1a) #returns numeric value of 1a for control
c_1b <- lungcontrol$`Mo IL-1b (19)`
c_1b_mean <- mean(c_1b)
c_2 <- lungcontrol$`Mo IL-2 (36)`
c_2_mean <- mean(c_2)
c_3 <- lungcontrol$`Mo IL-3 (18)`
c_3_mean <- mean(c_3)
c_4 <- lungcontrol$`Mo IL-4 (39)`
c_4_mean <- mean(c_4)
c_5 <- lungcontrol$`Mo IL-5 (52)`
c_5_mean <- mean(c_5)
c_6 <- lungcontrol$`Mo IL-6 (38)`
c_6_mean <- mean(c_6)
c_9 <- lungcontrol$`Mo IL-9 (33)`
c_9_mean <- mean(c_9)
c_10 <- lungcontrol$`Mo IL-10 (56)`
c_10_mean <- mean(c_10)
c_12_p40 <- lungcontrol$`Mo IL-12(p40) (76)`
c_12_p40_mean <- mean(c_12_p40)
c_12_p70 <- lungcontrol$`Mo IL-12(p70) (78)`
c_12_p70_mean <- mean(c_12_p70)
c_13 <- lungcontrol$`Mo IL-13 (37)`
c_13_mean <- mean(c_13)
c_17a <- lungcontrol$`Mo IL-17A (72)`
c_17a_mean <- mean(c_17a)
c_eotaxin <- lungcontrol$`Mo Eotaxin (74)`
c_eotaxin_mean <- mean(c_eotaxin)
c_GCSF <- lungcontrol$`Mo G-CSF (54)`
c_GCSF_mean <- mean(c_GCSF)
c_GMCSF <- lungcontrol$`Mo GM-CSF (73)`
c_GMCSF_mean <- mean(c_GMCSF)
c_IFNg <- lungcontrol$`Mo IFN-g (34)`
c_IFNg_mean <- mean(c_IFNg)
c_KC <- lungcontrol$`Mo KC (57)`
c_KC_mean <- mean(c_KC)
c_MCP1 <- lungcontrol$`Mo MCP-1 (51)`
c_MCP1_mean <- mean(c_MCP1)
c_MIP1a <- lungcontrol$`Mo MIP-1a (77)`
c_MIP1a_mean <- mean(c_MIP1a)
c_MIP1b <- lungcontrol$`Mo MIP-1b (75)`
c_MIP1b_mean <- mean(c_MIP1b)
c_RANTES <- lungcontrol$`Mo RANTES (55)`
c_RANTES_mean <- mean(c_RANTES)
c_TNFa <- lungcontrol$`Mo TNF-a (21)`
c_TNFa_mean <- mean(c_TNFa)


#mutate each column and perform value - c_x_mean / value * 100 to obtain percent change for each column  
lungdf2 <- lungdf1 %>% 
        filter(Regimen != "Control") #removed control wells from this df bc this df will be used to generate heatmap 
lungdf2 <- lungdf2 %>% 
        mutate(`Mo IL-1a (53)` = ((lungdf2$`Mo IL-1a (53)`- c_1a_mean)/lungdf2$`Mo IL-1a (53)`)*100,
               'Mo IL-1b (19)' = ((lungdf2$`Mo IL-1b (19)`- c_1b_mean)/lungdf2$`Mo IL-1b (19)`)*100,
               'Mo IL-2 (36)' = ((lungdf2$`Mo IL-2 (36)`- c_2_mean)/lungdf2$`Mo IL-2 (36)`)*100,
               'Mo IL-3 (18)' = ((lungdf2$`Mo IL-3 (18)`- c_3_mean)/lungdf2$`Mo IL-3 (18)`)*100,
               'Mo IL-4 (39)' = ((lungdf2$`Mo IL-4 (39)`- c_4_mean)/lungdf2$`Mo IL-4 (39)`)*100,
               'Mo IL-5 (52)' = ((lungdf2$`Mo IL-5 (52)`- c_5_mean)/lungdf2$`Mo IL-5 (52)`)*100,
               'Mo IL-6 (38)' = ((lungdf2$`Mo IL-6 (38)`- c_6_mean)/lungdf2$`Mo IL-6 (38)`)*100,
               'Mo IL-9 (33)' = ((lungdf2$`Mo IL-9 (33)`- c_9_mean)/lungdf2$`Mo IL-9 (33)`)*100,
               'Mo IL-10 (56)' = ((lungdf2$`Mo IL-10 (56)`- c_10_mean)/lungdf2$`Mo IL-10 (56)`)*100,
               'Mo IL-12(p40) (76)' = ((lungdf2$`Mo IL-12(p40) (76)`- c_12_p40_mean)/lungdf2$`Mo IL-12(p40) (76)`)*100,
               'Mo IL-12(p70) (78)' = ((lungdf2$`Mo IL-12(p70) (78)`- c_12_p70_mean)/lungdf2$`Mo IL-12(p70) (78)`)*100,
               'Mo IL-13 (37)' = ((lungdf2$`Mo IL-13 (37)`- c_13_mean)/lungdf2$`Mo IL-13 (37)`)*100,
               'Mo IL-17A (72)' = ((lungdf2$`Mo IL-17A (72)`- c_17a_mean)/lungdf2$`Mo IL-17A (72)`)*100,
               'Mo Eotaxin (74)' = ((lungdf2$`Mo Eotaxin (74)`- c_eotaxin_mean)/lungdf2$`Mo Eotaxin (74)`)*100,
               'Mo G-CSF (54)' = ((lungdf2$`Mo G-CSF (54)`- c_GCSF_mean)/lungdf2$`Mo G-CSF (54)`)*100,
               'Mo GM-CSF (73)' = ((lungdf2$`Mo GM-CSF (73)`- c_GMCSF_mean)/lungdf2$`Mo GM-CSF (73)`)*100,
               'Mo IFN-g (34)' = ((lungdf2$`Mo IFN-g (34)`- c_IFNg_mean)/lungdf2$`Mo IFN-g (34)`)*100,
               'Mo KC (57)' = ((lungdf2$`Mo KC (57)`- c_KC_mean)/lungdf2$`Mo KC (57)`)*100,
               'Mo MCP-1 (51)' = ((lungdf2$`Mo MCP-1 (51)`- c_MCP1_mean)/lungdf2$`Mo MCP-1 (51)`)*100,
               'Mo MIP-1a (77)' = ((lungdf2$`Mo MIP-1a (77)`- c_MIP1a_mean)/lungdf2$`Mo MIP-1a (77)`)*100,
               'Mo MIP-1b (75)' = ((lungdf2$`Mo MIP-1b (75)`- c_MIP1b_mean)/lungdf2$`Mo MIP-1b (75)`)*100,
               'Mo RANTES (55)' = ((lungdf2$`Mo RANTES (55)`- c_RANTES_mean)/lungdf2$`Mo RANTES (55)`)*100,
               'Mo TNF-a (21)' = ((lungdf2$`Mo TNF-a (21)`- c_TNFa_mean)/lungdf2$`Mo TNF-a (21)`)*100)

lungdf3<- lungdf2 %>%
        arrange(Regimen, Time_hrs) #sorts the df to display regimen and then time-hrs in ascending order 
        

#Transforming dataframe into a df identical to example shown here: https://jcoliver.github.io/learn-r/006-heatmaps.html
dflung <- pivot_longer(data = lungdf3,
                       cols = -c("Type","Well","Site","Regimen","Time_hrs"),
                       names_to = "Cytokine_Type",
                       values_to = "Concentration") 

