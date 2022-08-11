#Import necessary packages 
library(tidyverse)
library(readxl)

#Import lung df (windows script)
lungdf <- read_excel("C:/Users/erhe/Desktop/cytokine_raj/data/lungdf.xlsx")

#remove character '*' from all values within df 
#possibly perform this action manually, onto excel sheet itself 
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

lungdf1<- lungdf %>% 
        mutate_if(is.character(),as.numeric())
#Create vector for the control w respect to each cytokine, and then calculate the mean
#This value will be subtracted from each experimental value and then % chg will be calculated 
lungcontrol<-lungdf %>% 
        filter(Regimen == "Control") 



#This value calculated will be subtracted from all values listed that will be inputted into heatmap
#In attempts to debug the removal of "*" character in the columns, going to rename columns to remove spacings and parentheses 
namesofdf<- c("Type", "Well", "IL1a", "IL1b", "IL2","IL3",
              "IL4","IL5","IL6","IL9","IL10","IL12",
              "IL13","IL17A","Eotaxin","GCSF","GMCSF",
              "IFNg","KC","MCP1","MIP1a","MIP1b","RANTES",
              "TNFa","Site","Time_hrs","Regimen")
colnames(lungcontrol)<-namesofdf #replace existing df headers with new headers per namesofdf vector 

lungcontrol$IL1b<-gsub("*","",as.character(lungcontrol$IL1b))
lungcontrol$`Mo IL-1b (19)`<-gsub("*","",as.character(lungcontrol$`Mo IL-1b (19)`))

        

cIL1A<-as.numeric(c(lungcontrol$`Mo IL-1a (53)`))
cIL1Amean<-mean(cIL1A)

cIL1B<-as.numeric(c(lungcontrol$`Mo IL-1b (19)`))

#Subtract mean values from control from experimental values 

