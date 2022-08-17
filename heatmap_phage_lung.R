#Phage PK | Lung Heatmap 
dflung <- pivot_longer(data = lungdf3,
                       cols = -c("Type","Well","Site","Regimen","Time_hrs"),
                       names_to = "Cytokine_Type",
                       values_to = "Concentration") 
dfphagelung <- dflung %>% 
        filter(Regimen == "Phage_PK")
ggplot(data=dfphagelung, mapping = aes(
        x = Time_hrs, y = Cytokine_Type, fill = Concentration
)) + 
        geom_tile()+
        theme_bw()+
        ggtitle("Heatmap of [Cytokine] in Lung for Phage PK")+
        scale_x_continuous(breaks=c(0,10,24,36,48,192,360))
