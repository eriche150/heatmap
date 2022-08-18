#Phage_PK | Plasma Heatmap 
dfplasma <- pivot_longer(data = lungdf3,
                       cols = -c("Type","Well","Site","Regimen","Time_hrs"),
                       names_to = "Cytokine_Type",
                       values_to = "Concentration") 
dfphageplasma <- dfplasma %>% 
        filter(Regimen == "Phage_PK")
ggplot(data=dfphageplasma, mapping = aes(
        x = Time_hrs, y = Cytokine_Type, fill = Concentration
)) + 
        geom_tile()+
        theme_bw()+
        ggtitle("Heatmap of [Cytokine] in Plasma for Phage PK")+
        scale_x_continuous(breaks=c(10,24,36,48))+
        scale_fill_gradient(name = "% Change from 
Control",
                            low = "blue",
                            high="lightpink1")

