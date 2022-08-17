#Sigma DEX | Lung Heatmap 
dflung <- pivot_longer(data = lungdf3,
                       cols = -c("Type","Well","Site","Regimen","Time_hrs"),
                       names_to = "Cytokine_Type",
                       values_to = "Concentration") 
dfSIGMADEXlung <- dflung %>% 
        filter(Regimen == "SIGMA_DEX")
ggplot(data=dfSIGMADEXlung, mapping = aes(
        x = Time_hrs, y = Cytokine_Type, fill = Concentration
)) + 
        geom_tile()+
        theme_bw()+
        ggtitle("Heatmap of [Cytokine] in Lung for SIGMA+DEX")+
        scale_x_continuous(breaks=c(24,192,360))
