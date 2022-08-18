#Sigma_Dex | Plasma Heatmap 
dfplasma <- pivot_longer(data = lungdf3,
                         cols = -c("Type","Well","Site","Regimen","Time_hrs"),
                         names_to = "Cytokine_Type",
                         values_to = "Concentration") 
dfSIGMADEXplasma <- dfplasma %>% 
        filter(Regimen == "SIGMA_DEX")
ggplot(data=dfSIGMADEXplasma, mapping = aes(
        x = Time_hrs, y = Cytokine_Type, fill = Concentration
)) + 
        geom_tile()+
        theme_bw()+
        ggtitle("Heatmap of [Cytokine] in Plasma for SIGMA+DEX")+
        scale_x_continuous(breaks=c(24,192,360))
