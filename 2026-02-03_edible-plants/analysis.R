dfC <- select(df1, Name, Cultivation, Hardiness, minph = preferred_ph_lower, maxph = preferred_ph_upper)
dfC$hardinessN <- as.numeric(dfC$Hardiness)
dfC_summary <- dfC %>%
  group_by(Cultivation) %>%
  summarise(
    minph = min(minph, na.rm = TRUE),
    maxph = max(maxph, na.rm = TRUE),
    medianH = round(median(hardinessN, na.rm = TRUE)),
    .groups = "drop"
  )
dfC_summary$medianH <- factor(dfC_summary$medianH,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("Very Tender", "Tender", "Medium", "Hardy", "Very Hardy")
                              )
ggplot(dfC_summary,
       aes(y = Cultivation,
           xmin = minph,
           xmax = maxph,
           colour = medianH)) +
  geom_linerange(size = 4, lineend = "round") +
  labs(colour = "Median Hardiness", y = "Cultivation Type", x = "pH Range")+
  scale_color_manual(values = c("Very Tender" = "#f2f0f7",
                                "Tender" = "#dadaeb",
                                "Medium" = "#9e9ac8",
                                "Hardy" = "#6a51a3",
                                "Very Hardy" = "#2d0a5e"),
                     drop = F)+ #colours from colorbrewer
  scale_x_continuous(limits = c(4, 9), breaks = seq(4, 9, by = 1))+
theme_minimal()+
  theme(
    panel.grid = element_blank(),      
    axis.line = element_line(colour = "black") 
  )
