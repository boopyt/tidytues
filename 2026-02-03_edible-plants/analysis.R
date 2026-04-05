dfC <- select(df1, Name, Cultivation, Hardiness, minph = preferred_ph_lower, maxph = preferred_ph_upper)
dfC$hardinessN <- as.numeric(dfC$Hardiness)
table(dfC$hardinessN)
table(dfC$Hardiness)
dfC_summary <- dfC %>%
  group_by(Cultivation) %>%
  summarise(
    minph = minph, 
    maxph = maxph,
    medianH = median(hardinessN, na.rm = TRUE),
    .groups = "drop"
  )
unique(dfC$Hardiness)
ggplot(dfC_summary,
       aes(y = Cultivation,
           xmin = minph,
           xmax = maxph,
           colour = factor(medianH))) +
  geom_linerange(size = 4, lineend = "round") +
  labs(colour = "Median Hardiness", y = "Cultivation Type", x = "pH Range")+
  scale_colour_discrete(drop = FALSE) + #check this
  scale_color_manual(labels = c("Tender", "Medium", "Hardy", "Very Hardy"), values = c("#dadaeb", "#9e9ac8", "#6a51a3", "#2d0a5e")) + #colours from colorbrewer
theme_minimal()+
  theme(
    panel.grid = element_blank(),      
    axis.line = element_line(colour = "grey80") 
  )
