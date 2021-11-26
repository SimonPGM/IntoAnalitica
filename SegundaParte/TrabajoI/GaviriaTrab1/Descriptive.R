#Plots

p1 <- ggplot(PFE, aes(fill = Date, y = Today)) +
  geom_boxplot() + 
  labs(title = "Date Vs Today") +
  ylab("Today") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_blank())

p2 <- ggplot(PFE, aes(fill = Direction, y = lag2)) +
  geom_boxplot() + 
  labs(title = "Direction Vs Lag2") +
  ylab("Lag2") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_blank())

p3 <- ggplot(PFE, aes(fill = Direction, y = lag5)) +
  geom_boxplot() + 
  labs(title = "Direction Vs Lag5") +
  ylab("Lag5") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_blank())

p4 <- ggplot(PFE, aes(fill = Direction, y = Volume)) +
  geom_boxplot() + 
  labs(title = "Direction Vs Volume") +
  ylab("Volume") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_blank())

Descriptive1 <- ggpubr::ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2)

saveRDS(Descriptive1, "Descriptive1.Rds")

#Tabla de contingencia y prueba chi cuadrado

kable(table(PFE$Date, PFE$Direction), 
      caption = "Número de Subidas y Bajadas por año", align = 'c', 
      longtable = T, booktab = T)

chitable <- chisq.test(table(PFE$Date, PFE$Direction))
testchi <- data.frame(stat = chitable$statistic, df = chitable$parameter, 
                      pvalue = chitable$p.value)
kable(testchi, caption = "Prueba de independencia", align = 'c',
      colnames = c("$\chi^2$", "Df", "Pvalue"), longtable = T, booktab= T,
      escape = F)

# Reafirmando uniformidad en los años e independencia entre date y direction
Descriptive2 <- ggplotly(ggplot(PFE, aes(Date, fill = Direction, group = Direction)) + 
  geom_bar() + 
  labs(title = "Direction Vs Date") +
  ylab("Frequency") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)))
  
saveRDS(Descriptive2, "Descriptive2.Rds")

