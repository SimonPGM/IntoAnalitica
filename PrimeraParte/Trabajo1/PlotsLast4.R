sturges <- function(x) {
  n <- length(x)
  k <- round(1 + 3.322*log10(n))
  m <- min(x)
  M <- max(x)
  (M - m)/k
}

p41 <- ggplot(college, aes(Accept, Enroll)) +
  geom_point() +
  labs(x = "Cantidad de estudiantes aceptados",
       y = "Cantidad de estudiantes matriculados",
       title = "Gráfico de y vs x")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))
#a medida que aumenta la cantidad de estuidantes que aceptan el cupo
#aumenta la incertidumbre en la cantidad de estudiantes que se matriculan

p42 <- ggplot(college, aes(Top10perc)) +
  geom_histogram(color = "black", fill = "orange",
                 binwidth = sturges(college$Top10perc)) +
  labs(x = "Cantidad de estudiantes que estuvieron en el top 10% de H.S.",
       y = "Frecuencia",
       title = "Histograma para la cantidad de estudiantes\nen el top 10% de H.S.")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

p43 <- ggplot(college, aes(Top10perc)) +
  geom_histogram(color = "black", fill = "orange",
                 binwidth = sturges(college$Top25perc)) +
  labs(x = "Cantidad de estudiantes que estuvieron en el top 25% de H.S.",
       y = "Frecuencia",
       title = "Histograma para la cantidad de estudiantes\nen el top 25% de H.S.")+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


p47 <- college %>%
  mutate(Private = factor(if_else(Private == "Yes", "Universidad privada",
                                  "Universidad pública"))) %>%
ggplot(aes(Apps, fill = Private)) +
  geom_histogram(show.legend = F, color = "black",
                 binwidth = sturges(college$Apps)) +
  facet_wrap(".~Private") +
  labs(x = "Número de aplicaciones", y = "Frecuencia",
       title = "Distribución para el número de aplicaciones\ndiscriminando por tipo de universidad") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90)) +
  scale_color_manual(values = c("green", "orange"),
                     aesthetics = "fill")
