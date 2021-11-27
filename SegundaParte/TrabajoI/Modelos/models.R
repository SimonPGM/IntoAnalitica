library(caret)
library(tidyverse)
library(plotly)
library(reshape2)
datos <- readRDS("PFE.Rds") %>%
  mutate(Direction = factor(Direction, ordered = F))
datos.model <- datos[, -c(1, 8)] #excluyendo variables innecesarias
n <- nrow(datos)

# KNN

#Usando train-test split
set.seed(7)
idx <- sample(1:n, floor(n*0.7))
train.set <- datos.model[idx, ]
test.set <- datos.model[-idx, ]
TER.KNN.1 <- c() #Training error rate for this method

for (k in 1:20) {
  model.temp <- knn3Train(train.set[,-ncol(train.set)],
                          test.set[,-ncol(test.set)], cl = train.set$Direction, k = k)
  TER.KNN.1[k] <- mean(model.temp != as.character(test.set$Direction)) 
}

#Usando LOOCV
train.control.1 <- trainControl(method = "LOOCV") #control de entrenamiento
grid.1 <- expand.grid(k = 1:20) #generando maya de hiperparametros
mod2 <- train(Direction ~ ., data = datos.model, method = "knn",
              trControl = train.control.1, tuneGrid = grid.1) #haciendo el entrenamiento
TER.KNN.2 <-1 - mod2$results$Accuracy #getting test error rate

#Usando k-fold cross validation
train.control.2 <- trainControl(method = "cv", number = 10)
mod3 <- train(Direction ~ ., data = datos.model, method = "knn",
              trControl = train.control.2, tuneGrid = grid.1)
TER.KNN.3 <- 1 - mod3$results$Accuracy

resumen.knn <- data.frame(Error = c(TER.KNN.1, TER.KNN.2, TER.KNN.3),
                          k = rep(1:20, 3), 
                          "Método" = factor(rep(c("Validation Set", 
                                                "LOOCV", "K-fold cross validation"),
                                                each = 20)))
knn1 <- ggplotly(ggplot(resumen.knn, aes(k, Error, color = Método)) +
  geom_point() +
  geom_line() +
  labs(x = "K", y = "Tasa de error", title = "Comparación de validación cruzada",
       color = "Método") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5)))

#saveRDS(knn1, "knncv.Rds")


#Ajuste KNN, LDA Y QDA
mod.knn <- knn3(Direction ~ ., data = train.set, k = 3)
#Down = 0, Up = 1
mod.logistic <- glm(Direction ~ ., data = train.set, family = binomial(link = "logit")) #logistico
mod.LDA <- train(Direction ~ ., data = train.set,
                 method = "lda", trControl = trainControl(method = "none")) #LDA
mod.QDA <- train(Direction ~ ., data = train.set,
                 method = "qda", trControl = trainControl(method = "none")) #QDA

#Creando predicciones sobre el conjunto de prueba

pred.mod.knn <- predict(mod.knn, test.set) %>%
  apply(1, function(x) which.max(x)-1) %>%
  as.logical %>%
  if_else("Up", "Down") %>%
  factor

pred.mod.logistic <- (predict(mod.logistic, test.set, type = "response") > 0.5) %>%
  if_else("Up", "Down") %>%
  factor

pred.mod.LDA <- predict(mod.LDA, test.set)

pred.mod.QDA <- predict(mod.QDA, test.set)

#Generando las matrices de confusión
noms <- c("Reales", "Predichos")

#para KNN
conf1 <- table(test.set$Direction, pred.mod.knn) %>%
  melt(value.name = "Conteo")
names(conf1)[1:2] <- noms
TER.1 <- paste(100*round(mean(test.set$Direction != pred.mod.knn),4),
               "%", sep = "") #test error rate
conf1 <- conf1 %>%
  arrange(desc(Reales), desc(Predichos)) %>%
  mutate(Reales = factor(Reales, levels = c("Up", "Down")))

cm1 <- ggplotly(ggplot(conf1, aes(Reales,Predichos, fill = Conteo)) +
  geom_tile(show.legend = F) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5))+
    annotate("text", x = 1.5, y = 2.55,
             label = paste("Modelo KNN con tasa de error de prueba = ", TER.1, sep = "")))

#para el modelo logistico

conf2 <- table(test.set$Direction, pred.mod.logistic) %>%
  melt(value.name = "Conteo")
names(conf2)[1:2] <- noms
TER.2 <- paste(100*round(mean(test.set$Direction != pred.mod.logistic),4),
               "%", sep = "") #test error rate

conf2 <- conf2 %>%
  arrange(desc(Reales), desc(Predichos)) %>%
  mutate(Reales = factor(Reales, levels = c("Up", "Down")))

cm2 <- ggplotly(ggplot(conf2, aes(Reales,Predichos, fill = Conteo)) +
           geom_tile(show.legend = F) +
           theme_minimal() +
           theme(plot.title = element_text(hjust = .5)) +
             annotate("text", x = 1.5, y = 2.55,
                      label = paste("Modelo logístico con tasa de error de prueba = ", 
                                    TER.2, sep = "")))

#Para LDA
conf3 <- table(test.set$Direction, pred.mod.LDA) %>%
  melt(value.name = "Conteo")
names(conf3)[1:2] <- noms
TER.3 <- paste(100*round(mean(test.set$Direction != pred.mod.LDA),4),
               "%", sep = "") #test error rate

conf3 <- conf3 %>%
  arrange(desc(Reales), desc(Predichos)) %>%
  mutate(Reales = factor(Reales, levels = c("Up", "Down")))

cm3 <- ggplotly(ggplot(conf3, aes(Reales,Predichos, fill = Conteo)) +
           geom_tile(show.legend = F) +
           theme_minimal() +
           theme(plot.title = element_text(hjust = .5)) +
             annotate("text", x = 1.5, y = 2.55,
                      label = paste("Modelo LDA con tasa de error de prueba = ", 
                                    TER.3, sep = "")))

#Para QDA
conf4 <- table(test.set$Direction, pred.mod.QDA) %>%
  melt(value.name = "Conteo")
names(conf4)[1:2] <- noms
TER.4 <- paste(100*round(mean(test.set$Direction != pred.mod.QDA),4),
               "%", sep = "") #test error rate

conf4 <- conf4 %>%
  arrange(desc(Reales), desc(Predichos)) %>%
  mutate(Reales = factor(Reales, levels = c("Up", "Down")))

tit4 <- "Desempeño de los modelos KNN, LOGÍSTICO"
cm4 <- ggplotly(ggplot(conf4, aes(Reales,Predichos, fill = Conteo)) +
           geom_tile(show.legend = F) +
           labs(title = "Desempeño de los modelos") +
           theme_minimal() +
           theme(plot.title = element_text(hjust = .5)) +
             annotate("text", x = 1.5, y = 2.55,
                      label = paste("Modelo QDA con tasa de error de prueba = ", 
                                    TER.4, sep = "")))
#saveRDS(subplot(cm1, cm2, cm3, cm4, nrows = 2), "Confmat.Rds")
