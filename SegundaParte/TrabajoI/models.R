library(caret)
library(tidyverse)
library(plotly)
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

#Creando dfs para visualizar performance sobre el conjunto de entrenamiento

logistic.pred.train <- factor(if_else(mod.logistic$fitted.values > 0.5, "Up", "Down"))
knn.pred.train <- factor(ifelse(apply(predict(mod.knn, train.set), 1, which.max) == 1, "Down",
                          "Up"))
df.train.performance <- data.frame(Value = c(train.set$Direction, knn.pred.train,
                                             logistic.pred, predict(mod.LDA), predict(mod.QDA)),
                                   Modelo = rep(c("Original","KNN", "Logístico", "LDA", "QDA"),
                                                each = nrow(train.set)),
                                   Tiempo = rep(1:nrow(train.set), 5))

#Plot original vs knn
ggplotly(df.train.performance %>%
  filter(Modelo %in% c("Original", "KNN")) %>%
  mutate(Valor = if_else(Value == "Up", 1, 0)) %>%
  ggplot(aes(Tiempo, Valor, color = Modelo)) +
  geom_point())

aux <- as.matrix(table(train.set$Direction, knn.pred.train))
aux <- apply(as.matrix.noquote(aux),2,as.numeric)
rownames(aux) <- c("Down", "Up")
aux <- t(aux)
#Original
plot_ly(x=colnames(aux), y=rownames(aux), z = aux, type = "heatmap", annotation_text = matrix(1:4, ncol = 2, nrow = 2) ) %>%
  layout(margin = list(l=120))
