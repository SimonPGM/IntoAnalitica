library(plotly)
library(boot)

#Simulando los datos
set.seed(03062000)
x1 <- rnorm(500)

set.seed(03062000)
x2 <- runif(500,2,4)

set.seed(03062000)
x3 <- rf(500,3,4)

set.seed(03062000)
dummy <- sample(factor(c(rep(c("A","B","C"),167))))[1:500]
x4 <- ifelse(dummy == "B",1,0)
x5 <- ifelse(dummy == "C",1,0)

y <- 1 + 0.3*x1 + 0.6*x2 - 1*x3 + 1.5*x4 - 2*x5 + rnorm(500,2)

bd <- data.frame(y,x1,x2,x3,x4,x5)

#Creando modelos
modfull <- glm(y~., data = bd)
mod1 <- glm(y~x1, data = bd)
mod2 <- glm(y~x2, data = bd)
mod3 <- glm(y~x3, data = bd)
mod4 <- glm(y~x4, data = bd)
mod5 <- glm(y~x5, data = bd)

#Cross Validation
set.seed(03062000)
index <- sample(1:500, 350)
trainCV <- bd[index,]
testCV <- bd[-index,]

modfullCV <- lm(y~., data = trainCV)
mod1CV <- lm(y~x1, data = trainCV)
mod2CV <- lm(y~x2, data = trainCV)
mod3CV <- lm(y~x3, data = trainCV)
mod4CV <- lm(y~x4, data = trainCV)
mod5CV <- lm(y~x5, data = trainCV)

mse <- function(mod, test){return(mean((test$y - predict(mod,test[,-1]))^2))}

resultadocv <- c(mse(modfullCV,testCV),
                 mse(mod1CV,testCV),
                 mse(mod2CV,testCV),
                 mse(mod3CV,testCV),
                 mse(mod4CV,testCV),
                 mse(mod5CV,testCV))


#K-Fold Cross Validation w k=10
resultadokfcv <- c(cv.glm(bd,modfull,K=10)$delta[2],
                   cv.glm(bd,mod1,K=10)$delta[2],
                   cv.glm(bd,mod2,K=10)$delta[2],
                   cv.glm(bd,mod3,K=10)$delta[2],
                   cv.glm(bd,mod4,K=10)$delta[2],
                   cv.glm(bd,mod5,K=10)$delta[2])

#LOOCV
resultadoloocv <- c(cv.glm(bd,modfull)$delta[2],
                   cv.glm(bd,mod1)$delta[2],
                   cv.glm(bd,mod2)$delta[2],
                   cv.glm(bd,mod3)$delta[2],
                   cv.glm(bd,mod4)$delta[2],
                   cv.glm(bd,mod5)$delta[2])

resultadofinal <- data.frame(Modelo = as.factor(rep(c("Fullmod", "Mod1", "Mod2", "Mod3", "Mod4", "Mod5"),3)),
                             Method = as.factor(rep(c("CV", "Kfold", "LOOCV"),each=6)),
                             Result = c(resultadocv, resultadokfcv, resultadoloocv))

#Graficando
CompCV <- ggplotly(ggplot(resultadofinal, aes(x = Method, y = Result, group = Modelo))+
  geom_line(aes(color = Modelo), size = 1.5) +
  geom_point(aes(color = Modelo),  size = 2) + 
  labs(title = "Comparación CV, KfCV, LOOCV") +
  xlab("Método") +
  ylab("MSE") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)))

saveRDS(CompCV, "CompCV.Rds")

# El mejor modelo es el modelo completo seguido por el modelo ajustado con la covariable x3
#Los resultados usando los métodos k-fold y loocv son muy similares

#Bootstrap
CalcIC <- function(boots){
  return(data.frame(Estimación = boots$t0, ErrorStd = sd(boots$t),
                    LI = boots$t0 - qnorm(0.975)*sd(boots$t),
                    LS = boots$t0 + qnorm(0.975)*sd(boots$t)))
  }

boot.sigma <- function(data, train){
  return(summary(lm(y~., data = data, subset = train))$sigma)
}
boot_sigma <- boot(bd,boot.sigma,10000)

CalcIC(boot_sigma)

#El IC para sigma del modelo completo es (0.9543126 1.082276) con una significancia de 0.05

boot.r <- function(data, train){
  return(summary(lm(y~., data = data, subset = train))$r.squared)
}
boot_r <- boot(bd,boot.r,10000)

CalcIC(boot_r)

#El IC para R^2 del modelo completo es (0.8907368 0.9722412) con una significancia de 0.05

boot.rsq <- function(data, train){
  return(summary(lm(y~., data = data, subset = train))$adj.r.squared)
}
boot_rsq <- boot(bd,boot.rsq,10000)

CalcIC(boot_rsq)

#El IC para R^2adj del modelo completo es (0.8902414 0.9713497) con una significancia de 0.05

#Simulando con t-student
set.seed(03062000)
y2 <- 1 + 0.3*x1 + 0.6*x2 - 1*x3 + 1.5*x4 - 2*x5 + rt(500, 9)

#Bootstrap
boot.sigmat <- function(data, train){
  return(summary(lm(y2~., data = data, subset = train))$sigma)
}
boot_sigmat <- boot(bd,boot.sigmat,10000)

CalcIC(boot_sigmat)

#El IC para sigma del modelo completo es (1.033389 1.194802) con una significancia de 0.05

boot.rt <- function(data, train){
  return(summary(lm(y2~., data = data, subset = train))$r.squared)
}
boot_rt <- boot(bd,boot.rt,10000)

CalcIC(boot_rt)

#El IC para R^2 del modelo completo es (0.8656343 0.9631755) con una significancia de 0.05

boot.rsqt <- function(data, train){
  return(summary(lm(y2~., data = data, subset = train))$adj.r.squared)
}
boot_rsqt <- boot(bd,boot.rsqt,10000)

CalcIC(boot_rsqt)

#El IC para R^2adj del modelo completo es (0.8641427 0.9625836) con una significancia de 0.05

#La varianza teórica de los errores es 9/(9-2) = 1.2857

#Nueva simulación
set.seed(03062000)
y3 <- 1 + 0.3*x1 + 2*I(x1^3) + 0.6*x2 - 1*x3 + 1.5*x4 - 2*x5 + rnorm(500,2)
bd3 <- data.frame(y3,x1,x2,x3,x4,x5)

#Cross Validation
set.seed(03062000)
index <- sample(1:500, 350)
train <- bd3[index,]
test <- bd3[-index,]

MSEcv <- c()
for (i in 1:10){
  mod <- lm(y3~poly(x1,i)+x2+x3+x4+x5, data = train)
  pred<-predict(mod,test)
  MSEcv[i] <- mean((pred-test$y3)^2)
}


#K-fold cross validation

MSEkfcv <- c()
for (i in 1:10){
  modkfcv <- glm(y3~poly(x1,i)+x2+x3+x4+x5, data = bd3)
  MSEkfcv[i] <- cv.glm(bd3, modkfcv,K=10)$delta[2]
}

#LOOCV cross validation

MSEloocv <- c()
for (i in 1:10){
  modloocv <- glm(y3~poly(x1,i)+x2+x3+x4+x5, data = bd3)
  MSEloocv[i] <- cv.glm(bd3, modloocv)$delta[2]
}

resultsim3 <- data.frame(Grado = as.factor(rep(1:10,3)),
           MSE = c(MSEcv,MSEkfcv,MSEloocv),
           Metodo = as.factor(rep(c("CV","KfCV","LOOCV"),each = 10)))

GradPol <- ggplotly(ggplot(resultsim3, aes(Grado, MSE, group = Metodo)) + 
           geom_point(aes(color = Metodo)) + 
           geom_path(aes(color = Metodo)) + 
           labs(title = "Grado del polinomio que minimiza el MSE") +
           xlab("Grado") +
           ylab("MSE") + 
           theme_minimal() + 
           theme(plot.title = element_text(hjust = 0.5)))

saveRDS(GradPol, "GradPol")
#Pfizer Inc. es una compañía farmacéutica con su sede principal ubicada en Nueva York, Estados Unidos. 
#Esta compañía dedica sus actividades al desarrollo y producción de distintos tipos de medicamentos 
#(especialmente vacunas) enfocadas al tratamiento de patologías propias de la medicina interna, oncología,
#inflamación e inmunología. 




