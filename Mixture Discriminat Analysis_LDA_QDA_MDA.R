


#### -----------Mixture Discriminant Analysis------####

#Linear discriminant analysis (LDA)
#Quadratic discriminant analysis (QDA)
#Mixture discriminant analysis (MDA)


library(dbplyr)
library(MASS)
library(mvtnorm)
library(mda)
library(ggplot2)

Base <- read.table("Base_AbunNat.txt", header=TRUE, dec=",")

Base_Foliar<-subset(Base, Muestra=="Foliar")
Base_Foliar1<-Base_Foliar[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]
Base_Foliar1  # esta contiene solo las 6 variables descritas arriba.


#1. Split the data into training and test set
require (dbplyr)
set.seed(123)

training.samples=Base_Foliar1$Tratamiento %>%
  createDataPartition(p = 0.9, list = FALSE)#p se puede modificar de acuerdo a la precision que se desee en los modelos discriminantes

train.data=Base_Foliar1[training.samples, ]#contiene el 90% de los datos
test.data=Base_Foliar1[-training.samples, ]#contiene el 10% de los datos

#2. Normalize the data. Categorical variables are automatically ignored.
#esto sirve para normalIzar los datos? 
#o una estandarizacion de datos a nivel de escala?

# Estimate preprocessing parameters
preproc.param <- train.data %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transformed <- preproc.param %>% predict(train.data)
test.transformed <- preproc.param %>% predict(test.data)
#train.transformed se usara para los modelos
#test.transformed se usara para hacer las predicciones y ajustar el modelo

#### -----------Mixture Discriminant Analysis------####

#---1.Trains classifiers--

lda_out <- lda(Tratamiento ~ ., data = train.transformed) 
qda_out <- qda(Tratamiento ~ ., data = train.transformed) 
mda_out <- mda(Tratamiento ~ ., data = train.transformed)

#Generates test data that will be used to generate the decision boundaries via
contour_data <- expand.grid(X1 = seq(-8, 8, length = 300), 
                            X2 = seq(-8, 8, length = 300))

#Classifies the test data
lda_predict <- data.frame(contour_data, y = as.numeric(predict(lda_out, contour_data)$class)) 
qda_predict <- data.frame(contour_data, y = as.numeric(predict(qda_out, contour_data)$class)) 
mda_predict <- data.frame(contour_data, y = as.numeric(predict(mda_out, contour_data)))

#hasta aqui llegue, me dice que no encuentra N_por y no se porque? 

#Generates plots

p <- ggplot(train_data, aes(x = X1, y = X2, color = y)) + geom_point() p + stat_contour(aes(x = X1, y = X2, z = y), data = lda_predict) + ggtitle("LDA Decision Boundaries") 
p + stat_contour(aes(x = X1, y = X2, z = y), data = qda_predict) + ggtitle("QDA Decision Boundaries") 
p + stat_contour(aes(x = X1, y = X2, z = y), data = mda_predict) + ggtitle("MDA Decision Boundaries")

#Fuente: https://www.r-bloggers.com/2013/07/a-brief-look-at-mixture-discriminant-analysis/