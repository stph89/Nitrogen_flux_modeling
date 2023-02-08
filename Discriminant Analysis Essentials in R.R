

#####-------Discriminant Analysis Essentials in R-----

#http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/

install.packages("caret")
require (rmarkdown) 
require (dbplyr)
require(MASS)
require(caret)

Base <- read.table("Base_AbunNat.txt", header=TRUE, dec=",")

Base_Foliar<-subset(Base, Muestra=="Foliar")
Base_Foliar1<-Base_Foliar[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]
Base_Foliar1  # esta contiene solo las 6 variables descritas arriba.


#1. Split the data into training and test set

require (dbplyr)
set.seed(123)

training.samples=Base_Foliar1$Tratamiento %>%
  createDataPartition(p = 0.9, list = FALSE)

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

#----1. Linear discriminant analysis (LDA)-----

# LDA assumes that predictors are normally distributed (Gaussian distribution) and that the different classes have class-specific means and equal variance/covariance.
#in LDA we assume equality of covariance matrix for all of the classes.
#Before performing LDA, consider:
#Inspecting the univariate distributions of each variable and make sure that they are normally distribute. 
#If not, you can transform them using log and root for exponential distributions and Box-Cox for skewed distributions.
#removing outliers from your data and standardize the variables to make their scale comparable.


#Fit the model LDA
modelo=lda(Tratamiento ~ N_por+N15_por+C_N, data = train.transformed)
modelo

# Make predictions
contri_var_lda=modelo$scaling
contri_var_lda

predicciones=modelo %>% predict(test.transformed)
predicciones

#predict() nos da para LDA:
#class: predicted classes of observations.
#posterior: is a matrix whose columns are the groups, rows are the individuals and values are the posterior probability that the corresponding observation belongs to the groups.
#x: contains the linear discriminants <- este objeto no se da para los otros tipos de discriminantes?

head(predicciones$class)
head(predicciones$posterior)
head(predicciones$x)

# Model accuracy: es el % de precisión del modelo respecto a los datos evaluados
mean(predicciones$class==test.transformed$Tratamiento)

lda.data=cbind(train.transformed, predict(modelo)$x)
lda.data

require(ggplot2)
require(ggalt)

grafica_discriminante=ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(shape=Tratamiento,color = Tratamiento),size=2)+
  scale_shape_manual(values = c(0,1,2,3))+
  scale_color_manual(values=c("chartreuse4","cyan3",
                              "darkgoldenrod1", "orangered"))+
  geom_encircle(aes(fill = Tratamiento), s_shape = 1, expand = 0,
                alpha = 0.2, color = "black", show.legend = FALSE)+
  scale_fill_manual(values=c("chartreuse4","cyan3",
                             "darkgoldenrod1", "orangered"))+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman",face = "bold",size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = "black",size = 20),
        axis.text.y = element_text(colour = "black",size = 20),
        panel.background = element_blank())

grafica_discriminante

#Grafica de LDA con ggplot2 con puntos
lda.data.plot <- cbind(train.transformed, predict(modelo)$x)
ggplot(lda.data.plot, aes(LD1, LD2)) +
  geom_point(aes(color = Tratamiento))

#ESTE ES EL UNICO QUE GRAFICA BIEN
#COMO SE PUEDE GRAFICAR EL CUADRATICO, MIXTO, FLEXIBLE Y REGULADO. 
#SE PUEDEN SACAR GRAFICAS SIMILARES A ESTA EN DOS EJES PARA LOS OTROS TIPOS DE ANALISIS DISCRIMINANTES?

#-----2.Quadratic discriminant analysis QDA-----

#QDA assumes different covariance matrices for all the classes

library(MASS)
# Fit the model
modelQDA=qda(Tratamiento ~ N_por+N15_por+C_N, data = train.transformed)
modelQDA
# Make predictions
predictionsQDA <- modelQDA %>% predict(test.transformed)

QDA<-as.data.frame(predictionsQDA)
# Model accuracy
mean(predictionsQDA$class == test.transformed$Tratamiento)

qda.data=cbind(train.transformed, predict(modelQDA))
qda.data

#no funciono da un error del numero de filas


#Grafica de QDA con ggplot2 con puntos
qda.data.plot <- cbind(train.transformed, predict(modelQDA))
ggplot(qda.data.plot) +
  geom_point(aes(color = Tratamiento))

# Fit the model
modelQDA=qda(Tratamiento ~ N_por+N15_por+C_N, data = train.transformed)
modelQDA
# Make predictions
predictionsQDA <- modelQDA %>% predict(test.transformed)

QDA_PLOT=ggplot(qda.data.plot, aes(LD1, LD2)) +
  geom_point(aes(shape=Tratamiento,color = Tratamiento),size=2)+
  scale_shape_manual(values = c(0,1,2,3))+
  scale_color_manual(values=c("chartreuse4","cyan3",
                              "darkgoldenrod1", "orangered"))+
  geom_encircle(aes(fill = Tratamiento), s_shape = 1, expand = 0,
                alpha = 0.2, color = "black", show.legend = FALSE)+
  scale_fill_manual(values=c("chartreuse4","cyan3",
                             "darkgoldenrod1", "orangered"))+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman",face = "bold",size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = "black",size = 20),
        axis.text.y = element_text(colour = "black",size = 20),
        panel.background = element_blank())

QDA_PLOT


#   ----3. Mixture discriminant analysis - MDA--------
#Each class is assumed to be a Gaussian mixture of subclasses.

library(mda)
require(mda)

# Fit the model
modelMDA <- mda(Tratamiento ~ N_por+N15_por+C_N, data = train.transformed)
modelMDA

# Make predictions
predicted.classes1 <- modelMDA %>% predict(test.transformed)

# Model accuracy
mean(predicted.classes1 == test.transformed$Tratamiento)

mda.data=cbind(train.transformed, predict(modelMDA)$x)

#errorrrrr
#dice que $ es invalido para vectores atomicos
#no encuentra en objeto x<- creo q es porque solo el LDA que el linear tiene este objeto que guarda las relaciones lineales entre los discriminantes
#no se puede generar el objeto mda.data y no se peude graficar por eso

predicciones=modelo %>% predict(test.transformed)
predicciones

#predict() nos da:
#class: predicted classes of observations.
#posterior: is a matrix whose columns are the groups, rows are the individuals and values are the posterior probability that the corresponding observation belongs to the groups.
#x: contains the linear discriminants

head(predicted.classes1$class)
head(predicted.classes1$posterior)
head(predicted.classes1$x)

str(predicted.classes1)


#Grafica de QDA con ggplot2 con puntos
qda.data.plot <- cbind(train.transformed, predict(modelQDA)$x)
ggplot(qda.data.plot, aes(LD1, LD2)) +
  geom_point(aes(color = Tratamiento))


QDA_PLOT=ggplot(qda.data.plot, aes(LD1, LD2)) +
  geom_point(aes(shape=Tratamiento,color = Tratamiento),size=2)+
  scale_shape_manual(values = c(0,1,2,3))+
  scale_color_manual(values=c("chartreuse4","cyan3",
                              "darkgoldenrod1", "orangered"))+
  geom_encircle(aes(fill = Tratamiento), s_shape = 1, expand = 0,
                alpha = 0.2, color = "black", show.legend = FALSE)+
  scale_fill_manual(values=c("chartreuse4","cyan3",
                             "darkgoldenrod1", "orangered"))+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman",face = "bold",size=20),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(colour = "black",size = 20),
        axis.text.y = element_text(colour = "black",size = 20),
        panel.background = element_blank())

QDA_PLOT

#     -4. Flexible discriminant analysis - FDA----
#FDA is a flexible extension of LDA that uses non-linear combinations of predictors such as splines
#FDA is useful to model multivariate non-normality or non-linear relationships among variables within 
#each group, allowing for a more accurate classification.

library(mda)
# Fit the model
modelFDA <- fda(Tratamiento~., data = train.transformed)
# Make predictions
predicted.classes2 <- modelFDA %>% predict(test.transformed)
# Model accuracy
mean(predicted.classes2 == test.transformed$Tratamiento)

fda.data=cbind(train.transformed, predict(modelFDA)$x)

# $ es invalido para vectores atomicos

#     -5. Regularized discriminant analysis - RDA------
#Regularized discriminant analysis is an intermediate between LDA and QDA.
library(klaR)
# Fit the model
modelRDA <- rda(Tratamiento~., data = train.transformed)
# Make predictions
predictions3 <- modelRDA %>% predict(test.transformed)
# Model accuracy
mean(predictions3$class == test.transformed$Tratamiento)
rda.data=cbind(train.transformed, predict(modelRDA)$x)
