#    -----Analisis discriminante datos abundancia natural------


#Cargar directorio Tesis Maestria
#Cargar Base de datos total

setwd("~/Documents/R/Tesis Maestria Univalle/Codigos R Analisis 2020")
Base <- read.table("Base_AbunNat.txt", header=TRUE, dec=",")

#Crear subsets por tipo de muestra y profundidad (raices y suelos)

Base_Foliar<-subset(Base, Muestra=="Foliar")
Base_Foliar1<-Base_Foliar[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]
Base_Foliar1  # esta contiene solo las 6 variables descritas arriba.

Base_Suelo<-subset(Base, Muestra=="Suelo")
Base_Suelo1<-Base_Suelo[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]
Base_Suelo1  
str(Base_Suelo1)

Base_Suelo10<-subset(Base_Suelo,Profundidad==10)[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]
str(Base_Suelo10)

Base_Suelo15<-subset(Base_Suelo,Profundidad==15)[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]
str(Base_Suelo15)

Base_Raiz<-subset(Base, Muestra=="Raiz")
Base_Raiz10<-subset(Base_Raiz,Profundidad==10)[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]
Base_Raiz15<-subset(Base_Raiz,Profundidad==15)[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]  
str(Base_Raiz10)
str(Base_Raiz15)
str(Base_Raiz)

Base_Hojarasca<-subset(Base, Muestra=="Hojarasca")
Base_Hojarasca1<-Base_Hojarasca[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]
Base_Hojarasca1  

#Cargar paquetes

library(reshape2)
library(knitr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(GGally)

##------Plots de ggpair-------

#ggpairs() -> Combina en un único gráfico diagramas de dispersión, distribución 
#de las variables y los valores de correlación

#FOLIAR ABUNDANCIA NATURAL
Hojas=ggpairs(Base_Foliar1[,-1], aes(colour = Base_Foliar1$Tratamiento, alpha = 0.4), 
        columnLabels = c("N (%)", "C (%)","N15 (atom%)","C13(atom%)", "C:N"))+
theme (text = element_text(family = "Arial",face = "bold",size=12),
       panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.background = element_rect(fill = NA, color = "black",size = 1),
       legend.background=element_rect(fill="white", colour="white"),
       legend.key=element_rect(colour="black",size = 0.1),
       legend.key.size=unit(1.8, "lines"),
       legend.position=("bottom"),
       legend.title = element_blank(),
       axis.text.x = element_text(size=6,face="bold"),
       axis.text.y = element_text(size=7.5,face="bold",family = "Arial"))
Hojas
ggsave("Hojas AN.png",plot=Hojas)#,dpi=400,width=16,height = 16,units = "cm")

#HOJARASCA 

Hojarasca=ggpairs(Base_Hojarasca1[,-1], aes(colour = Base_Hojarasca1$Tratamiento, alpha = 0.4), 
              columnLabels = c("N (%)", "C (%)","N15 (atom%)","C13(atom%)", "C:N"))+
  theme (text = element_text(family = "Arial",face = "bold",size=12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = NA, color = "black",size = 1),
         legend.background=element_rect(fill="white", colour="white"),
         legend.key=element_rect(colour="black",size = 0.1),
         legend.key.size=unit(1.8, "lines"),
         legend.position=("bottom"),
         legend.title = element_blank(),
         axis.text.x = element_text(size=6.2,face="bold"),
         axis.text.y = element_text(size=7.5,face="bold",family = "Arial"))
Hojarasca
ggsave("Hojarasca AN.png",plot=Hojarasca)#,dpi=400,width=16,height = 16,units = "cm")

#RAICES 10 CM 
Raices10=ggpairs(Base_Raiz10[,-1], aes(colour = Base_Raiz10$Tratamiento, alpha = 0.4), 
                  columnLabels = c("N (%)", "C (%)","N15 (atom%)","C13(atom%)", "C:N"))+
  theme (text = element_text(family = "Arial",face = "bold",size=12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = NA, color = "black",size = 1),
         legend.background=element_rect(fill="white", colour="white"),
         legend.key=element_rect(colour="black",size = 0.1),
         legend.key.size=unit(1.8, "lines"),
         legend.position=("bottom"),
         legend.title = element_blank(),
         axis.text.x = element_text(size=6.2,face="bold"),
         axis.text.y = element_text(size=7.5,face="bold",family = "Arial"))
Raices10
ggsave("Raices10 AN.png",plot=Raices10)

#RAICES 15 CM 
Raices15=ggpairs(Base_Raiz15[,-1], aes(colour = Base_Raiz15$Tratamiento, alpha = 0.4), 
                 columnLabels = c("N (%)", "C (%)","N15 (atom%)","C13(atom%)", "C:N"))+
  theme (text = element_text(family = "Arial",face = "bold",size=12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = NA, color = "black",size = 1),
         legend.background=element_rect(fill="white", colour="white"),
         legend.key=element_rect(colour="black",size = 0.1),
         legend.key.size=unit(1.8, "lines"),
         legend.position=("bottom"),
         legend.title = element_blank(),
         axis.text.x = element_text(size=6.2,face="bold"),
         axis.text.y = element_text(size=7.5,face="bold",family = "Arial"))
Raices15
ggsave("Raices15 AN.png",plot=Raices15)


#SUELOS 10 CM 
Suelos10=ggpairs(Base_Suelo10[,-1], aes(colour = Base_Suelo10$Tratamiento, alpha = 0.4), 
                 columnLabels = c("N (%)", "C (%)","N15 (atom%)","C13(atom%)", "C:N"))+
  theme (text = element_text(family = "Arial",face = "bold",size=12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = NA, color = "black",size = 1),
         legend.background=element_rect(fill="white", colour="white"),
         legend.key=element_rect(colour="black",size = 0.1),
         legend.key.size=unit(1.8, "lines"),
         legend.position=("bottom"),
         legend.title = element_blank(),
         axis.text.x = element_text(size=6,face="bold"),
         axis.text.y = element_text(size=7.5,face="bold",family = "Arial"))
Suelos10
ggsave("Suelos10 AN.png",plot=Suelos10)

#SUELOS 15 CM 
Suelos15=ggpairs(Base_Suelo15[,-1], aes(colour = Base_Suelo15$Tratamiento, alpha = 0.4), 
                 columnLabels = c("N (%)", "C (%)","N15 (atom%)","C13(atom%)", "C:N"))+
  theme (text = element_text(family = "Arial",face = "bold",size=12),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = NA, color = "black",size = 1),
         legend.background=element_rect(fill="white", colour="white"),
         legend.key=element_rect(colour="black",size = 0.1),
         legend.key.size=unit(1.8, "lines"),
         legend.position=("bottom"),
         legend.title = element_blank(),
         axis.text.x = element_text(size=6,face="bold"),
         axis.text.y = element_text(size=7.5,face="bold",family = "Arial"))
Suelos15
ggsave("Suelos15 AN.png",plot=Suelos15)

#Normalidad multivariante
library(kableExtra)
library(MVN)

#Test para evaluar la normalidad conjunta para todas las variables (multi-normalidad)

#mvn() -> Función del paquete MVN, que incluye argumentos para llevar a cabo tests y 
#gráficos de normalidad multivariante, detección de outliers multivariantes, tests y 
#gráficos de normalidad univariante

#Detección de outliers multivariantes
outliers <- mvn(data = Base_Foliar1[,-1], mvnTest = "hz", multivariateOutlierMethod = "quan")
outliers

#Con el paquete MVN podemos evaluar la normalidad multivariante con tres de los test comúnmente utilizados, 
#como el de Mardia, Royston y Henze-Zirkler, así como identificar los outliers multivariantes que puedan 
#influir en el contraste.

#Prueba de Royston
royston_test <- mvn(data = Base_Foliar1[,-1], mvnTest = "royston", multivariatePlot = "qq")
royston_test$multivariateNormality

#Prueba de Henze-Zirkler
hz_test <- mvn(data = Base_Foliar1[,-1], mvnTest = "hz")
hz_test$multivariateNormality


####  -----Matrices de Covarianza - Homogeneidad de varianza------

#boxM() -> Realiza el test M de Box (1949) para determinar la 
#homogeneidad de las matrices de covarianza obtenidas a partir 
#de datos normales multivariados según uno o más factores de clasificación. 

#Tiene como hipótesis nula que las matrices de covarianza son iguales. 
#IMP: sensible a la falta de normalidad multivariante

library(biotools)
boxM(data = Base_Foliar1[,-1], grouping = Base_Foliar1[,1])

head(Base_Foliar1)


#ANALISIS DISCRIMINANTE
#1. HECHO CON  WILMAR SIN TRANSFORMAR LOS DATOS

library(MASS)
modelo_lda <- lda(Tratamiento ~ N_por+N15_por+C_N, #C13_por C_por,
                  data = Base_Foliar1)
modelo_lda

predicciones <- predict(object = modelo_lda, newdata = Base_Foliar1[, -1])
table(Base_Foliar1$Tratamiento, predicciones$class, dnn = c("Clase real", "Clase predicha"))

trainig_error <- mean(Base_Foliar1$Tratamiento != predicciones$class) * 100
paste("trainig_error =", trainig_error, "%")

Base_Foliar1$Tratamiento<-as.factor(Base_Foliar1$Tratamiento)

library(klaR)
partimat(Tratamiento ~ N_por+N15_por+C_N, #C_por + C13_por
         data = Base_Foliar1, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),
         col.mean = "firebrick")

#partimat()-> permite representar los límites de clasificación de un modelo 
#discriminante lineal o cuadrático para cada par de predictores. 
#Cada color representa una región de clasificación acorde al modelo, 
#se muestra el centroide de cada región y el valor real de las observaciones.

#partimat()-> Proporciona una matriz de figuras múltiples que muestra la clasificación 
#de las observaciones basadas en métodos de clasificación (por ejemplo, lda, qda) para 
#cada combinación de dos variables. Además, se muestran los límites de clasificación y 
#las tasas de error aparentes se dan en cada título

