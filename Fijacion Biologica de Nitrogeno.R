setwd("~/Documents/R/Tesis Maestria Univalle/Codigos R Analisis 2020")

Base<-read.table("BaseR.txt", header=TRUE, dec=",", na.strings = "NA")
head(Base)
str(Base)
Base$Profundidad1<-as.factor(Base$Profundidad)

library(ggplot2)

B_Foliar<-subset(Base, Muestra=="Foliar")
B_Raiz<-subset(Base, Muestra=="Raiz")
B_Suelo<-subset(Base, Muestra=="Suelo")
B_Hojarasca<-subset(Base, Muestra=="Hojarasca")

Base_Foliar1<-Base_Foliar[,c("Tratamiento","N_por", "C_por","N15_por","C13_por", "C_N")]

#Delta15N de Ingas y Platanos
FBN_Foliar <- subset(B_Foliar, Tiempo1=="T0" & N_por<8 & Tratamiento !="1CI" & Tratamiento !="3CP" & Tratamiento !="5CS"& N15_por<0.371)
head(FBN_Foliar)
str(FBN_Foliar)

FBN_Foliar_Inga<-subset(FBN_Foliar,Tratamiento!="4P")
FBN_Inga<-FBN_Foliar_Inga[,c("dN15")]
str(FBN_Inga)
head(FBN_Inga)

FBN_Foliar_Platano<-subset(FBN_Foliar,Tratamiento!="2I")
FBN_Platano<-FBN_Foliar_Platano[,c("dN15")]


#(N15_Platano-N15_Inga)/(N15_Platano-B)

A<-mean(FBN_Platano-FBN_Inga)
A
B<-mean(FBN_Platano-(-0.9))
B
C<-A/B
C
FBN<-C*100
FBN

mean(FBN)

#Contribucion de la Fijacion Biologica de Nitrogeno por Inga densiflora
FBN_Total<-(mean(FBN_Platano-FBN_Inga)/mean(FBN_Platano-(-0.9)))
FBN_Total

#Cantidad de N derivado de Fijacion Biologica de N atmosferico
N_Inga<-FBN_Foliar_Inga[,c("N_mg")]
NTotal_Inga<-mean(N_Inga)
NTotal_Inga

Cont_FBN<-FBN_Total/NTotal_Inga
Cont_FBN

I<-(54.71*0.076)/100
I
