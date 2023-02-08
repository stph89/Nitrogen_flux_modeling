#    -Correlaciones entre las variables N15, N% y C:N-----

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


library(nortest)
library(psych)

#Foliar
#CI
B_Foliar2 <- subset(B_Foliar, Tiempo1=="T0" & N_por<8 & Tratamiento !="2I" & Tratamiento !="4P" & N15_por<0.371)
B_Foliar_Cor_CI <- subset(B_Foliar2, Tratamiento=="1CI")[,c("N_por","dN15","C_N")]
apply(B_Foliar_Cor_CI, 2, ad.test)
corr.test(B_Foliar_Cor_CI, method="pearson")

#CP
B_Foliar_Cor_CP <- subset(B_Foliar2, Tratamiento=="3CP")[,c("N_por","dN15","C_N")]
apply(B_Foliar_Cor_CP, 2, ad.test)
corr.test(B_Foliar_Cor_CP, method="pearson")
corr.test(B_Foliar_Cor_CP, method="spearman")

#CS
B_Foliar_Cor_CS <- subset(B_Foliar2, Tratamiento=="5CS")[,c("N_por","dN15","C_N")]
apply(B_Foliar_Cor_CS, 2, ad.test)
corr.test(B_Foliar_Cor_CS, method="pearson")


#Hojarasca
#CI
B_Hojarasca2<-subset(B_Hojarasca, N_por<4 & N15_por<0.369 & Tratamiento !="2I" & Tratamiento !="4P" )
B_Hojarasca_Cor_CI <- subset(B_Hojarasca2, Tratamiento=="1CI")[,c("N_por","dN15","C_N")]
apply(B_Hojarasca_Cor_CI, 2, ad.test)
corr.test(B_Hojarasca_Cor_CI, method="pearson")
corr.test(B_Hojarasca_Cor_CI, method="spearman")

#CP
B_Hojarasca_Cor_CP <- subset(B_Hojarasca2, Tratamiento=="3CP")[,c("N_por","dN15","C_N")]
apply(B_Hojarasca_Cor_CP, 2, ad.test)
corr.test(B_Hojarasca_Cor_CP, method="pearson")
corr.test(B_Hojarasca_Cor_CP, method="spearman")

#CS
B_Hojarasca_Cor_CS <- subset(B_Hojarasca2, Tratamiento=="5CS")[,c("N_por","dN15","C_N")]
apply(B_Hojarasca_Cor_CS, 2, ad.test)
corr.test(B_Hojarasca_Cor_CS, method="pearson")


#Raiz 10

#CI
B_Raiz_10_2 <- subset(B_Raiz, Tiempo1=="T0" & N15_por<0.37 & Tratamiento !="2I" & Tratamiento !="4P" & Profundidad==10)
B_Raiz_10_Cor_CI <- subset(B_Raiz_10_2, Tratamiento=="1CI")[,c("N_por","dN15","C_N")]
apply(B_Raiz_10_Cor_CI, 2, ad.test)
corr.test(B_Raiz_10_Cor_CI, method="spearman")
corr.test(B_Raiz_10_Cor_CI, method="pearson")

#CP
B_Raiz_10_Cor_CP <- subset(B_Raiz_10_2, Tratamiento=="3CP")[,c("N_por","dN15","C_N")]
apply(B_Raiz_10_Cor_CP, 2, ad.test)
corr.test(B_Raiz_10_Cor_CP, method="pearson")

#CS
B_Raiz_10_Cor_CS <- subset(B_Raiz_10_2, Tratamiento=="5CS")[,c("N_por","dN15","C_N")]
apply(B_Raiz_10_Cor_CS, 2, ad.test)
corr.test(B_Raiz_10_Cor_CS, method="pearson")


#Raiz 15
#CI
B_Raiz_15_2 <- subset(B_Raiz, Tiempo1=="T0" & N15_por<0.37 & Tratamiento !="2I" & Tratamiento !="4P" & Profundidad==15)
B_Raiz_15_Cor_CI <- subset(B_Raiz_15_2, Tratamiento=="1CI")[,c("N_por","dN15","C_N")]
apply(B_Raiz_15_Cor_CI, 2, ad.test)
corr.test(B_Raiz_15_Cor_CI, method="pearson")

#CP
B_Raiz_15_Cor_CP <- subset(B_Raiz_15_2, Tratamiento=="3CP")[,c("N_por","dN15","C_N")]
apply(B_Raiz_15_Cor_CP, 2, ad.test)
corr.test(B_Raiz_15_Cor_CP, method="pearson")

#CS
B_Raiz_15_Cor_CS <- subset(B_Raiz_15_2, Tratamiento=="5CS")[,c("N_por","dN15","C_N")]
apply(B_Raiz_15_Cor_CS, 2, ad.test)
corr.test(B_Raiz_15_Cor_CS, method="pearson")


#Suelo 10
#CI
B_Suelo_10_2 <- subset(B_Suelo, Tiempo1=="T0" & N15_por<0.37 & Tratamiento !="2I" & Tratamiento !="4P" & Profundidad==10)
B_Suelo_10_Cor_CI <- subset(B_Suelo_10_2, Tratamiento=="1CI")[,c("N_por","dN15","C_N")]
apply(B_Suelo_10_Cor_CI, 2, ad.test)
corr.test(B_Suelo_10_Cor_CI, method="pearson")

#CP
B_Suelo_10_Cor_CP <- subset(B_Suelo_10_2, Tratamiento=="3CP")[,c("N_por","dN15","C_N")]
apply(B_Suelo_10_Cor_CP, 2, ad.test)
corr.test(B_Suelo_10_Cor_CP, method="pearson")

#CS
B_Suelo_10_Cor_CS <- subset(B_Suelo_10_2, Tratamiento=="5CS")[,c("N_por","dN15","C_N")]
apply(B_Suelo_10_Cor_CS, 2, ad.test)
corr.test(B_Suelo_10_Cor_CS, method="spearman")


#Suelo 15
#CI
B_Suelo_15_2 <- subset(B_Suelo, Tiempo1=="T0" & N15_por<0.37 & Tratamiento !="2I" & Tratamiento !="4P" & Profundidad==15)
B_Suelo_15_Cor_CI <- subset(B_Suelo_15_2, Tratamiento=="1CI")[,c("N_por","dN15","C_N")]
apply(B_Suelo_15_Cor_CI, 2, ad.test)
corr.test(B_Suelo_15_Cor_CI, method="pearson")
corr.test(B_Suelo_15_Cor_CI, method="spearman")

#CP
B_Suelo_15_Cor_CP <- subset(B_Suelo_15_2, Tratamiento=="3CP")[,c("N_por","dN15","C_N")]
apply(B_Suelo_15_Cor_CP, 2, ad.test)
corr.test(B_Suelo_15_Cor_CP, method="pearson")

#CS
B_Suelo_15_Cor_CS <- subset(B_Suelo_15_2, Tratamiento=="5CS")[,c("N_por","dN15","C_N")]
apply(B_Suelo_15_Cor_CS, 2, ad.test)
corr.test(B_Suelo_15_Cor_CS, method="spearman")

