#    ----Tomando Café con Inga (CI) como planta de referencia no fijadora de N2----
Base_I_CI<-subset(BaseR, Tratamiento!="3CP" & Tratamiento!="4P" & Tratamiento!="5CS" & Tiempo1=="T0")




NF<-tapply(Base_I_CI_Foliar$N_mg, Base_I_CI_Foliar$Tratamiento, mean)
NR10<-tapply(Base_I_CI_Raiz_10$N_mg, Base_I_CI_Raiz_10$Tratamiento, mean)
NR15<-tapply(Base_I_CI_Raiz_15$N_mg, Base_I_CI_Raiz_15$Tratamiento, mean)

N_CI<-NF[1]+NR10[1]+NR15[1] #N_mg total en la planta de CI
N_I<-NF[2]+NR10[2]+NR15[2] #N_mg total en la planta de I

N_CI*100 #25 plantas/ha
N_I*100
#mg7ha
(N_I*200)/1000 #para gramos

#cuantas plantas de cafe se siembran en el cauca por Ha?
#cuantos guamos se siembran en el cauca por ha? 200?

##     --Fijacion Biologica de N2-----

#(N15_planta referencia-N15_planta fijadora)/(N15_planta referencia-B)

A <- (subset(Base_I_CI_Foliar, Tratamiento =="1CI")$dN15 - subset(Base_I_CI_Foliar, Tratamiento =="2I")$dN15)
A
B<-(subset(Base_I_CI_Foliar, Tratamiento =="1CI")$dN15-(-0.9))
B
C<-A/B
C
FBN<-C*100
FBN
mean(FBN) #valor promedio por planta

mean(subset(Base_I_CI_Foliar, Tratamiento =="1CI")$dN15)
mean(subset(Base_I_CI_Foliar, Tratamiento =="2I")$dN15)

mean(FBN)*N_I*25 #FBN Inga mg/h
mean(FBN)*N_I*70 #(70 arboles/ha es lo recomendado por Cenicafe para un cafetal con sombrio)

#FBN tomando N15 (atom%), el valor del FBN en % da mas logico utilizando el deltaN15 que viene en unidades %o (partes por mil)
A1 <- (subset(Base_I_CI_Foliar, Tratamiento =="1CI")$N15_por - subset(Base_I_CI_Foliar, Tratamiento =="2I")$N15_por)
A1
B1<-(subset(Base_I_CI_Foliar, Tratamiento =="1CI")$N15_por-(-0.9))
B1
C1<-A1*B1
FBN1<-C1*100
FBN1
mean(FBN1)

#    ----Platano como planta de referencia no fijadora de N2----

Base_I_P<-subset(BaseR, Tratamiento!="3CP" & Tratamiento!="1CI" & Tratamiento!="5CS" & Tiempo1=="T0")
Base_I_P_Foliar <- subset(Base_I_P, Muestra=="Foliar" & N_por<8 & N15_por<0.371)
Base_I_P_Raiz_10 <- subset(Base_I_P, Muestra=="Raiz" & N15_por<0.37 & Profundidad == "10")
Base_I_P_Raiz_15 <- subset(Base_I_P, Muestra=="Raiz" & N15_por<0.37 & Profundidad == "15")


##     --Fijacion Biologica de N2-----
#(N15_Platano-N15_Inga)/(N15_Platano-B)
A2 <- (subset(Base_I_P_Foliar, Tratamiento =="4P")$dN15 - subset(Base_I_P_Foliar, Tratamiento =="2I")$dN15)
A2
B2<-(subset(Base_I_P_Foliar, Tratamiento =="4P")$dN15-(-0.9))
B2
C2<-A2/B2
C2
FBN2<-C2*100
FBN2

mean(FBN2) #valor promedio por planta

mean(FBN2)*N_I2*25 #FBN Inga mg/h
mean(FBN2)*N_I2*70 #(70 arboles/ha es lo recomendado por Cenicafe para un cafetal con sombrio)

(mean(FBN2)*(N_I2*50))/100



#CANTIDAD DE N TOTAL EN PLANTAS

NF_I_P_<-tapply(Base_I_P_Foliar$N_mg, Base_I_P_Foliar$Tratamiento, mean)
NR10_I_P_<-tapply(Base_I_P_Raiz_10$N_mg, Base_I_P_Raiz_10$Tratamiento, mean)
NR15_I_P_<-tapply(Base_I_P_Raiz_15$N_mg, Base_I_P_Raiz_15$Tratamiento, mean)

#INGAS
N_I2<-NF_I_P_[1]+NR10_I_P_[1]+NR15_I_P_[1] #N_mg total en la planta de P
N_I2
N_I2*25
N_I2*50
N_I2*70

#PLATANO
N_P2<-NF_I_P_[2]+NR10_I_P_[2]+NR15_I_P_[2] #N_mg total en la planta de I
N_P2
N_P2*100

#25 plantas/ha
N_I2*200  #mg7ha
(N_I2*200)/1000 #para gramos


#Nitrogeno total en Ingas N en mg por cada kg de muestra seca

N_Ingas<-subset(BaseR, Tratamiento!="3CP" & Tratamiento!="4P" & Tratamiento!="5CS" & Tiempo1=="T0"& Tratamiento=="2I")

N_Ingas_F <- subset(N_Ingas, Muestra=="Foliar" & N_por<8 & N15_por<0.371)
N_Ingas_Raiz_10 <- subset(N_Ingas, Muestra=="Raiz" & N15_por<0.37 & Profundidad == "10")
N_Ingas_Raiz_15 <- subset(N_Ingas, Muestra=="Raiz" & N15_por<0.37 & Profundidad == "15")

Peso_Ingas_T<-N_Ingas_F$Peso + N_Ingas_Raiz_10$Peso + N_Ingas_Raiz_15$Peso 
Peso<-mean(Peso_Ingas_T)
Peso

#Nitrogeno total en Ingas -> N en gramos por cada kg de muestra seca -> solo hojas
Hojas<-mean(N_Ingas_F$Peso)#peso de muestras de hojas secas de inga
Hojas #pasar el peso de las muestras de mg a kg (dividir en 1000000)
NHojas<-mean(N_Ingas_F$N_mg)
NHojas #pasar el N de las muestras de mg a g (dividir en 1000)
NsoloH<-(NHojas/1000)/(Hojas/1000000)  #-> Nitrogeno total en Ingas -> N en gramos por cada kg de muestra seca -> solo hojas
NsoloH

#Nitrogeno total en Ingas -> N en gramos por cada kg de muestra seca ->  hojas y raices
N_Ingas_T<-N_Ingas_F$N_mg + N_Ingas_Raiz_10$N_mg + N_Ingas_Raiz_15$N_mg 
NT<-mean(N_Ingas_T)
N_HR<-(NT/1000)/(Peso/1000000)#<- Nitrogeno total en Ingas N en gramos por cada kg de muestra seca
N_HR

mean(N_Ingas_Raiz_10$N_mg) + mean(N_Ingas_Raiz_15$N_mg) + mean(N_Ingas_F$N_mg)


((mean(FBN))*((NT*1000000)/Peso))/100

N15_I<-mean(subset(Base_I_CI_Foliar, Tratamiento =="2I")$dN15)
N15_I
N15_CI<-mean(subset(Base_I_CI_Foliar, Tratamiento =="1CI")$dN15)
N15_CI
(N15_CI-N15_I)/(N15_CI-(-0.9))
0.645501/4.691193


R<-(subset(Base_I_CI_Foliar, Tratamiento =="1CI")$dN15) -(subset(Base_I_CI_Foliar, Tratamiento =="2I")$dN15)
T<-(subset(Base_I_CI_Foliar, Tratamiento =="1CI")$dN15) - (-0.9)
R/T
mean(R/T)*100
mean(T)
mean(R)/mean(T) #no es lo mismo promediar y hacer las operacines con los promedios, que usar todos los datos y promediar al final

N_FBN<-mean(R/T)*100
N_FBN

#N aportado desde la leguminosa a las hojas del CI
(N_FBN*NsoloH)/100

(N_FBN*N_HR)/100

32.4631-2.8063

Base_I_CI_Foliar <- subset(Base_I_CI, Muestra=="Foliar" & N_por<8 & N15_por<0.371)
Base_I_CI_Raiz_10 <- subset(Base_I_CI, Muestra=="Raiz" & N15_por<0.37 & Profundidad == "10")
Base_I_CI_Raiz_15 <- subset(Base_I_CI, Muestra=="Raiz" & N15_por<0.37 & Profundidad == "15")

mean(subset(Base_I_CI_Foliar, Tratamiento =="1CI")$Peso)
mean(subset(Base_I_CI_Raiz_10, Tratamiento =="1CI")$Peso)
mean(subset(Base_I_CI_Raiz_15, Tratamiento =="1CI")$Peso)

mean(subset(Base_I_CI_Foliar, Tratamiento =="2I")$Peso)
mean(subset(Base_I_CI_Raiz_10, Tratamiento =="2I")$Peso)
mean(subset(Base_I_CI_Raiz_15, Tratamiento =="2I")$Peso)

