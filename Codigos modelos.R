library(nortest)

#Modelos Abundancia Natural

Base_AbunNat<-read.table("Base_AbunNat.txt", header=TRUE, dec=",")

#N15_por<0.39 , N_por<8 , CN>9
Base_AbunNat$Int<-interaction(Base_AbunNat$Muestra, Base_AbunNat$Prof_T0)

Base_AbunNat1<-subset(Base_AbunNat, N15_por<0.371)

library(nlme)

#dN15
mod1<-gls(N15_por~N_por*Tratamiento*Int, data=Base_AbunNat1)
summary(mod1)
anova(mod1)
plot(mod1)
ad.test(resid(mod1))

mod2<-gls(N15_por~N_por*Tratamiento*Int,
         weights=varComb(varExp(form=~fitted(.))),
         data=Base_AbunNat1)
summary(mod2)
anova(mod2)
plot(mod2)
ad.test(resid(mod2))

anova(mod1,mod2)

ggplot(Base_AbunNat1, aes(x=N_por, y=N15_por)) + geom_point(aes(color=Int))
ggplot(Base_AbunNat1, aes(x=N_por, y=N15_por)) + geom_point(aes(color=Tratamiento))

library(multcomp)
library(lsmeans)

Comp1<-lsmeans(mod1, ~Tratamiento|Int)
pairs(Comp1)
cld(Comp1, nivel=0.05)
cld(Comp1, nivel=0.05, adj="none")

Comp2<-lsmeans(mod1, ~Int|Tratamiento)
pairs(Comp2)
cld(Comp2, nivel=0.05)
cld(Comp2, nivel=0.05, adj="none")


#C:N

Base_AbunNat_CN<-read.table("Base_AbunNat_CN.txt", header=TRUE, dec=",")

#N15_por<0.39 , N_por<8 , CN>9
Base_AbunNat_CN$Int<-interaction(Base_AbunNat_CN$Muestra, Base_AbunNat_CN$Prof_T0)

Base_AbunNat_CN1<-subset(Base_AbunNat_CN, N15_por<0.371)

ggplot(Base_AbunNat_CN1,
       aes(x=Tratamiento, y=C_N, fill=Int)) + geom_boxplot() + theme_bw() +
  labs(y="C:N", x="Tratamiento")

ggplot(Base_AbunNat_CN1,
       aes(x=Int, y=C_N, fill=Tratamiento)) + geom_boxplot() + theme_bw() +
  labs(y="C:N", x="Tratamiento")

mod3<-gls(log(C_N)~N_por*Tratamiento*Int, data=Base_AbunNat_CN1)
summary(mod3)
anova(mod3)
plot(mod3)
ad.test(resid(mod3))


mod4<-gls(log(C_N)~N_por*Tratamiento*Int,
          weights=varIdent(form=~1|Int),
          data=Base_AbunNat_CN1)
summary(mod4)
anova(mod4)
plot(mod4)
ad.test(resid(mod4))
hist(resid(mod4))

anova(mod3,mod4)
library(car)
qqPlot(resid(mod4))

library(ggplot2)
ggplot(Base_AbunNat_CN1, aes(x=N_por, y=C_N)) + geom_point(aes(color=Int))
ggplot(Base_AbunNat_CN1, aes(x=N_por, y=C_N)) + geom_point(aes(color=Tratamiento))

library(multcomp)
library(lsmeans)
Comp3<-lsmeans(mod4, ~Tratamiento|Int, mode = "boot-satterthwaite")
pairs(Comp3)
cld(Comp3, nivel=0.05)
cld(Comp3, nivel=0.05, adj="none")



Comp4<-lsmeans(mod4, ~Int|Tratamiento, mode = "boot-satterthwaite")
pairs(Comp4)
cld(Comp4, nivel=0.05)
cld(Comp4, nivel=0.05, adj="none")



#N (%)
mod5<-gls(log(N_por)~Tratamiento*Int, data=Base_AbunNat1)
summary(mod5)
anova(mod5)
plot(mod5)
ad.test(resid(mod5))

mod6<-gls(log(N_por)~Tratamiento*Int,
          weights=varComb(varExp(form=~fitted(.))),
          data=Base_AbunNat1)
summary(mod6)
anova(mod6)
plot(mod6)
ad.test(resid(mod6))

anova(mod5,mod6)

Comp5<-lsmeans(mod5, ~Tratamiento|Int)
pairs(Comp5)
cld(Comp5, nivel=0.05)

Comp6<-lsmeans(mod5, ~Int|Tratamiento)
pairs(Comp6)
cld(Comp6, nivel=0.05)


                  
#Modelos Mixtos 

library(ggplot2)
library(lme4)
library(nlme)

data_mod1<-subset(Base, Tratamiento !="2I" & Tratamiento !="4P" &
                    Muestra != "Hojarasca" & Profundidad1 != "0")

str(data_mod1)

data_mod1=subset(B_Foliar, Tratamiento !="2I" & Tratamiento !="4P" &
                   Muestra != "Hojarasca" & Profundidad1 != "0" & N15_por<0.3858)
head(data_mod1)
ggplot(data_mod1, aes(x=Tiempo1, y=N15_por, color=Tratamiento)) + geom_boxplot() + 
  facet_grid(Muestra~.)

mod1<-lme(N15_por~1+Tratamiento*Profundidad1*Tiempo1*Muestra,
          random=~1|Individuo, data=data_mod1,
          na.action=na.exclude, weights=varComb(varExp(form=~fitted(.))))
summary(mod1)
anova(mod1)
plot(mod1)
hist(resid(mod1))

library(multcomp)
library(multcompView)
library(lsmeans)

Comp1<-lsmeans(mod1, ~Tratamiento|Tiempo1+Muestra)
pairs(Comp1)
cld(Comp1, nivel=0.05)

Comp2<-lsmeans(mod1, ~Muestra|Tiempo1+Tratamiento)
pairs(Comp2)
cld(Comp2, nivel=0.05)


library(nlme)
mod2<-lme(dN15~1+Tratamiento+Profundidad1+Tiempo+Lote, random=~1|Individuo, data=B_Foliar)
summary(mod2)
library(car)
Anova(mod1)




#Modelos Enriquecimiento

subset(B_Foliar, N15_por<0.39 & N_por<8 & C_N<27.5)
subset(B_Raiz, N15_por<0.9 & N_por<4)
subset(B_Suelo, N15_por<0.475 & N_por<0.9 & C_N>7.5 & C_N<20)


library(ggplot2)
library(lme4)
library(nlme)

B_Enriq<-read.table("Base_Enriq.txt", header=TRUE, dec=",")
head(B_Enriq)

B_Enriq$Profundidad1<-as.factor(B_Enriq$Profundidad)

ggplot(B_Enriq, aes(x=Tiempo1, y=N15_por, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y=expression(paste(delta^{15}, "N foliar (atom %)")), x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ Muestra)+
  scale_fill_discrete(labels=c("CI","CP","CS"))

ggplot(B_Enriq, aes(x=Tiempo1, y=N_por, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y="N foliar (%)", x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ Muestra)+
  scale_fill_discrete(labels=c("CI","CP","CS"))

ggplot(B_Enriq, aes(x=Tiempo1, y=C_N, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y="C:N", x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ Muestra)+
  scale_fill_discrete(labels=c("CI","CP","CS"))


B_Enriq_Foliar<-subset(B_Enriq, Muestra=="Foliar")
B_Enriq_Raiz<-subset(B_Enriq, Muestra=="Raiz")
B_Enriq_Suelo<-subset(B_Enriq, Muestra=="Suelo")

ggplot(B_Enriq_Foliar, aes(x=Tiempo1, y=N15_por, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y=expression(paste(delta^{15}, "N foliar (atom %)")), x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ .)+
  scale_fill_discrete(labels=c("CI","CP","CS"))

ggplot(B_Enriq_Foliar, aes(x=Tiempo1, y=N_por, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y="N foliar (%)", x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ .)+
  scale_fill_discrete(labels=c("CI","CP","CS"))

ggplot(B_Enriq_Foliar, aes(x=Tiempo1, y=C_N, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y="C:N", x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ .)+
  scale_fill_discrete(labels=c("CI","CP","CS"))


ggplot(B_Enriq_Raiz, aes(x=Tiempo1, y=N15_por, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y=expression(paste(delta^{15}, "N Radicular (atom %)")), x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ .)+
  scale_fill_discrete(labels=c("CI","CP","CS"))

ggplot(B_Enriq_Raiz, aes(x=Tiempo1, y=N_por, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y="N Radicular (%)", x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ .)+
  scale_fill_discrete(labels=c("CI","CP","CS"))

ggplot(B_Enriq_Raiz, aes(x=Tiempo1, y=C_N, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y="C:N Radicular", x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ .)+
  scale_fill_discrete(labels=c("CI","CP","CS"))


ggplot(B_Enriq_Suelo, aes(x=Tiempo1, y=N15_por, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y=expression(paste(delta^{15}, "N Edáfico (atom %)")), x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ .)+
  scale_fill_discrete(labels=c("CI","CP","CS"))

ggplot(B_Enriq_Suelo, aes(x=Tiempo1, y=N_por, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y="N Edáfico (%)", x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ .)+
  scale_fill_discrete(labels=c("CI","CP","CS"))

ggplot(B_Enriq_Suelo, aes(x=Tiempo1, y=C_N, fill=Tratamiento)) + geom_boxplot() +
  theme_bw() +
  labs(y="C:N Edáfico", x="Tiempo (días)") +
  scale_x_discrete(labels=c("0","3","7","14","35")) +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(Profundidad ~ .)+
  scale_fill_discrete(labels=c("CI","CP","CS"))


#Modelo por Muestra

#Foliar - N15

mod1.1<-lme(N15_por~1+Tratamiento*Profundidad1*Tiempo1,
          random=~1|Individuo, data=B_Enriq_Foliar,
          na.action=na.exclude)

mod1.2<-lme(N15_por~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude, weights=varIdent(form=~1|Tratamiento))

mod1.3<-lme(N15_por~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude, weights=varIdent(form=~1|Profundidad1))

mod1.4<-lme(N15_por~1+Tratamiento*Profundidad1*Tiempo1,
          random=~1|Individuo, data=B_Enriq_Foliar,
          na.action=na.exclude, weights=varComb(varExp(form=~fitted(.))))

AIC(mod1.1,mod1.2,mod1.3,mod1.4)
BIC(mod1.1,mod1.2,mod1.3,mod1.4) #Mejor mod1.4
 
summary(mod1.4)
anova(mod1.4)
plot(mod1.4)
hist(resid(mod1.4))

library(multcomp)
library(multcompView)
library(lsmeans)

Comp1<-lsmeans(mod1.4, ~Tratamiento)
pairs(Comp1)
cld(Comp1, nivel=0.05)

Comp2<-lsmeans(mod1.3, ~Tiempo1)
pairs(Comp2)
cld(Comp2, nivel="0.05", adj="tukey")

boxplot(B_Enriq_Foliar$N15_por~B_Enriq_Foliar$Tiempo1)
tapply(B_Enriq_Foliar$N15_por,B_Enriq_Foliar$Tiempo1, mean)



#Foliar - N_por

mod2.1<-lme(log(N_por)~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude)

mod2.2<-lme(log(N_por)~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude, weights=varIdent(form=~1|Tratamiento))

mod2.3<-lme(log(N_por)~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude, weights=varIdent(form=~1|Profundidad1))

mod2.4<-lme(log(N_por)~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude, weights=varComb(varExp(form=~fitted(.))))

AIC(mod2.1,mod2.2,mod2.3,mod2.4)
BIC(mod2.1,mod2.2,mod2.3,mod2.4) #Mejor mod2.4

summary(mod2.4)
anova(mod2.4)
plot(mod2.4)
hist(resid(mod2.4))

library(multcomp)
library(multcompView)
library(lsmeans)

mod2.5<-lme(log(N_por)~1+Tratamiento*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude, weights=varComb(varExp(form=~fitted(.))))

AIC(mod2.4,mod2.5)
anova(mod2.4,mod2.5)

Comp1<-lsmeans(mod2.5, ~Tratamiento|Tiempo1)
pairs(Comp1)
cld(Comp1, nivel=0.05)

Comp2<-lsmeans(mod2.5, ~Tiempo1|Tratamiento)
pairs(Comp2)
cld(Comp2, nivel=0.05)


#Foliar - C:N

mod3.1<-lme(log(C_N)~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude)

mod3.2<-lme(log(C_N)~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude, weights=varIdent(form=~1|Tratamiento))

mod3.3<-lme(log(C_N)~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude, weights=varIdent(form=~1|Profundidad1))

mod3.4<-lme(log(C_N)~1+Tratamiento*Profundidad1*Tiempo1,
            random=~1|Individuo, data=B_Enriq_Foliar,
            na.action=na.exclude, weights=varComb(varExp(form=~fitted(.))))

AIC(mod3.1,mod3.2,mod3.3,mod3.4)
BIC(mod3.1,mod3.2,mod3.3,mod3.4) #Mejor mod3.4

summary(mod3.4)
anova(mod3.4)
plot(mod3.4)
hist(resid(mod3.4))

library(multcomp)
library(multcompView)
library(lsmeans)

Comp1<-lsmeans(mod3.4, ~Tratamiento|Tiempo1)
pairs(Comp1)
cld(Comp1, nivel=0.05)

Comp2<-lsmeans(mod3.4, ~Tiempo1|Tratamiento)
pairs(Comp2)
cld(Comp2, nivel=0.05)


tapply(B_Enriq_Foliar$C_N,interaction(B_Enriq_Foliar$Tiempo1, B_Enriq_Foliar$Tratamiento), mean)


