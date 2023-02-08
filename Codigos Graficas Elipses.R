Base<-read.table("BaseR.txt", header=TRUE, dec=",", na.strings = "NA")
head(Base)

Base$Profundidad1<-as.factor(Base$Profundidad)

library(ggplot2)

B_Foliar<-subset(Base, Muestra=="Foliar")
B_Raiz<-subset(Base, Muestra=="Raiz")
B_Suelo<-subset(Base, Muestra=="Suelo")
B_Hojarasca<-subset(Base, Muestra=="Hojarasca")


install.packages("extrafont")
library(extrafont)
font_import()
fonts()

#Abundancia natural - T0
#%N15 vs %NTotal por muestra entre tratamientos
N15_Foliar<-ggplot(subset(B_Foliar, Tiempo1=="T0" & N_por<8 & Tratamiento !="2I" & Tratamiento !="4P" & N15_por<0.371),
                   aes(x=N_por, y=N15_por,color=Tratamiento)) + geom_point() +
  stat_ellipse(aes(color=Tratamiento),type = "t", level=0.95) +theme_bw() +
  labs(y=expression(paste(delta^{15}, "N Foliar (atom %)")), x="N Foliar (%)") +
  theme(text=element_text(size=12, family="Arial")) +
  scale_color_discrete(labels=c("CI", "CP","CS")) +
  ylim(c(0.365,0.3715))

N15_Raiz<-ggplot(subset(B_Raiz, Tiempo1=="T0" & N15_por<0.37 & Tratamiento !="2I" & Tratamiento !="4P" ),
                 aes(x=N_por, y=N15_por,color=Tratamiento)) + geom_point() +
  stat_ellipse(aes(color=Tratamiento),type = "t", level=0.95) +theme_bw() +
  labs(y=expression(paste(delta^{15}, "N Radicular (atom %)")), x="N Radicular (%)") +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(.~Profundidad, labeller=as_labeller(c(`10` = "Profundidad 10cm",
                                                   `15` = "Profundidad 15cm"))) +
  scale_color_discrete(labels=c("CI", "CP","CS")) +
  ylim(c(0.365,0.3715))

N15_Suelo<-ggplot(subset(B_Suelo, Tiempo1=="T0" & N15_por<0.37 & Tratamiento !="2I" & Tratamiento !="4P" ),
                  aes(x=N_por, y=N15_por,color=Tratamiento)) + geom_point() +
  stat_ellipse(aes(color=Tratamiento),type = "t", level=0.95) +theme_bw() +
  labs(y=expression(paste(delta^{15}, "N Edáfico (atom %)")), x="N Edáfico (%)") +
  theme(text=element_text(size=12, family="Arial")) +
  facet_grid(.~Profundidad, labeller=as_labeller(c(`10` = "Profundidad 10cm",
                                                   `15` = "Profundidad 15cm"))) +
  scale_color_discrete(labels=c("CI", "CP","CS")) +
  ylim(c(0.365,0.3715))

N15_Hojarasca<-ggplot(subset(B_Hojarasca, N_por<4 & N15_por<0.369 & Tratamiento !="2I" & Tratamiento !="4P" ),
                      aes(x=N_por, y=N15_por,color=Tratamiento)) + geom_point() +
  stat_ellipse(aes(color=Tratamiento),type = "t", level=0.95) +theme_bw() +
  labs(y=expression(paste(delta^{15}, "N Hojarasca (atom %)")), x="N Hojarasca (%)") +
  theme(text=element_text(size=12, family="Arial")) +
  scale_color_discrete(labels=c("CI", "CP","CS")) +
  ylim(c(0.365,0.3715))

jpeg("N15_Foliar.jpeg", width=18, height=12, units="cm", res=600, quality = 100)
print(N15_Foliar)
dev.off()

jpeg("N15_Raiz.jpeg", width=18, height=12, units="cm", res=600, quality = 100)
print(N15_Raiz)
dev.off()

jpeg("N15_Suelo.jpeg", width=18, height=12, units="cm", res=600, quality = 100)
print(N15_Suelo)
dev.off()

jpeg("N15_Hojarasca.jpeg", width=16, height=12, units="cm", res=600, quality = 100)
print(N15_Hojarasca)
dev.off()


#Abundancia natural - T0
#%N15 vs %NTotal por tratamiento entre muestras 


CI_plot<-subset(Base, Tratamiento=="1CI" & Tiempo1 =="T0" & N15_por<0.369)
CI_plot$Int<-interaction(CI_plot$Muestra, CI_plot$Prof_T0)

N15_CI<-ggplot(CI_plot,
               aes(x=N_por, y=N15_por,color=Int)) + geom_point() +
  stat_ellipse(aes(color=Int),type = "t", level=0.95) +theme_bw() +
  labs(y=expression(paste(delta^{15}, "N (atom %)")), x="N (%)") +
  theme(text=element_text(size=12, family="Arial")) +
  ylim(c(0.365,0.3715)) +
  scale_color_discrete("Muestra", labels=c("Foliar", "Hojarasca","Raiz 10cm","Suelo 10cm","Raiz 15cm","Suelo 15cm"))


CP_plot<-subset(Base, Tratamiento=="3CP" & Tiempo1 =="T0" & N15_por<0.37)
CP_plot$Int<-interaction(CP_plot$Muestra, CP_plot$Prof_T0)

N15_CP<-ggplot(CP_plot,
               aes(x=N_por, y=N15_por,color=Int)) + geom_point() +
  stat_ellipse(aes(color=Int),type = "t", level=0.95) +theme_bw() +
  labs(y=expression(paste(delta^{15}, "N (atom %)")), x="N (%)") +
  theme(text=element_text(size=12, family="Arial")) +
  ylim(c(0.365,0.3715)) +
  scale_color_discrete("Muestra", labels=c("Foliar", "Hojarasca","Raiz 10cm","Suelo 10cm","Raiz 15cm","Suelo 15cm"))


CS_plot<-subset(Base, Tratamiento=="5CS" & Tiempo1 =="T0" & N_por<7 & N15_por<0.37)
CS_plot$Int<-interaction(CS_plot$Muestra, CS_plot$Prof_T0)

N15_CS<-ggplot(CS_plot,
               aes(x=N_por, y=N15_por,color=Int)) + geom_point() +
  stat_ellipse(aes(color=Int),type = "t", level=0.95) +theme_bw() +
  labs(y=expression(paste(delta^{15}, "N (atom %)")), x="N (%)") +
  theme(text=element_text(size=12, family="Arial")) +
  ylim(c(0.365,0.3715)) +
  scale_color_discrete("Muestra", labels=c("Foliar", "Hojarasca","Raiz 10cm","Suelo 10cm","Raiz 15cm","Suelo 15cm"))


I_plot<-subset(Base, Tratamiento=="2I" & Tiempo1 =="T0" & N15_por<0.37)
I_plot$Int<-interaction(I_plot$Muestra, I_plot$Prof_T0)

N15_I<-ggplot(I_plot,
              aes(x=N_por, y=N15_por,color=Int)) + geom_point() +
  stat_ellipse(aes(color=Int),type = "t", level=0.95) +theme_bw() +
  labs(y=expression(paste(delta^{15}, "N (atom %)")), x="N (%)") +
  theme(text=element_text(size=12, family="Arial")) +
  ylim(c(0.365,0.3715)) +
  scale_color_discrete("Muestra", labels=c("Foliar", "Hojarasca","Raiz 10cm","Suelo 10cm","Raiz 15cm","Suelo 15cm"))


P_plot <- subset(Base, Tratamiento=="4P" & Tiempo1 =="T0" & N15_por<0.3703 & N_por<5)
P_plot$Int <- interaction(P_plot$Muestra, P_plot$Prof_T0)

N15_P<-ggplot(P_plot,
              aes(x=N_por, y=N15_por,color=Int)) + geom_point() +
  stat_ellipse(aes(color=Int),type = "t", level=0.95) + theme_bw() +
  labs(y=expression(paste(delta^{15}, "N (atom %)")), x="N (%)") +
  theme(text=element_text(size=12, family="Arial")) +
  ylim(c(0.365,0.3715)) +
  scale_color_discrete("Muestra", labels=c("Foliar", "Hojarasca","Raiz 10cm","Suelo 10cm","Raiz 15cm","Suelo 15cm"))


jpeg("N15_CI.jpeg", width=20, height=12, units="cm", res=600, quality = 100)
print(N15_CI)
dev.off()

jpeg("N15_CP.jpeg", width=20, height=12, units="cm", res=600, quality = 100)
print(N15_CP)
dev.off()

jpeg("N15_CS.jpeg", width=20, height=12, units="cm", res=600, quality = 100)
print(N15_CS)
dev.off()

jpeg("N15_I.jpeg", width=20, height=12, units="cm", res=600, quality = 100)
print(N15_I)
dev.off()

jpeg("N15_P.jpeg", width=20, height=12, units="cm", res=600, quality = 100)
print(N15_P)
dev.off()


