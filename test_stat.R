library(tidyverse)
library(lme4)
library(Rcpp)
library(lmerTest)
library(car)
library(ggplot2)

data_101121<-as_tibble(read.csv("Mesures_neighbors_101121.csv",header=TRUE,sep=","))
data_121121<-as_tibble(read.csv("Mesures_neighbors_121121.csv",header=TRUE,sep=","))
data_151121<-as_tibble(read.csv("Mesures_neighbors_151121.csv",header=TRUE,sep=","))
data_171121<-as_tibble(read.csv("Mesures_neighbors_171121.csv",header=TRUE,sep=","))
data_final<-as_tibble(read.csv("Mesures_neighbors_final.csv",header=TRUE,sep=","))
#Ajout jour dans les datasets et ajustement du nombre de colonnes

data_101121<-data_101121 %>% 
  mutate(Date_mesure="10/11/2021") %>% 
  mutate(plante_genotype = as.factor(plante_genotype))
data_121121<-data_121121 %>% 
  mutate(Date_mesure="12/11/2021") %>% 
  mutate(plante_genotype = as.factor(plante_genotype))
data_151121<-data_151121 %>% 
  mutate(Date_mesure="15/11/2021") %>% 
  mutate(plante_genotype = as.factor(plante_genotype))
data_171121<-data_171121 %>% 
  mutate(Date_mesure="17/11/2021") %>% 
  mutate(plante_genotype = as.factor(plante_genotype))

data<-rbind(data_101121,data_121121,data_151121,data_171121)

lm1_1<-lm(as.numeric(Longueur_feuille_2) ~ as.numeric(poids_graine) ,data= data_final)
plot(lm1_1)
shapiro.test(lm1_1$residuals)
bartlett.test(Longueur_feuille_2 ~ poids_graine ,data= data_final)
summary(lm1_1)
anova(lm1_1)

lm1_2<-lm(as.numeric(Largeur_feuille_2) ~ as.numeric(poids_graine) ,data= data_final)
plot(lm1_2)
shapiro.test(lm1_2$residuals)
bartlett.test(Largeur_feuille_2 ~ poids_graine ,data= data_final)
summary(lm1_2)
anova(lm1_2)

lm1_3<-lm(as.numeric(vitesse_coleoptile_feuille_1) ~ as.numeric(poids_graine) ,data= data_final)
plot(lm1_3)
shapiro.test(lm1_3$residuals)
bartlett.test(as.numeric(vitesse_coleoptile_feuille_1) ~ as.numeric(poids_graine) ,data= data_final)
summary(lm1_3)
anova(lm1_3)

lm1_4<-lm(as.numeric(vitesse_coleoptile_feuille_2) ~ as.numeric(poids_graine) ,data= data_final)
plot(lm1_4)
shapiro.test(lm1_4$residuals)
bartlett.test(as.numeric(vitesse_coleoptile_feuille_2) ~ as.numeric(poids_graine) ,data= data_final)
summary(lm1_4)
anova(lm1_4)

lm1_5<-lm(as.numeric(vitesse_longueur_f1) ~ as.numeric(poids_graine) ,data= data_final)
plot(lm1_5)
shapiro.test(lm1_5$residuals)
bartlett.test(as.numeric(vitesse_longueur_f1) ~ as.numeric(poids_graine) ,data= data_final)
summary(lm1_5)
anova(lm1_5)

lm1_6<-lm(as.numeric(vitesse_largeur_f1) ~ as.numeric(poids_graine) ,data= data_final)
plot(lm1_6)
shapiro.test(lm1_6$residuals)
bartlett.test(as.numeric(vitesse_largeur_f1) ~ as.numeric(poids_graine) ,data= data_final)
summary(lm1_6)
anova(lm1_6)


lm2_1<-lm(as.numeric(Longueur_feuille_2) ~ as.numeric(plante_genotype) ,data= data_final)
plot(lm2_1)
shapiro.test(lm2_1$residuals)
bartlett.test(as.numeric(Longueur_feuille_2) ~ as.numeric(plante_genotype) ,data= data_final)
summary(lm2_1)
anova(lm2_1)

lm2_2<-lm(as.numeric(Largeur_feuille_2) ~ as.numeric(plante_genotype) ,data= data_final)
plot(lm2_2)
shapiro.test(lm2_2$residuals)
bartlett.test(as.numeric(Largeur_feuille_2) ~ as.numeric(plante_genotype) ,data= data_final)
summary(lm2_2)
anova(lm2_2)

lm2_3<-lm(as.numeric(vitesse_coleoptile_feuille_1) ~ as.numeric(plante_genotype) ,data= data_final)
plot(lm2_3)
shapiro.test(lm2_3$residuals)
bartlett.test(as.numeric(vitesse_coleoptile_feuille_1) ~ as.numeric(plante_genotype) ,data= data_final)
summary(lm2_3)
anova(lm2_3)

lm2_4<-lm(as.numeric(vitesse_coleoptile_feuille_2) ~ as.numeric(plante_genotype) ,data= data_final)
plot(lm2_4)
shapiro.test(lm2_4$residuals)
bartlett.test(as.numeric(vitesse_coleoptile_feuille_2) ~ as.numeric(plante_genotype) ,data= data_final)
summary(lm2_4)
anova(lm2_4)

lm2_5<-lm(as.numeric(vitesse_longueur_f1) ~ plante_genotype + Position + Monoculture_Couple ,data= data_final)
plot(lm2_5)
shapiro.test(lm2_5$residuals)
bartlett.test(as.numeric(vitesse_longueur_f1) ~ as.numeric(plante_genotype) ,data= data_final)
summary(lm2_5)
anova(lm2_5)

lm2_6<-lm(as.numeric(vitesse_largeur_f1) ~ as.numeric(plante_genotype) ,data= data_final)
plot(lm2_6)
shapiro.test(lm2_6$residuals)
bartlett.test(as.numeric(vitesse_largeur_f1) ~ as.numeric(plante_genotype) ,data= data_final)
summary(lm2_6)
anova(lm2_6)

lm3_1<-lm(as.numeric(Longueur_feuille_2) ~ Position ,data= data_final)
plot(lm3_1)
shapiro.test(lm3_1$residuals)
bartlett.test(as.numeric(Longueur_feuille_2) ~ Position ,data= data_final)
summary(lm3_1)
anova(lm3_1)

lm3_2<-lm(as.numeric(Largeur_feuille_2) ~ Position ,data= data_final)
plot(lm3_2)
shapiro.test(lm3_2$residuals)
bartlett.test(as.numeric(Largeur_feuille_2) ~ Position ,data= data_final)
summary(lm3_2)
anova(lm3_2)

lm3_3<-lm(as.numeric(vitesse_coleoptile_feuille_1) ~ Position ,data= data_final)
plot(lm3_3)
shapiro.test(lm3_3$residuals)
bartlett.test(as.numeric(vitesse_coleoptile_feuille_1) ~ Position ,data= data_final)
summary(lm3_3)
anova(lm3_3)

lm3_4<-lm(as.numeric(vitesse_coleoptile_feuille_2) ~ Position ,data= data_final)
plot(lm3_4)
shapiro.test(lm3_4$residuals)
bartlett.test(as.numeric(vitesse_coleoptile_feuille_2) ~ Position ,data= data_final)
summary(lm3_4)
anova(lm3_4)

lm3_5<-lm(as.numeric(vitesse_longueur_f1) ~ Position ,data= data_final)
plot(lm3_5)
shapiro.test(lm3_5$residuals)
bartlett.test(as.numeric(vitesse_longueur_f1) ~ Position ,data= data_final)
summary(lm3_5)
anova(lm3_5)

lm3_6<-lm(as.numeric(vitesse_largeur_f1) ~ Position ,data= data_final)
plot(lm3_6)
shapiro.test(lm3_6$residuals)
bartlett.test(as.numeric(vitesse_largeur_f1) ~ Position ,data= data_final)
summary(lm3_6)
anova(lm3_6)

lm4_1<-lm(as.numeric(Longueur_feuille_2) ~ Monoculture_Couple ,data= data_final)
plot(lm4_1)
shapiro.test(lm4_1$residuals)
bartlett.test(as.numeric(Longueur_feuille_2) ~ Monoculture_Couple ,data= data_final)
summary(lm4_1)
anova(lm4_1)

lm4_2<-lm(as.numeric(Largeur_feuille_2) ~ Monoculture_Couple ,data= data_final)
plot(lm4_2)
shapiro.test(lm4_2$residuals)
bartlett.test(as.numeric(Largeur_feuille_2) ~ Monoculture_Couple ,data= data_final)
summary(lm4_2)
anova(lm4_2)

lm4_3<-lm(as.numeric(vitesse_coleoptile_feuille_1) ~ Monoculture_Couple ,data= data_final)
plot(lm4_3)
shapiro.test(lm4_3$residuals)
bartlett.test(as.numeric(vitesse_coleoptile_feuille_1) ~ Monoculture_Couple ,data= data_final)
summary(lm4_3)
anova(lm4_3)

lm4_4<-lm(as.numeric(vitesse_coleoptile_feuille_2) ~ Monoculture_Couple ,data= data_final)
plot(lm4_4)
shapiro.test(lm4_4$residuals)
bartlett.test(as.numeric(vitesse_coleoptile_feuille_2) ~ Monoculture_Couple ,data= data_final)
summary(lm4_4)
anova(lm4_4)

lm4_5<-lm(as.numeric(vitesse_longueur_f1) ~ Monoculture_Couple ,data= data_final)
plot(lm4_5)
shapiro.test(lm4_5$residuals)
bartlett.test(as.numeric(vitesse_longueur_f1) ~ Monoculture_Couple ,data= data_final)
summary(lm4_5)
anova(lm4_5)

lm4_6<-lm(as.numeric(vitesse_largeur_f1) ~ Monoculture_Couple ,data= data_final)
plot(lm4_6)
shapiro.test(lm4_6$residuals)
bartlett.test(as.numeric(vitesse_largeur_f1) ~ Monoculture_Couple ,data= data_final)
summary(lm4_6)
anova(lm4_6)

### Modèle mixte

mod_mixte_1 <- lmer(Longueur_feuille_1 ~poids_graine + Position + (1|plante_genotype) ,data= data)
summary(mod_mixte_1)
Anova(mod_mixte_1)

test_position<-lm(taille_coleotile_feuille_1 ~ Bloc + Colonne + Ligne ,data= data)
summary(test_position)
anova(test_position)


test<-lm(taille_coleotile_feuille_1 ~ plante_genotype*Monoculture_Couple ,data= data)
summary(test)
anova(test)

?Anova
?arrange
ggplot(data=data_spd,aes(x=Date_mesure,y=taille_coleotile_feuille_1,group=Couple))+
  geom_point()



