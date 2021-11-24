library(tidyverse)
library(lme4)

data_101121<-as_tibble(read.csv("Mesures_neighbors_101121.csv",header=TRUE,sep=","))
data_121121<-as_tibble(read.csv("Mesures_neighbors_121121.csv",header=TRUE,sep=","))
data_151121<-as_tibble(read.csv("Mesures_neighbors_151121.csv",header=TRUE,sep=","))
data_171121<-as_tibble(read.csv("Mesures_neighbors_171121.csv",header=TRUE,sep=","))

#Ajout jour dans les datasets et ajustement du nombre de colonnes
data_101121<-data_101121 %>% 
  mutate(Date_mesure="10/11/2021")
data_121121<-data_121121 %>% 
  mutate(Date_mesure="12/11/2021")
data_151121<-data_151121 %>% 
  mutate(Date_mesure="15/11/2021")
data_171121<-data_171121 %>% 
  mutate(Date_mesure="17/11/2021") %>% 
  mutate(plante_genotype = as.factor(plante_genotype))

data<-rbind(data_101121,data_121121,data_151121,data_171121)
lmpoids_graine<-lm(Longueur_feuille_1~ plante_genotype  ,data= data_171121)
plot(lmpoids_graine)
summary(lmpoids_graine)
shapiro.test(lmpoids_graine$residuals)
bartlett.test(Longueur_feuille_1~poids_graine,data=data_171121)
?bartlett.test

### Modèle mixte

mod_mixte <- lmer(Longueur_feuille_1 ~ Monoculture_Couple  + (1|Position) ,data= data_171121)

