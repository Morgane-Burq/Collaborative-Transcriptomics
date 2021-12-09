library(tidyverse)
library(lme4)
library(Rcpp)
library(lmerTest)
library(car)
library(ggplot2)
library(cowplot)
# Gestion dataset
data_final<-as_tibble(read.csv("Mesures_neighbors_final.csv",header=TRUE,sep=","))
names(data_final)
data_dates<-read.csv("Mesures_neighbors_dates.csv",header=TRUE,sep=",")
liste<-which(!is.na(data_dates$date_ligule_1)&(is.na(data_dates$date_ligule_2)))
data_dates$date_ligule_2=as.numeric(substr(data_dates$date_ligule_2,1,2))
data_final<-data_final %>% 
  mutate(plante_genotype=as.character(plante_genotype),
         Nombre_de_feuille_ligulÃ.e=as.numeric(Nombre_de_feuille_ligulÃ.e),
         Longueur_feuille_2=as.numeric(Longueur_feuille_2),
         vitesse_longueur_f1=as.numeric(vitesse_longueur_f1),
         vitesse_largeur_f1=as.numeric(vitesse_largeur_f1),
         poids_graine=as.numeric(poids_graine),
         vitesse_coleoptile_feuille_1=as.numeric(vitesse_coleoptile_feuille_1),
         vitesse_coleoptile_feuille_2=as.numeric(vitesse_coleoptile_feuille_2),
         Bloc=as.factor(Bloc),
         Colonne=as.factor(Colonne),
         Ligne=as.factor(Ligne)) %>% 
  filter(Longueur_feuille_2>50)
data_dates<-data_dates[-liste3,] %>% 
  mutate(poids_graine=as.numeric(poids_graine),
         plante_genotype=as.factor(plante_genotype),
         Bloc=as.factor(Bloc),
         Colonne=as.factor(Colonne),
         Ligne=as.factor(Ligne),
         Condition_hydrique=as.factor(Condition_hydrique),
         Monoculture_Couple=as.factor(Monoculture_Couple),
         date_ligule_2=as.numeric(substr(date_ligule_2,1,2))) %>% 
  filter(!is.na(date_ligule_2)) 
liste2<-which(is.na(data_final$Longueur_feuille_2))
liste3<-which(data_final$Couple%in%data_final$Couple[liste2])
#Graphs généraux
data<-data_final %>% 
  select(poids_graine,plante_genotype) %>% 
  group_by(plante_genotype) %>% 
  filter(is.na(poids_graine)==FALSE) %>% 
  summarise(Moyenne_poids_graine=mean(poids_graine)) %>% 
  arrange(Moyenne_poids_graine)
i=10
data_final<-as.data.frame(data_final)
for (i in c(10:14,17:20)){
  print(hist(data_final[,i],main=names(data_final)[i]))
}

data$plante_genotype<-fct_relevel(data$plante_genotype,c("118","235","114","265","130","89","74","348","165","482","395","412","329","131","376"))

ggplot(data=data,aes(x=plante_genotype,y=Moyenne_poids_graine,fill=plante_genotype))+
  geom_bar(stat = "identity",show.legend = FALSE)+
  theme_cowplot()

hist(data_dates$date_ligule_2)

#vitesse croissance longueur feuille 1
##Quelques graphs
barplot_speed_length_1_Mono_couple<-ggplot(data=data_final,aes(x=Monoculture_Couple,y=vitesse_longueur_f1,fill=Monoculture_Couple))+
  geom_boxplot()
barplot_speed_length_1_Mono_couple




##Question 1
lm_speed_length_l1_q1_h0<-lm(vitesse_longueur_f1~1,data=data_final)
lm_speed_length_l1_q1_h1<-lm(vitesse_longueur_f1~plante_genotype,data=data_final)
plot(lm_speed_length_l1_q1_h0)
summary(lm_speed_length_l1_q1_h0)
plot(lm_speed_length_l1_q1_h1)
summary(lm_speed_length_l1_q1_h1)
anova(lm_speed_length_l1_q1_h0,lm_speed_length_l1_q1_h1)

## Question 2
lm_speed_length_l1_q2_h0<-lm(vitesse_longueur_f1~plante_genotype,data=data_final)
lm_speed_length_l1_q2_h1<-lm(vitesse_longueur_f1~plante_genotype+Position,data=data_final)
plot(lm_speed_length_l1_q2_h0)
summary(lm_speed_length_l1_q2_h0)
plot(lm_speed_length_l1_q2_h1)
summary(lm_speed_length_l1_q2_h1)
anova(lm_speed_length_l1_q2_h0,lm_speed_length_l1_q2_h1)

##Question 3
lm_speed_length_l1_q3_h0<-lm(vitesse_longueur_f1~plante_genotype+Position,data=data_final)
lm_speed_length_l1_q3_h1<-lm(vitesse_longueur_f1~plante_genotype*Position,data=data_final)
plot(lm_speed_length_l1_q3_h0)
summary(lm_speed_length_l1_q3_h0)
plot(lm_speed_length_l1_q3_h1)
summary(lm_speed_length_l1_q3_h1)
anova(lm_speed_length_l1_q3_h0,lm_speed_length_l1_q3_h1)

##Question 4 
lm_speed_length_l1_q4_h0<-lm(vitesse_longueur_f1~plante_genotype,data=data_final)
lm_speed_length_l1_q4_h1<-lm(vitesse_longueur_f1~plante_genotype+Monoculture_Couple,data=data_final)
plot(lm_speed_length_l1_q4_h0)
summary(lm_speed_length_l1_q4_h0)
plot(lm_speed_length_l1_q4_h1)
summary(lm_speed_length_l1_q4_h1)
anova(lm_speed_length_l1_q4_h0,lm_speed_length_l1_q4_h1)

##Question 5
lm_speed_length_l1_q5_h0<-lm(vitesse_longueur_f1~plante_genotype+Monoculture_Couple,data=data_final)
lm_speed_length_l1_q5_h1<-lm(vitesse_longueur_f1~plante_genotype*Monoculture_Couple,data=data_final)
plot(lm_speed_length_l1_q5_h0)
summary(lm_speed_length_l1_q5_h0)
plot(lm_speed_length_l1_q5_h1)
summary(lm_speed_length_l1_q5_h1)
anova(lm_speed_length_l1_q5_h0,lm_speed_length_l1_q5_h1)

##Question 6
lm_speed_length_l1_q6_h0<-lm(vitesse_longueur_f1~plante_genotype,data=data_final)
lm_speed_length_l1_q6_h1<-lm(vitesse_longueur_f1~plante_genotype+Condition_hydrique,data=data_final)
plot(lm_speed_length_l1_q6_h0)
summary(lm_speed_length_l1_q6_h0)
plot(lm_speed_length_l1_q6_h1)
summary(lm_speed_length_l1_q6_h1)
anova(lm_speed_length_l1_q6_h0,lm_speed_length_l1_q6_h1)

##Question 7
lm_speed_length_l1_q7_h0<-lm(vitesse_longueur_f1~plante_genotype+Condition_hydrique,data=data_final)
lm_speed_length_l1_q7_h1<-lm(vitesse_longueur_f1~plante_genotype*Condition_hydrique,data=data_final)
plot(lm_speed_length_l1_q7_h0)
summary(lm_speed_length_l1_q7_h0)
plot(lm_speed_length_l1_q7_h1)
summary(lm_speed_length_l1_q7_h1)
anova(lm_speed_length_l1_q7_h0,lm_speed_length_l1_q7_h1)

##Question 8
lm_speed_length_l1_q8_h0<-lm(vitesse_longueur_f1~plante_genotype+Condition_hydrique+Monoculture_Couple,data=data_final)
lm_speed_length_l1_q8_h1<-lm(vitesse_longueur_f1~plante_genotype+Monoculture_Couple*Condition_hydrique,data=data_final)
plot(lm_speed_length_l1_q8_h0)
summary(lm_speed_length_l1_q8_h0)
plot(lm_speed_length_l1_q8_h1)
summary(lm_speed_length_l1_q8_h1)
anova(lm_speed_length_l1_q8_h0,lm_speed_length_l1_q8_h1)

##Question 9
lm_speed_length_l1_q9_h0<-lm(vitesse_longueur_f1~plante_genotype+Condition_hydrique+Monoculture_Couple,data=data_final)
lm_speed_length_l1_q9_h1<-lm(vitesse_longueur_f1~plante_genotype*Monoculture_Couple*Condition_hydrique,data=data_final)
plot(lm_speed_length_l1_q9_h0)
summary(lm_speed_length_l1_q9_h0)
plot(lm_speed_length_l1_q9_h1)
summary(lm_speed_length_l1_q9_h1)
anova(lm_speed_length_l1_q9_h0,lm_speed_length_l1_q9_h1)

#Longueur feuille 2
##Quelques graphs
barplot_length_l2_Mono_couple<-ggplot(data=data_final,aes(x=Monoculture_Couple,y=Longueur_feuille_2,fill=Monoculture_Couple))+
  geom_boxplot(show.legend=FALSE)
barplot_length_l2_Mono_couple
ggplot(data=data_final,aes(y=Longueur_feuille_2))+
  geom_boxplot()






##Question 1
lm_length_l2_q1_h0<-lm(Longueur_feuille_2~1,data=data_final)
lm_length_l2_q1_h1<-lm(Longueur_feuille_2~plante_genotype,data=data_final)
plot(lm_length_l2_q1_h0)
summary(lm_length_l2_q1_h0)
plot(lm_length_l2_q1_h1)
summary(lm_length_l2_q1_h1)
anova(lm_length_l2_q1_h0,lm_length_l2_q1_h1)

## Question 2
lm_length_l2_q2_h0<-lm(Longueur_feuille_2~as.character(plante_genotype),data=data_final)
lm_length_l2_q2_h1<-lm(Longueur_feuille_2~as.character(plante_genotype)+Bloc,data=data_final)
plot(lm_length_l2_q2_h0)
summary(lm_length_l2_q2_h0)
plot(lm_length_l2_q2_h1)
summary(lm_length_l2_q2_h1)
anova(lm_length_l2_q2_h1)
anova(lm_length_l2_q2_h0,lm_length_l2_q2_h1)

##Question 3
lm_length_l2_q3_h0<-lm(Longueur_feuille_2~plante_genotype+Position,data=data_final)
lm_length_l2_q3_h1<-lm(Longueur_feuille_2~plante_genotype*Position,data=data_final)
plot(lm_length_l2_q3_h0)
summary(lm_length_l2_q3_h0)
plot(lm_length_l2_q3_h1)
summary(lm_length_l2_q3_h1)
anova(lm_length_l2_q3_h0,lm_length_l2_q3_h1)

##Question 4 
lm_length_l2_q4_h0<-lm(Longueur_feuille_2~plante_genotype,data=data_final)
lm_length_l2_q4_h1<-lm(Longueur_feuille_2~plante_genotype+Monoculture_Couple,data=data_final)
plot(lm_length_l2_q4_h0)
summary(lm_length_l2_q4_h0)
plot(lm_length_l2_q4_h1)
summary(lm_length_l2_q4_h1)
anova(lm_length_l2_q4_h0,lm_length_l2_q4_h1)

##Question 5
lm_length_l2_q5_h0<-lm(Longueur_feuille_2~plante_genotype+Monoculture_Couple,data=data_final)
lm_length_l2_q5_h1<-lm(Longueur_feuille_2~plante_genotype*Monoculture_Couple,data=data_final)
plot(lm_length_l2_q5_h0)
summary(lm_length_l2_q5_h0)
plot(lm_length_l2_q5_h1)
summary(lm_length_l2_q5_h1)
anova(lm_length_l2_q5_h0,lm_length_l2_q5_h1)

##Question 6
lm_length_l2_q6_h0<-lm(Longueur_feuille_2~plante_genotype,data=data_final)
lm_length_l2_q6_h1<-lm(Longueur_feuille_2~plante_genotype+Condition_hydrique,data=data_final)
plot(lm_length_l2_q6_h0)
summary(lm_length_l2_q6_h0)
plot(lm_length_l2_q6_h1)
summary(lm_length_l2_q6_h1)
anova(lm_length_l2_q6_h1)
anova(lm_length_l2_q6_h1,lm_length_l2_q6_h0)

##Question 7
lm_length_l2_q7_h0<-lm(Longueur_feuille_2~plante_genotype+Condition_hydrique+poids_graine,data=data_final)
lm_length_l2_q7_h1<-lm(Longueur_feuille_2~poids_graine+plante_genotype*Condition_hydrique,data=data_final)
plot(lm_length_l2_q7_h0)
summary(lm_length_l2_q7_h0)
plot(lm_length_l2_q7_h1)
summary(lm_length_l2_q7_h1)
anova(lm_length_l2_q7_h1)
anova(lm_length_l2_q7_h0,lm_length_l2_q7_h1)

##Question 8
lm_length_l2_q8_h0<-lm(Longueur_feuille_2~plante_genotype+Condition_hydrique+Monoculture_Couple,data=data_final)
lm_length_l2_q8_h1<-lm(Longueur_feuille_2~plante_genotype+Monoculture_Couple*Condition_hydrique,data=data_final)
plot(lm_length_l2_q8_h0)
summary(lm_length_l2_q8_h0)
plot(lm_length_l2_q8_h1)
summary(lm_length_l2_q8_h1)
anova(lm_length_l2_q8_h0,lm_length_l2_q8_h1)

##Question 9
lm_length_l2_q9_h0<-lm(Longueur_feuille_2~plante_genotype+Condition_hydrique+Monoculture_Couple,data=data_final)
lm_length_l2_q9_h1<-lm(Longueur_feuille_2~plante_genotype*Monoculture_Couple*Condition_hydrique,data=data_final)
plot(lm_length_l2_q9_h0)
summary(lm_length_l2_q9_h0)
plot(lm_length_l2_q9_h1)
summary(lm_length_l2_q9_h1)
anova(lm_length_l2_q9_h0,lm_length_l2_q9_h1)

#Date
##Quelques graphs
barplot_length_l2_Mono_couple<-ggplot(data=data_dates,aes(x=Monoculture_Couple,y=date_ligule_2,fill=Monoculture_Couple))+
  geom_boxplot(show.legend=FALSE)
barplot_length_l2_Mono_couple
ggplot(data=data_dates,aes(y=date_ligule_2))+
  geom_boxplot()






##Question 1
data_dates$poids_graine<-as.numeric(data_dates$poids_graine)
data_dates$plante_genotype<-as.factor(data_dates$plante_genotype)
data_dates$Bloc<-as.factor(data_dates$Bloc)
data_dates$Colonne<-as.factor(data_dates$Colonne)
data_dates$Ligne<-as.factor(data_dates$Ligne)
data_dates$Condition_hydrique<-as.factor(data_dates$Condition_hydrique)
data_dates$Monoculture_Couple<-as.factor(data_dates$Monoculture_Couple)

lm_date_q1_h0<-lm(date_ligule_2~poids_graine,data=data_dates)
lm_date_q1_h1<-lm(Biomasse_seche~Bloc+Bloc:Ligne+Bloc:Colonne+Condition_hydrique+
                    poids_graine + plante_genotype
                  + plante_genotype:Condition_hydrique
                  +Monoculture_Couple
                  +plante_genotype:Monoculture_Couple
                  +plante_genotype:Monoculture_Couple:Condition_hydrique,data=data_final)

lm_date_q1_h12<-lm(Biomasse_seche~Bloc+Bloc:Ligne+Bloc:Colonne+Condition_hydrique+
                    poids_graine ,data=data_final)
plot(lm_date_q1_h0)
summary(lm_date_q1_h0)
anova(lm_date_q1_h0)
plot(lm_date_q1_h1)
summary(lm_date_q1_h1)
anova(lm_date_q1_h12)
anova(lm_date_q1_h0,lm_date_q1_h1)

boxplot(data_dates$date_ligule_2~data_dates$Monoculture_Couple:data_dates$plante_genotype)

## Question 2
lm_date_q2_h0<-lm(date_ligule_2~as.character(plante_genotype),data=data_dates)
lm_date_q2_h1<-lm(date_ligule_2~as.character(plante_genotype)+Bloc,data=data_dates)
plot(lm_date_q2_h0)
summary(lm_date_q2_h0)
plot(lm_date_q2_h1)
summary(lm_date_q2_h1)
anova(lm_date_q2_h1)
anova(lm_date_q2_h0,lm_date_q2_h1)

##Question 3
lm_date_q3_h0<-lm(date_ligule_2~plante_genotype+Position,data=data_dates)
lm_date_q3_h1<-lm(date_ligule_2~plante_genotype*Position,data=data_dates)
plot(lm_date_q3_h0)
summary(lm_date_q3_h0)
plot(lm_date_q3_h1)
summary(lm_date_q3_h1)
anova(lm_date_q3_h0,lm_date_q3_h1)

##Question 4 
lm_date_q4_h0<-lm(date_ligule_2~plante_genotype,data=data_dates)
lm_date_q4_h1<-lm(date_ligule_2~plante_genotype+Monoculture_Couple,data=data_dates)
plot(lm_date_q4_h0)
summary(lm_date_q4_h0)
plot(lm_date_q4_h1)
summary(lm_date_q4_h1)
anova(lm_date_q4_h0,lm_date_q4_h1)

##Question 5
lm_date_q5_h0<-lm(date_ligule_2~plante_genotype+Monoculture_Couple,data=data_dates)
lm_date_q5_h1<-lm(date_ligule_2~plante_genotype*Monoculture_Couple,data=data_dates)
plot(lm_date_q5_h0)
summary(lm_date_q5_h0)
plot(lm_date_q5_h1)
summary(lm_date_q5_h1)
anova(lm_date_q5_h0,lm_date_q5_h1)

##Question 6
lm_date_q6_h0<-lm(date_ligule_2~plante_genotype,data=data_dates)
lm_date_q6_h1<-lm(date_ligule_2~plante_genotype+Condition_hydrique,data=data_dates)
plot(lm_date_q6_h0)
summary(lm_date_q6_h0)
plot(lm_date_q6_h1)
summary(lm_date_q6_h1)
anova(lm_date_q6_h1)
anova(lm_date_q6_h1,lm_date_q6_h0)

##Question 7
lm_date_q7_h0<-lm(date_ligule_2~plante_genotype+Condition_hydrique+poids_graine,data=data_dates)
lm_date_q7_h1<-lm(date_ligule_2~poids_graine+plante_genotype*Condition_hydrique,data=data_dates)
plot(lm_date_q7_h0)
summary(lm_date_q7_h0)
plot(lm_date_q7_h1)
summary(lm_date_q7_h1)
anova(lm_date_q7_h1)
anova(lm_date_q7_h0,lm_date_q7_h1)

##Question 8
lm_date_q8_h0<-lm(date_ligule_2~plante_genotype+Condition_hydrique+Monoculture_Couple,data=data_dates)
lm_date_q8_h1<-lm(date_ligule_2~plante_genotype+Monoculture_Couple*Condition_hydrique,data=data_dates)
plot(lm_date_q8_h0)
summary(lm_date_q8_h0)
plot(lm_date_q8_h1)
summary(lm_date_q8_h1)
anova(lm_date_q8_h0,lm_date_q8_h1)

##Question 9
lm_date_q9_h0<-lm(date_ligule_2~plante_genotype+Condition_hydrique+Monoculture_Couple,data=data_dates)
lm_date_q9_h1<-lm(date_ligule_2~plante_genotype*Monoculture_Couple*Condition_hydrique,data=data_dates)
plot(lm_date_q9_h0)
summary(lm_date_q9_h0)
plot(lm_date_q9_h1)
summary(lm_date_q9_h1)
anova(lm_date_q9_h0,lm_date_q9_h1)
