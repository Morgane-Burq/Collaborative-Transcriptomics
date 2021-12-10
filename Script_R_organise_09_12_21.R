library(tidyverse)
library(lme4)
library(Rcpp)
library(lmerTest)
library(car)
library(ggplot2)
library(cowplot)

# Lecture des fichiers
```{r}
liste_rht<-read.csv("liste_gÈnotypes.csv")
data_final<-as_data_frame(read.csv("Mesures_neighbors_final.tsv",header=TRUE,sep="\t"))
```
# Ajout de la colonne voisin
```{r}
data_final_pair = data_final[c(1:270)*2,]
data_final_impair = data_final[c((1:270)*2 -1),]
data_final_pair$voisin = data_final_impair$plante_genotype
data_final_impair$voisin = data_final_pair$plante_genotype
data_final = as.data.frame(rbind(data_final_pair,data_final_impair))
```
#Suppression de la colonne Surface feuille 2 (pas rempli).
```{r}
data_final<-data_final[-c(16)] 
```
#RÈecriture des lignes et colonnes.
```{r}
data_final$Ligne = as.numeric(data_final$Ligne)
data_final$Colonne = as.numeric(data_final$Colonne)
data_final[data_final$Bloc %in% c(2,4,6),]$Ligne = data_final[data_final$Bloc %in% c(2,4,6),]$Ligne + 9
data_final[data_final$Bloc %in% c(5,6),]$Colonne = data_final[data_final$Bloc %in% c(5,6),]$Colonne + 10
data_final[data_final$Bloc %in% c(3,4),]$Colonne = data_final[data_final$Bloc %in% c(3,4),]$Colonne + 5
```
#Ajout de la colonne RHT_rht
```{r}
for (elt in (1:nrow(data_final))){
  if (data_final$plante_genotype[elt]%in%c("114","235","329","376","395","482","89")){
    data_final$RHT_rht[elt]="rht"
  }
  else{
    data_final$RHT_rht[elt]="RHT"
  }
}
```
# CrÈation des listes isoler les couples/pots o˘ une plante ou deux n'a pas pousser
```{r}
liste2<-which(is.na(data_final$Longueur_feuille_2))
liste3<-which(data_final$Couple%in%data_final$Couple[liste2])
liste_vitesse_coleoptile_feuille_2<-append(liste3,which(data_final$Couple == data_final$Couple[which(data_final$vitesse_coleoptile_feuille_2<0)]))
```
# Retrait des Couples/pots o˘ une plante ou deux n'a pas pousser
```{r}
data_final<-data_final[-liste3,]
```
# Ajustement des types de toutes les donnÈes
```{r}
data_final<-data_final %>% 
  mutate(plante_genotype=as.factor(plante_genotype),
         RHT_rht=as.factor(RHT_rht),
         Nombre_de_feuille_ligul√.e=as.numeric(Nombre_de_feuille_ligul√.e),
         Longueur_feuille_2=as.numeric(Longueur_feuille_2),
         vitesse_longueur_f1=as.numeric(vitesse_longueur_f1),
         vitesse_largeur_f1=as.numeric(vitesse_largeur_f1),
         poids_graine=as.numeric(poids_graine),
         vitesse_coleoptile_feuille_1=as.numeric(vitesse_coleoptile_feuille_1),
         vitesse_coleoptile_feuille_2=as.numeric(vitesse_coleoptile_feuille_2),
         Monoculture_Couple=as.factor(Monoculture_Couple),
         Condition_hydrique=as.factor(Condition_hydrique),
         RHT_rht=as.factor(RHT_rht),
         Bloc=as.factor(Bloc),
         Colonne=as.factor(Colonne),
         Ligne=as.factor(Ligne),
         plante_genotype=as.factor(plante_genotype),
         Nombre_de_feuille_ligul√.e=as.numeric(Nombre_de_feuille_ligul√.e),
         Couple=as.factor(Couple),
         Position=as.factor(Position),
         Position_pots=as.factor(Position_pots)) 
```
# Quelques graphiques gÈnÈraux
```{r}
names(data_final)
for (i in c(11:20)){
  print(hist(data_final[,i],main=names(data_final)[i]))
}
```
## Biomasse seche
```{r}
shapiro.test(data_final$Biomasse_seche)
bartlett.test(Biomasse_seche~plante_genotype,data=data_final)
```
#Statistiques.
```{r}
mod = lm(Biomasse_seche ~ Bloc + poids_graine , data = data_final)
mod1 = lm(Biomasse_seche ~ Bloc + poids_graine +plante_genotype:voisin, data = data_final)
summary(mod1)
anova(mod,mod1)
write_csv(data_final,"Mesure_Neighbors_Arrange.csv")
```








#En plus pour data_dates et autres
```{r}
data_dates<-read.csv("Mesures_neighbors_dates.csv",header=TRUE,sep=",")
liste<-which(!is.na(data_dates$date_ligule_1)&(is.na(data_dates$date_ligule_2)))
liste_vitesse_coleoptile_feuille_2<-append(liste3,which(data_final$Couple == data_final$Couple[which(data_final$vitesse_coleoptile_feuille_2<0)]))
data_dates$date_ligule_2=as.numeric(substr(data_dates$date_ligule_2,1,2))
data_dates<-data_dates %>% mutate(RHT_rht=data_final$RHT_rht)
data_dates<-data_dates[-liste3,] %>% 
  mutate(poids_graine=as.numeric(poids_graine),
         plante_genotype=as.character(plante_genotype),
         Bloc=as.numeric(Bloc),
         Colonne=as.numeric(Colonne),
         Ligne=as.numeric(Ligne),
         Condition_hydrique=as.character(Condition_hydrique),
         Monoculture_Couple=as.character(Monoculture_Couple),
         date_ligule_2=as.numeric(substr(date_ligule_2,1,2))) %>% 
  filter(!is.na(date_ligule_2))
```
```{r}
data_vitesse_coleoptile_feuille_2<-data_final %>% 
  filter(vitesse_coleoptile_feuille_2>0)
```
```{r}
data_vitesse_coleoptile_feuille_1<-data_final %>% 
  filter(vitesse_coleoptile_feuille_1>0)
```
```{r}
data_vitesse_longueur_f1<- data_final %>% 
  filter(vitesse_longueur_f1>20)
```
```{r}
data_vitesse_largeur_f1<-data_final %>% 
  filter(vitesse_largeur_f1>0.5)
```

?write_csv
?as.numeric
?as.numeric
?as.numeric
