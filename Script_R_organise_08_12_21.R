library(tidyverse)
library(lme4)
library(Rcpp)
library(lmerTest)
library(car)
library(ggplot2)
library(cowplot)
library(rmarkdown)
# Lecture des fichiers

liste_rht<-read.csv("liste_génotypes.csv")
liste_rht<-liste_rht %>% 
  mutate(Geno_n=substr(Geno,6,9))
data_final<-as_data_frame(read.csv("Mesures_neighbors_final.tsv",header=TRUE,sep="\t"))

# Ajout de la colonne voisin

data_final_pair = data_final[c(1:270)*2,]
data_final_impair = data_final[c((1:270)*2 -1),]
data_final_pair$voisin = data_final_impair$plante_genotype
data_final_impair$voisin = data_final_pair$plante_genotype
data_final = as.data.frame(rbind(data_final_pair,data_final_impair))

#Suppression de la colonne Surface feuille 2 (pas rempli).

data_final<-data_final[-c(16)] 

#Réecriture des lignes et colonnes.

data_final$Ligne = as.numeric(data_final$Ligne)
data_final$Colonne = as.numeric(data_final$Colonne)
data_final[data_final$Bloc %in% c(2,4,6),]$Ligne = data_final[data_final$Bloc %in% c(2,4,6),]$Ligne + 9
data_final[data_final$Bloc %in% c(5,6),]$Colonne = data_final[data_final$Bloc %in% c(5,6),]$Colonne + 10
data_final[data_final$Bloc %in% c(3,4),]$Colonne = data_final[data_final$Bloc %in% c(3,4),]$Colonne + 5

  
#Ajout de la colonne RHT_rht

for (elt in (1:nrow(data_final))){
  if (data_final$plante_genotype[elt]%in%c("114","235","329","376","395","482","89")){
    data_final$RHT_rht[elt]="rht"
  }
  else{
    data_final$RHT_rht[elt]="RHT"
  }
}

#Ajout de la colonne RHT_voisin

data_final<-data_final %>% 
  mutate(RHT_voisin = liste_rht$AX.89431564[voisin])


# Création des listes isoler les couples/pots où une plante ou deux n'a pas pousser

liste2<-which(is.na(data_final$Longueur_feuille_2))
liste3<-which(data_final$Couple%in%data_final$Couple[liste2])

# Retrait des Couples/pots où une plante ou deux n'a pas pousser

data_final<-data_final[-liste3,]

# Création des listes isoler les couples/pots où on a pas de biomasse_seche ou deux n'a pas pousser

liste4<-which(is.na(data_final$Biomasse_seche))
liste5<-which(data_final$Couple%in%data_final$Couple[liste4])

# Retrait des Couples/pots où une plante ou deux n'a pas de biomasse_seche

data_final<-data_final[-liste5,]

# Création des listes isoler les couples/pots où on a une biomasse_seche <20

liste6<-which(data_final$Biomasse_seche<24)
liste7<-which(data_final$Couple%in%data_final$Couple[liste6])

# Retrait des Couples/pots où on a une biomasse_seche <20

data_final<-data_final[-liste7,]

# Ajustement des types de toutes les données restantes

data_final<-data_final %>% 
  mutate(plante_genotype=as.factor(plante_genotype),
         RHT_rht=as.factor(RHT_rht),
         Nombre_de_feuille_ligulÃ.e=as.numeric(Nombre_de_feuille_ligulÃ.e),
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
         Nombre_de_feuille_ligulÃ.e=as.numeric(Nombre_de_feuille_ligulÃ.e),
         Couple=as.factor(Couple),
         Position=as.factor(Position),
         Position_pots=as.factor(Position_pots),
         voisin=as.factor(voisin)) 

# Quelques graphiques généraux

ggplot(data=data_final,aes(x=Couple:Position_pots,y=Biomasse_seche))+
  geom_point()

names(data_final)
for (i in c(11:20)){
  print(hist(data_final[,i],main=names(data_final)[i]))
}

##Boxplot Biomasse_seche pour chaque condition hydrique
ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique),data=data_final)+
  geom_boxplot()+
  ggtitle("Biomasse sèche en fonction de la Condition hydrique")+
  theme_cowplot()
  

##Boxplot Biomasse_seche pour chaque condition de culture (Monoculture/Couple)

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple),data=data_final)+
  geom_boxplot()+
  ggtitle("Biomasse sèche en fonction de la Condition de culture")+
  theme_cowplot()

##Boxplot Biomasse_seche pour RHT + et rht - 

ggplot(aes(x=RHT_rht,y=Biomasse_seche,fill=RHT_rht),data=data_final)+
  geom_boxplot()+
  ggtitle("Biomasse sèche en fonction du génotype RHT / rht")+
  theme_cowplot()

##Boxplot Biomasse_seche pour chaque genotype

ggplot(aes(x=plante_genotype,y=Biomasse_seche,fill=plante_genotype),data=data_final)+
  geom_boxplot()+
  ggtitle("Biomasse sèche en fonction du génotype")+
  theme_cowplot()

#Boxplot Biomasse_seche pour chaque genotype focal en fonction de son voisin

###74

data_74<-data_final %>% 
  filter(plante_genotype=="74")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_74)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 74 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_74)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 74 en fonction des Conditions de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_74)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 74 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_74)+
  geom_boxplot()+
  theme_cowplot()

###265

data_265<-data_final %>% 
  filter(plante_genotype=="265")

ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_265)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 265 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_265)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 265 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_265)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 265 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_265)+
  geom_boxplot()+
  theme_cowplot()

###118

data_118<-data_final %>% 
  filter(plante_genotype=="118")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_118)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 118 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_118)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 118 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_118)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 118 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_118)+
  geom_boxplot()+
  theme_cowplot()

###348

data_348<-data_final %>% 
  filter(plante_genotype=="348")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_348)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 348 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_348)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 348 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_348)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 348 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_348)+
  geom_boxplot()+
  theme_cowplot()
###412

data_412<-data_final %>% 
  filter(plante_genotype=="412")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_412)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 412 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_412)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 412 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_412)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 412 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_412)+
  geom_boxplot()+
  theme_cowplot()

###395

data_395<-data_final %>% 
  filter(plante_genotype=="395")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_395)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 395 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_395)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 395 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_395)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 395 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_395)+
  geom_boxplot()+
  theme_cowplot()

###130

data_130<-data_final %>% 
  filter(plante_genotype=="130")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_130)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 130 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_130)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 130 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_130)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 130 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_130)+
  geom_boxplot()+
  theme_cowplot()
###131

data_131<-data_final %>% 
  filter(plante_genotype=="131")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_131)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 131 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_131)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 131 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_131)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 131 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_131)+
  geom_boxplot()+
  theme_cowplot()

###165

data_165<-data_final %>% 
  filter(plante_genotype=="165")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_165)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 165 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_165)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 165 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_165)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 165 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_165)+
  geom_boxplot()+
  theme_cowplot()

###235

data_235<-data_final %>% 
  filter(plante_genotype=="235")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_235)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 235 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_235)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 235 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_235)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 235 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_235)+
  geom_boxplot()+
  theme_cowplot()

###376

data_376<-data_final %>% 
  filter(plante_genotype=="376")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_376)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 376 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_376)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 376 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_376)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 376 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_376)+
  geom_boxplot()+
  theme_cowplot()

###114

data_114<-data_final %>% 
  filter(plante_genotype=="114")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_114)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 114 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_114)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 114 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_114)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 114 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_114)+
  geom_boxplot()+
  theme_cowplot()

###329

data_329<-data_final %>% 
  filter(plante_genotype=="329")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_329)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 329 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_329)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 329 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_329)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 329 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_329)+
  geom_boxplot()+
  theme_cowplot()
###89

data_89<-data_final %>% 
  filter(plante_genotype=="89")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_89)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 89 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_89)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 89 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_89)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 89 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_89)+
  geom_boxplot()+
  theme_cowplot()

###482

data_482<-data_final %>% 
  filter(plante_genotype=="482")
ggplot(aes(x=voisin,y=Biomasse_seche,fill=voisin)
       ,data=data_482)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 482 en fonction du génotype du voisin")+
  theme_cowplot()

ggplot(aes(x=Monoculture_Couple,y=Biomasse_seche,fill=Monoculture_Couple)
       ,data=data_482)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 482 en fonction des Condition de culture")+
  theme_cowplot()

ggplot(aes(x=Condition_hydrique,y=Biomasse_seche,fill=Condition_hydrique)
       ,data=data_482)+
  geom_boxplot()+
  ggtitle("Biomasse sèche des plantes de génotype 482 en fonction des Conditions hydriques")+
  theme_cowplot()

ggplot(aes(x=RHT_voisin,y=Biomasse_seche,fill=RHT_voisin)
       ,data=data_482)+
  geom_boxplot()+
  theme_cowplot()

## test normalité et homoscedasticité

shapiro.test(data_final$Biomasse_seche)
bartlett.test(Biomasse_seche~plante_genotype,data=data_final)
?bartlett.test

#Statistiques.

mod = lm(Biomasse_seche ~ Bloc+
           poids_graine+
           plante_genotype+
           Condition_hydrique:plante_genotype
         ,data = data_final)
summary(mod)
anova(mod)
mod1 = lm(Biomasse_seche ~ Bloc+
            poids_graine+
            plante_genotype+
            Condition_hydrique:plante_genotype+
            plante_genotype:RHT_voisin
            ,data = data_final)
anova(mod1)
summary(mod1)
anova(mod,mod1)
anova(mod1)


write_csv(data_final,"Mesure_Neighbors_Arrange.csv")









#En plus pour data_dates et autres

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

data_vitesse_coleoptile_feuille_2<-data_final %>% 
  filter(vitesse_coleoptile_feuille_2>0)

data_vitesse_coleoptile_feuille_1<-data_final %>% 
  filter(vitesse_coleoptile_feuille_1>0)

data_vitesse_longueur_f1<- data_final %>% 
  filter(vitesse_longueur_f1>20)

data_vitesse_largeur_f1<-data_final %>% 
  filter(vitesse_largeur_f1>0.5)

liste_vitesse_coleoptile_feuille_2<-append(liste3,which(data_final$Couple == data_final$Couple[which(data_final$vitesse_coleoptile_feuille_2<0)]))




?cor
?corr
?write_csv
?as.numeric
?as.numeric
?as.numeric
