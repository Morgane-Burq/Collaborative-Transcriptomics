library(sommer)
data_final_pair = data_final[c(1:270)*2,]
data_final_impair = data_final[c((1:270)*2 -1),]
data_final_pair$voisin = data_final_impair$plante_genotype
data_final_impair$voisin = data_final_pair$plante_genotype
data_final = as.data.frame(rbind(data_final_pair,data_final_impair))


data_final$plante_genotype = as.factor(data_final$plante_genotype)
Model1 = mmer(Biomasse_seche ~ Bloc + poids_graine,
              random = ~ plante_genotype,
              data = data_final)
Model2 = mmer(Biomasse_seche ~ Bloc + poids_graine,
             random = ~ plante_genotype + voisin:plante_genotype,
             data = data_final)


anova.mmer(Model1,Model2)

## Pas d'effet du type de mélange_mono



