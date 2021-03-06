library(tidyverse)
library(xlsx)
set.seed(66)
liste<-read.csv("C:/Users/Nicolas/Documents/GitHub/Collaborative-Transcriptomics/liste_g�notypes.csv")
liste<-as.tibble(liste[,2])
colnames(liste)<-c("Geno")
#data<-read.csv("C:\\Users\\salas\\Desktop\\supagro\\JRL-AT\\R projet neighbors\\SAS_BLUE.P.csv") %>% 
  #select(-c(X)) %>% 
  #mutate(BLUE_MOY=(BLUE_RC+BLUE_RS)/2) %>% 
#mutate(logBLUE_MOY=(BLUE_RC_log+BLUE_RS_log/2)) %>% 
#mutate(BLUE_MOY=log(BLUE_MOY)) %>% 
#mutate(logBLUE_MOY_norm=logBLUE_MOY/mean(logBLUE_MOY)) %>% 
#arrange(logBLUE_MOY_norm) %>% 
#mutate(x=c(178:1))
#mediane = median(data$logBLUE_MOY_norm)-min(data$logBLUE_MOY_norm)
#ggplot(data,aes(x=x,y=logBLUE_MOY_norm-min(logBLUE_MOY_norm)))+
#geom_col(color="red")+
#theme_bw()+
#geom_line(aes(x=x,y=mediane),show.legend = TRUE)+
#geom_text(inherit.aes = FALSE, label=mediane, aes(x=160,y=mediane+0.002),size=3)+
#geom_line(aes(x=x,y=mediane+0.01))+
#geom_text(inherit.aes = FALSE, label=mediane+0.01, aes(x=160,y=mediane+0.012),size=3)+
#geom_line(aes(x=x,y=mediane-0.01))+
#geom_text(inherit.aes = FALSE, label=mediane-0.01, aes(x=160,y=mediane-0.008),size=3)
#datasup<-data %>% filter(logBLUE_MOY_norm-min(logBLUE_MOY_norm)>mediane)
#datainf<-data %>% filter(logBLUE_MOY_norm-min(logBLUE_MOY_norm)<mediane)

load("C:/Users/salas/Desktop/SG.RData")
SG_numeric_matrix <- matrix(as.numeric(SG), ncol =  ncol(SG)) 
GWAS <- read_csv("C:\\Users\\salas\\Desktop\\liste_Gwas.csv",col_names = FALSE)
SG_tibble<-as_tibble(SG,rownames="rowname")
SG_GWAS<-SG_tibble %>% select(rowname,"AX-89431564")

selection_GWAS<-function(SG_GWAS,GWAS){
  G<-matrix(ncol=2)
  colnames(G)<-c("rowname","AX-89431564")
  for (i in (1:nrow(SG_GWAS))){
    G1<-matrix(ncol=2)
    colnames(G1)<-c("rowname","AX-89431564")
    for (j in (1:nrow(GWAS))){
      if (SG_GWAS[i,1]==GWAS[j,1]){
        G1<-SG_GWAS[i,]
      }
    }
    G<-rbind(G,G1)
  }
  return(G)
}
Selection<-as_tibble(selection_GWAS(SG_GWAS,GWAS))
Selection0<-Selection %>% filter(is.na(rowname)==FALSE & `AX-89431564`==0)
Selection2<-Selection %>% filter(is.na(rowname)==FALSE & `AX-89431564`==2)
Selection74<-Selection %>% select(rowname) %>% filter(rowname=="EL4X_74")
colnames(Selection74)<-c("Geno")

aleatoire <- function(df,n){
  l<-as_tibble(c())
  a<-sample(x=1:nrow(df),size=n)
  l<-slice(df,a)
  l<-l[,1]
  colnames(l)<-c("Geno")
  return(l)
}
alea_rht_moins<-aleatoire(Selection0,7)
alea_rht_plus<-aleatoire(Selection2,7)
alea_rht<-alea_rht_moins %>% full_join(.,alea_rht_plus) %>% full_join(.,Selection74)
create_matrice<- function(df){
  v1<-select(df,Geno) %>% 
    mutate(Geno=as.character(Geno))
  m<-matrix(ncol=nrow(df),nrow=nrow(v1))
  for (i in 1:nrow(v1)){
    for (j in i:nrow(v1)){
      m[i,j]<-paste(v1[i,1],v1[j,1],sep=",")
      }
    }
  return(m)
}
matriceenti�re<-create_matrice(liste)
choix <- function(M,n){
  l<-list()
  for (i in 1:nrow(M)){
    l[length(l)+1]<-M[i,i]
  }
  if ((nrow(M)/(n+1))%%1==0){
    for (a in 0:((nrow(M)/(n+1))-1)){
      for (colonne in (2+((1+n)*a)):((1+n)*(a+1))){
        for (ligne in (1+((1+n)*a)):(((1+n)*(a+1))-1)){
          if (colonne-ligne>=1){
            l[length(l)+1]<-M[ligne,colonne]
            }
          }
        }
    }
    return (l)
  }
  return ("NOPE")
}
liste <- choix(matriceenti�re,4)
courbes<-function(n){
  l<-c()
  for (i in 1:50){
    l[length(l)+1]<-i*(n+2)/2
  } 
  l<-as.tibble(l)
  return (l)
}
c1<-courbes(1)
c2<-courbes(2)
c3<-courbes(3)
c4<-courbes(4)
c5<-courbes(5)
gg
ggplot(c2,aes(x=1:50,y=value))+
  geom_line(aes(y=value))+
  geom_line(aes(y=c3$value))+
  geom_line(aes(y=c4$value,color="red"))+
  geom_line(aes(y=c5$value))+
  geom_line(aes(y=c1$value))
nb_pots<-function(n,x){
  z<-3*(n+2)*x
  return(z)
}
nb_pots(4,15)
Wt_add <- function(l){
  l2<-l
  for (i in (1:length(l))){
    l[i]<-paste(l[i],"WT")
    l2[i]<-paste(l2[i],"wt")
  }
  l3<-append(l,l2)
  return(l3)
}
Wtliste <- Wt_add(liste)
Bloc_add <- function(l){
  l1<-l
  l2<-l
  for (i in (1:length(l))){
    l1[i]<-paste(l[i],"b1")
    l2[i]<-paste(l[i],"b2")
    l[i]<-paste(l[i],"b3")
  }
  l1<-append(l1,l2)
  l1<-append(l1,l)
  return(l1)
}
Wtbliste<-Bloc_add(Wtliste)
crit�res_feuille<-function(){
  l<-list()
  for (i in 1:21){
    l[1+length(l)]<-paste("Longueur col�optile",as.character((i)))
    for (j in 1:3){
      if (j==1){
        l[1+length(l)]<-paste("Longueur 1�re feuille",as.character((i)))
        l[1+length(l)]<-paste("Largueur 1�re feuille",as.character((i)))
        l[1+length(l)]<-paste("Surface foliaire 1�re feuille calcul�e",as.character((i)))
      }
      if (j==2){
        l[1+length(l)]<-paste("Longueur 2�me feuille",as.character((i)))
        l[1+length(l)]<-paste("Largueur 2�me feuille",as.character((i)))
        l[1+length(l)]<-paste("Surface foliaire 2�me feuille calcul�e",as.character((i)))
      }
      if (j==3){
        l[1+length(l)]<-paste("Longueur 3�me feuille",as.character((i)))
        l[1+length(l)]<-paste("Largueur 3�me feuille",as.character((i)))
        l[1+length(l)]<-paste("Surface foliaire 3�me feuille calcul�e",as.character((i)))
      }
    }
  }
  return(l)
}
crit�res_autres<-function(){
  l<-list("Biomasse a�rienne","Surface foliaire")
  return(l)
}

cr�ation_matrice_dim3<-function(){
  M<-array(dim=c(270,7,21))
  M<-provideDimnames(M , sep = "_", base = list('couple',"crit�re",'jour'))
  rownames(M)<-Wtbliste
  colnames(M)<-c("Longueur col�optile","Longueur 1�re feuille","Largueur 1�re feuille","Longueur 2�me feuille","Largueur 2�me feuille","Longueur 3�me feuille","Largueur 3�me feuille")
  return(M)
}


M<-cr�ation_matrice_dim3()
colnames(M)
view(M)
cr�ation_excel <- function(M){
  l<-dimnames(M)[[3]]
  for (i in 1:21){
    write.xlsx2(M[,,i],file="xlsxtest4.xlsx",append=TRUE,sheetName = l[i])
  }
}
cr�ation_excel(M)

?write.xlsx2
?as.matrix
?strsplit
?contains
?label_value
?mutate
?geom_text
?filter
?geom_line
?rownames
?list
?across
?slice
?select
?is.null
?write_csv
?provideDimnames
glimpse(data)
write.csv(binded,"liste_g�notypes.csv")

liste_genotypes<-read.csv("\\Users\\salas\\Documents\\GitHub\\Collaborative-Transcriptomics\\liste_g�notypes.csv")
merge<-SG_tibble %>% select(rowname,"AX-89431564")
colnames(merge)<-c("Geno","AX-89431564")
binded<-merge(merge,liste_genotypes,by="Geno")
for (elt in (1:nrow(binded))){
  if (binded[elt,2]==2){
    binded[elt,2]="RHT"
  }
  else{
    binded[elt,2]="rht"
  }
}
binded<-binded %>% select(c(1,2))


getwd()

