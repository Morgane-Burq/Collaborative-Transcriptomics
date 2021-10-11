library(tidyverse)
data<-read.csv("C:\\Users\\salas\\Desktop\\supagro\\JRL-AT\\R projet neighbors\\SAS_BLUE.P.csv") %>% 
  select(-c(X)) %>% 
  mutate(BLUE_MOY=(BLUE_RC+BLUE_RS)/2) %>% 
  mutate(logBLUE_MOY=(BLUE_RC_log+BLUE_RS_log/2)) %>% 
  mutate(BLUE_MOY=log(BLUE_MOY)) %>% 
  mutate(logBLUE_MOY_norm=logBLUE_MOY/mean(logBLUE_MOY)) %>% 
  arrange(logBLUE_MOY_norm) %>% 
  mutate(x=c(178:1))
mediane = median(data$logBLUE_MOY_norm)-min(data$logBLUE_MOY_norm)
ggplot(data,aes(x=x,y=logBLUE_MOY_norm-min(logBLUE_MOY_norm)))+
  geom_col(color="red")+
  theme_bw()+
  geom_line(aes(x=x,y=mediane),show.legend = TRUE)+
  geom_text(inherit.aes = FALSE, label=mediane, aes(x=160,y=mediane+0.002),size=3)+
  geom_line(aes(x=x,y=mediane+0.01))+
  geom_text(inherit.aes = FALSE, label=mediane+0.01, aes(x=160,y=mediane+0.012),size=3)+
  geom_line(aes(x=x,y=mediane-0.01))+
  geom_text(inherit.aes = FALSE, label=mediane-0.01, aes(x=160,y=mediane-0.008),size=3)
datasup<-data %>% filter(logBLUE_MOY_norm-min(logBLUE_MOY_norm)>mediane)
datainf<-data %>% filter(logBLUE_MOY_norm-min(logBLUE_MOY_norm)<mediane)

aleatoire <- function(df,n){
  l<-as_tibble(c())
  a<-sample(x=1:nrow(df),size=n)
  l<-slice(df,a)
  return(l)
}
selectioninf<-aleatoire(datainf,10)
selectionsup<-aleatoire(datasup,10)
selection<-full_join(selectionsup,selectioninf)
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
matriceentière<-create_matrice(selection)
choix <- function(M){
  l<-list()
  for (i in 1:nrow(M)){
    l[length(l)+1]<-M[i,i]
  }
  for (a in 0:4){
    for (colonne in (2+4*a):(4*(a+1))){
      for (ligne in (1+4*a):(4*(a+1)-1)){
        if (colonne-ligne>=1){
          l[length(l)+1]<-M[ligne,colonne]
        }
      }
    }
  }
  return (l)
}
liste <- choix(matriceentière)
courbes<-function(n){
  l<-c()
  for (i in 1:50){
    l[length(l)+1]<-i*(n+2)/2
  } 
  l<-as.tibble(l)
  return (l)
}
c2<-courbes(2)
c3<-courbes(3)
c4<-courbes(4)
c5<-courbes(5)
ggplot(c2,aes(x=1:50,y=value))+
  geom_line(aes(y=value))+
  geom_line(aes(y=c3$value))+
  geom_line(aes(y=c4$value))+
  geom_line(aes(y=c5$value))
nb_pots<-function(n,x){
  z<-3*(n+2)*x
  return(z)
}
nb_pots(3,25)
nb_pots(4,20)
?strsplit
?contains
?label_value
?mutate
?geom_text
?filter
?geom_line
?rownames
?across
?slice
?select
?is.null
glimpse(data)
