#TP 3 Code
#Par Claude Demers-Bélanger (1534217) et Mikael Perreault (1741869)

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et vérifier que les packages nécessaires sont bien installés
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2)

#setwd('D:/Polytechnique/A2018/SystRec/TP1')
setwd('C:/Users/claudedb/Documents/GitHub/RecSys/TP 3')
# Data Load
u.data<-read.csv('Data/u.data.csv',sep='|')
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')


# Question 1 ---------------------------------------------------------------
# Déterminer une baseline pour l'évaluation de la méthode.
# Prenons une méthode ou nous appliquons la moyenne des lignes et des colonnes pour évaluer les notes

moy.col<-colSums(m)/colSums(m!=0)
moy.row<-rowSums(m)/rowSums(m!=0)

m.baseline<-m
m.baseline[]<-0
m.baseline<-sweep(sweep(m.baseline,1,moy.row/2,"+"),2,moy.col/2,"+")
m.NA<-m
m.NA[m.NA==0]<-NA
RMSE.baseline<-sqrt(mean(as.matrix((m.NA-m.baseline)^2),na.rm=TRUE))


# Question 2 --------------------------------------------------------------
# Appliquer la décomposition SVD

m.norm<-m.NA-moy.row
m.norm[is.na(m.norm)]<-0
m.norm<-Matrix(m.norm, sparse=TRUE)

SVD<-svd(m.norm)

S<-SVD$d
S<-diag(S)
U<-SVD$u
V<-SVD$v



# Question 3 --------------------------------------------------------------

# Effectuez l'estimation des votes sur la base de SVD avec 10 dimensions.

m.moy2<-m
m.moy2[]<-0
m.moy2<-sweep(m.moy2,1,moy.row,'+')
dim.q3<-10

S10<-sqrt(S[1:dim.q3,1:dim.q3])
U10<-U[,1:dim.q3]
V10<-V[,1:dim.q3]

m.pred.q3<-m.moy2+(U10%*%t(S10))%*%S10%*%t(V10)

# Question 4 --------------------------------------------------------------
#Calculez l'erreur absolue moyenne et l'erreur quadratique moyenne.

#Absolue moyenne
ABS.q4<-mean(as.matrix(abs(m.NA-m.pred.q3)),na.rm=TRUE)

#Quadratique Moyenne
RMSE.q4<-sqrt(mean(as.matrix((m.NA-m.pred.q3)^2),na.rm=TRUE))

# Question 5 --------------------------------------------------------------
#Déterminez le nombre de dimensions optimal (sans appliquer de validation croisée).

dim.vector<-seq(from = 10, to = nrow(m), by = 20)
RMSE.q5<-data.frame(dimension=dim.vector,RMSE=rep(0,length(dim.vector)),
                    MAE=rep(0,length(dim.vector)))


for(i in RMSE.q5$dimension)
{
  Si<-sqrt(S[1:i,1:i])
  Ui<-U[,1:i]
  Vi<-V[,1:i]
  
  m.pred.i<-m.moy2+(Ui%*%t(Si))%*%Si%*%t(Vi)
  
  
  RMSE.q5$RMSE[which(RMSE.q5$dimension==i)]<-sqrt(mean(as.matrix((m.NA-m.pred.i)^2),
                                                       na.rm=TRUE))
  RMSE.q5$MAE[which(RMSE.q5$dimension==i)]<-mean(as.matrix(abs(m.NA-m.pred.i)),
                                                 na.rm=TRUE)
  
}
g<-ggplot(RMSE.q5,aes(x=dimension))+
  geom_point(aes(y=RMSE,color='RED'))+
  geom_point(aes(y=MAE,color='BLUE'))+
  labs(title='Evolution du RMSE et MAE en fonction du nombre de dimensions',
       x='Dimensions',y='Erreur',color="Type d'erreur")+
  scale_color_manual(labels=c('RMSE',"MAE"),values=c('RED','BLUE'))

#Sans utilisé de validation croisé, le nombre maximal de dimension 
#est égale au nombre d'utilisateur

# Question 6 --------------------------------------------------------------

#Déterminez le nombre optimal de dimensions, 
#mais en utilisant cette fois une validation croisée.


# Question 7 --------------------------------------------------------------


# Question 8 --------------------------------------------------------------

#Comparez la performance de cette approche avec celle d'une approche collaborative de votre choix
#(avec l'erreur quadratique et erreur absolue moyennes). Utilisez une validation croisée.


