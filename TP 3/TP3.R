#TP 3 Code
#Par Claude Demers-Belanger (1534217) et Mikael Perreault (1741869)

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et verifier que les packages necessaires sont bien installes
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2)
rm(list=ls())
setwd('C:/Users/mikap/OneDrive/Documents/GitHub/RecSys/TP 3')
#setwd('C:/Users/claudedb/Documents/GitHub/RecSys/TP 3')
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

min.nindex <- function(m, n=5) {
  i <- order(m, decreasing=FALSE)
  return(i[1:n])
}
order.partial=function(vec){
  idx<-which(vec<=sort(vec,partial=21)[21])
  idx[order(vec[idx])][2:21]
}
cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }
# Data Load

u.data<-read.csv('Data/u.data.csv',sep='|')
m <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')


# Question 1 ---------------------------------------------------------------
# Determiner une baseline pour l'evaluation de la methode

#Ces seed seront utilisés pour les 10 folds de la cross
#validation pendant tout le TP. 
seed<-c(55,75,95,105,222,60,78,4,6,23)
n_fold<-length(seed)

#Fonction pour la cross-val de la baseline
cv.baseline = function (k,pct,error.type){
  set.seed(k)
  #Segmentation des training et testing set
  j<-m>0
  j.test<-j
  j.test[sample(length(j),pct*length(j))]<-FALSE
  j.train<-!j.test
  m.test<-m
  m.train<-m
  m.train[j.test]<-0
  m.test[!j.test]<-NA

  
  moy.col<-colSums(m.train)/colSums(m.train!=0)
  moy.row<-rowSums(m.train)/rowSums(m.train!=0)
  
  #Calcul de la baseline: on prend la moyenne entre la moyenne
  #de la ligne i et la moyenne de la colonne j pour un élément (i,j)
  m.baseline<-m.train
  m.baseline[]<-0
  m.baseline<-sweep(sweep(m.baseline,1,moy.row/2,"+"),
                    2,moy.col/2,"+")
  RMSE.baseline<-sqrt(mean(as.matrix((m.test-m.baseline)^2),
                           na.rm=TRUE))
  MAE.baseline<-mean(as.matrix(abs(m.test-m.baseline)),
                     na.rm=TRUE)
  
  #Difficulté à retourner les valeurs de RMSE et MAE en 
  #une exécution: on devra exécuter la fonction 2 fois. 
  if(error.type=='RMSE'){
    RMSE.q1<-sqrt(mean(as.matrix((m.test-m.baseline)^2),
                       na.rm=TRUE))
    return(RMSE.q1)
  } else if(error.type=='MAE'){
    MAE.q1<-mean(as.matrix(abs(m.test-m.baseline)),
                 na.rm=TRUE)
    return(MAE.q1)
  } else{
    return(0)
  }
  
}
#On choisit 90% des valeurs en training/10% en testing
RMSE.q1V<-sapply(seed,cv.baseline,pct=0.9,error.type='RMSE')
MAE.q1V<-sapply(seed,cv.baseline,pct=0.9,error.type='MAE')


RMSE.q1<-mean(RMSE.q1V)
MAE.q1<-mean(MAE.q1V)
#Résultat : RMSE = 0.982, MAE = 0.795


# Question 2 --------------------------------------------------------------
# Appliquer la decomposition SVD
m.NA=m
m.NA[m.NA==0]=NA
moy.col<-colSums(m)/colSums(m!=0)
moy.row<-rowSums(m)/rowSums(m!=0)
#On normalise en retirant la moyenne des users 
m.norm<-m.NA-moy.row 

#En affectant les valeurs manquantes à zéro dans la matrice 
#normalisée, ça revient au même que les affecter à la moyenne
#des user dans la matrice m initiale. 
m.norm[is.na(m.norm)]<-0
m.norm<-Matrix(m.norm, sparse=TRUE)
SVD<-svd(m.norm)


#On extrait les matrices de SVD
S<-SVD$d
S<-diag(S)
U<-SVD$u
V<-SVD$v



# Question 3 --------------------------------------------------------------

# Effectuez l'estimation des votes sur la base 
#de SVD avec 10 dimensions.

m.moy2<-m
m.moy2[]<-0
#Moyenne des users
m.moy2<-sweep(m.moy2,1,moy.row,'+')
dim.q3<-10

S10<-sqrt(S[1:dim.q3,1:dim.q3])
U10<-U[,1:dim.q3]
V10<-V[,1:dim.q3]

#La matrice des estimations
m.pred.q3<-m.moy2+(U10%*%t(S10))%*%S10%*%t(V10)
#Correction pour les valeurs out of bound
m.pred.q3[m.pred.q3<0]=0
m.pred.q3[m.pred.q3>5]=5

#La matrice d'estimation est m.pred.q3, roulez le code pour 
#plus de détails. 
# Question 4 --------------------------------------------------------------
#Calculez l'erreur absolue moyenne et l'erreur quadratique moyenne.

#Absolue moyenne
ABS.q4<-mean(as.matrix(abs(m.NA-m.pred.q3)),na.rm=TRUE)

#Quadratique Moyenne
RMSE.q4<-sqrt(mean(as.matrix((m.NA-m.pred.q3)^2),na.rm=TRUE))

#Résultat : RMSE = 0.902, MAE = 0.707

# Question 5 --------------------------------------------------------------
#Determinez le nombre de dimensions optimal (sans appliquer de validation croisee).

#On étudie des dimensions de 10 à 930, par bonds de 20
dim.vector<-seq(from = 10, to = nrow(m), by = 20)
RMSE.q5<-data.frame(dimension=dim.vector,RMSE=rep(0,length(dim.vector)),
                    MAE=rep(0,length(dim.vector)))


#La fonction qui calcule SVD pour les dimensions
#données en argument
SVD.q5 <- function(i)
  # for (i in dim.vector)
{
  Si<-sqrt(S[1:i,1:i])
  Ui<-U[,1:i]
  Vi<-V[,1:i]
  
  m.pred.i<-m.moy2+(Ui%*%t(Si))%*%Si%*%t(Vi)
  m.pred.i[m.pred.i<0]=0
  m.pred.i[m.pred.i>5]=5
  
  RMSE<-sqrt(mean(as.matrix((m.NA-m.pred.i)^2),na.rm=TRUE))
  MAE<-mean(as.matrix(abs(m.NA-m.pred.i)),na.rm=TRUE)
  
  
  return(list("RMSE"=RMSE,"MAE"=MAE))
  
}
result.q5<-sapply(dim.vector,SVD.q5)
RMSE.q5$RMSE<-result.q5[1,]
RMSE.q5$MAE<-result.q5[2,]


#Création du graphique 
plot.RMSE=cbind(dimension=RMSE.q5$dimension,RMSE=RMSE.q5$RMSE)
plot.MAE=cbind(dimension=RMSE.q5$dimension,MAE=RMSE.q5$MAE)
plot(plot.RMSE,col="blue",main = 'Evolution du RMSE et MAE en fonction du nombre de dimensions (sans CV)', 
     xlab = 'Nombre de dimensions', ylab = 'Erreur' )  
xspline(plot.RMSE,shape=1,border='blue')
points(plot.MAE,col="red")
xspline(plot.MAE,shape=1,border='red')
legend('topright',legend=c('RMSE','MAE'),col=c('blue','red'),lty=c(1,1))


#Sans utilise de validation croise, le nombre optimal
#de dimension est egale au nombre d'utilisateur (943)
#Voir le graphique en pdf joint au code (Graph1)
# Question 6 --------------------------------------------------------------

#Determinez le nombre optimal de dimensions, 
#mais en utilisant cette fois une validation croisee.

seed<-c(55,75,95,105,222,60,78,4,6,23)
n_fold<-length(seed)


#On réduit le nombre de dimensions à tester pour racourcir le temps 
#de calcul. On l'a fait une fois initialement pour des dimensions 
#allant de 2 à 943 et nous avons trouvé que la dimension optimale 
#était de 13. De plus, afin d'observer la tendance générale 
#sur le graphique, on considère le vecteur en commentaire
#où les points sont très concentrées entre 1 et 20 et 
#plus éloignés entre 20 et 500. 
dim.vector.q6<-c(c(1:20),seq(from=40,to=100,by=20))
#dim.vector.q6=seq(from=2,to=20,by=1)
ndimension<-length(dim.vector.q6)
result.SVD.q6<-rbind(rep(0,ndimension),rep(0,ndimension))
RMSE.q6<-data.frame(dimension=dim.vector.q6,RMSE=rep(0,ndimension),
                    MAE=rep(0,ndimension))

#Fonction cross validation pour SVD
cross.validation = function (k,pct,error.type){
  set.seed(k)
  j<-m>0
  j.test<-j
  j.test[sample(length(j),pct*length(j))]<-FALSE
  j.train<-!j.test
  m.test<-m.NA
  m.train<-m.NA
  m.train[j.test]<-NA
  m.test[!j.test]<-NA
  moy.row.train<-rowSums(m.train,na.rm=TRUE)/rowSums(m>0,na.rm = TRUE)
  m.norm.q6<-m.train-moy.row.train
  m.norm.q6[is.na(m.norm.q6)]<-0
  m.norm.q6<-Matrix(m.norm.q6, sparse=TRUE)
  
  SVD.q6<-svd(m.norm.q6)
  
  S.q6<-SVD.q6$d
  S.q6<-diag(S.q6)
  U.q6<-SVD.q6$u
  V.q6<-SVD.q6$v
  
  m.moy.q6<-m.train
  m.moy.q6[]<-0
  m.moy.q6<-sweep(m.moy.q6,1,moy.row.train,'+')
  
  #Une fonction pour les prédictions de SVD
  SVD.cv=function (i,error.type)
  {
    Si<-sqrt(S.q6[1:i,1:i])
    Ui<-U.q6[,1:i]
    Vi<-V.q6[,1:i]
    
    m.pred.q6<-m.moy.q6+(Ui%*%t(Si))%*%Si%*%t(Vi)
    m.pred.q6[m.pred.q6<0]=0
    m.pred.q6[m.pred.q6>5]=5
    if(error.type=='RMSE'){
      RMSE.q6<-sqrt(mean(as.matrix((m.test-m.pred.q6)^2),na.rm=TRUE))
      return(RMSE.q6)
    } else if(error.type=='MAE'){
      MAE.q6<-mean(as.matrix(abs(m.test-m.pred.q6)),na.rm=TRUE)
      return(MAE.q6)
    } else{
      return(0)
    }
  }
  result.SVD.q6<-sapply(RMSE.q6$dimension,SVD.cv,error.type=error.type)
  return(result.SVD.q6)
}
RMSE.q6V<-sapply(seed,cross.validation,pct=0.9,error.type='RMSE')
MAE.q6V<-sapply(seed,cross.validation,pct=0.9,error.type='MAE')

#On calcule la moyenne des erreurs issues de la cross-validation
RMSE.q6$RMSE<-rowMeans(RMSE.q6V)
RMSE.q6$MAE<-rowMeans(MAE.q6V)
plot.RMSE=cbind(dimension=RMSE.q6$dimension,RMSE=RMSE.q6$RMSE)
plot.MAE=cbind(dimension=RMSE.q6$dimension,MAE=RMSE.q6$MAE)
plot(plot.RMSE,col="blue",main = 'Evolution du RMSE et MAE en fonction du nombre de dimensions (avec CV)', 
     xlab = 'Nombre de dimensions', ylab = 'Erreur',ylim=c(min(RMSE.q6[,2:3]),max(RMSE.q6[,2:3]))) 
xspline(plot.RMSE,shape=1,border='blue')
points(plot.MAE,col="red")
xspline(plot.MAE,shape=1,border='red')
legend('right',legend=c('RMSE','MAE'),col=c('blue','red'),lty=c(1,1))
#Dimension optimale = 13, RMSE = 0.98975, MAE = 0.80199
#Voir Graph 2 pour l'evolution du RMSE et MAE en fonction du nombre de dimension 


# Question 7 --------------------------------------------------------------
#Comparez la performance de cette approche avec celle d'une approche collaborative de votre choix
#(avec l'erreur quadratique et erreur absolue moyennes). Utilisez une validation croisee.
#Utilisons une methode User-User, sans toutefois prendre en considération les valeurs manquantes et la 
#pondération par rapport aux votes communs. 
seed<-c(55,75,95,105,222,60,78,4,6,23)
n_fold<-length(seed)

#Cross validation avec user-user
cv.useruser = function (k,pct,error.type){
  set.seed(k)
  j<-m>0
  j.test<-j
  j.test[sample(length(j),pct*length(j))]<-FALSE
  j.train<-!j.test
  m.test<-m
  m.train<-m
  m.train[j.test]<-0
  m.test[!j.test]<-NA
  m.NA=m.train
  m.NA[m.NA==0]=NA
  sums_2<-rowSums(m.train^2)
  dist_eucl<-sweep(sweep(-2*m.train %*% t(m.train),1,sums_2,"+"),2,sums_2,"+")
  neighbors<-t(apply(dist_eucl,1,order.partial))
  cosinus.mat=matrix(0,nrow=dim(m)[1],ncol=dim(neighbors)[2])
  estimation=matrix(0,nrow=dim(m)[1],ncol=dim(m)[2])
  avg.user=rowSums(m.train)/rowSums(m.train>0)
  for (i in c(1:dim(m)[1])){
    cosinus.mat[i,]=as.vector(cosinus.vm(m.train[i,],t(m.train[neighbors[i,],])))
  }
  kappa=(rowSums(cosinus.mat))^-1
  ecart=matrix(0,nrow=dim(m)[1], ncol=dim(m)[2])
  m.centre=m.NA-avg.user
  m.centre[is.na(m.centre)]=0
  for (i in c(1:dim(m)[1])){
    estimation[i,]=as.vector(avg.user[i]+kappa[i]*(cosinus.mat[i,]%*%m.centre[neighbors[i,],]))
  }
  estimation[estimation>5]=5
  
  if(error.type=='RMSE'){
    RMSE.q7<-sqrt(mean(as.matrix((m.test-estimation)^2),na.rm=TRUE))
    return(RMSE.q7)
  } else if(error.type=='MAE'){
    MAE.q7<-mean(as.matrix(abs(m.test-estimation)),na.rm=TRUE)
    return(MAE.q7)
  } else{
    return(0)
  }
  
}
RMSE.q7V<-sapply(seed,cv.useruser,pct=0.9,error.type='RMSE')
MAE.q7V<-sapply(seed,cv.useruser,pct=0.9,error.type='MAE')


RMSE.q7<-mean(RMSE.q7V)
MAE.q7<-mean(MAE.q7V)

#Résultat: RMSE = 1.01, MAE = 0.809





#DISCUSSION 

#Compilation des résultats avec cross-validation : 
                                    #RMSE          #MSE
#Baseline (moyenne colonne+ligne): 0.982           0.795

#SVD dimension optimale (13)       0.990           0.802

#User-User                         1.01            0.809


#Selon les résultats, on voit que la méthode user-user est moins performante que la méthode 
#SVD. Par contre, fait étonnant, c'est notre baseline qui nous donne les meilleurs résultats. 
#Dans un prochain travail, on pourrait tenter d'améliorer la méthode SVD avec une normalisation
#différente de la matrice (ex: en apportant une contrainte sur l'écart-type i.e. en calculant une matrice 
#centrée réduite). Sinon, on peut aussi améliorer la méthode user-user en calculant les 
#cosinus seulement sur les valeurs communes et en apportant une pondération sur le vote commun 
#lors du calcul de l'estimation. Toutefois, on conclut que les méthodes sont cohérentes et donnent 
#des résultats similaires pour le présent dataset. 
