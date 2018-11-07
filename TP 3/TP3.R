#TP 3 Code
#Par Claude Demers-Belanger (1534217) et Mikael Perreault (1741869)

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et verifier que les packages necessaires sont bien installes
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
# Determiner une baseline pour l'evaluation de la methode.
# Prenons une methode ou nous appliquons la moyenne des lignes et des colonnes pour evaluer les notes

moy.col<-colSums(m)/colSums(m!=0)
moy.row<-rowSums(m)/rowSums(m!=0)

m.baseline<-m
m.baseline[]<-0
m.baseline<-sweep(sweep(m.baseline,1,moy.row/2,"+"),2,moy.col/2,"+")
m.NA<-m
m.NA[m.NA==0]<-NA
RMSE.baseline<-sqrt(mean(as.matrix((m.NA-m.baseline)^2),na.rm=TRUE))
MAE.baseline<-mean(as.matrix(abs(m.NA-m.baseline)),na.rm=TRUE)


# Question 2 --------------------------------------------------------------
# Appliquer la decomposition SVD

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
#Determinez le nombre de dimensions optimal (sans appliquer de validation croisee).


dim.vector<-seq(from = 10, to = nrow(m), by = 20)
RMSE.q5<-data.frame(dimension=dim.vector,RMSE=rep(0,length(dim.vector)),
                    MAE=rep(0,length(dim.vector)))


SVD.q5 <- function(i)
 # for (i in dim.vector)
{
  Si<-sqrt(S[1:i,1:i])
  Ui<-U[,1:i]
  Vi<-V[,1:i]
  
  m.pred.i<-m.moy2+(Ui%*%t(Si))%*%Si%*%t(Vi)
  
  
  RMSE<-sqrt(mean(as.matrix((m.NA-m.pred.i)^2),na.rm=TRUE))
  MAE<-mean(as.matrix(abs(m.NA-m.pred.i)),na.rm=TRUE)
  

  return(list("RMSE"=RMSE,"MAE"=MAE))
  
}
#essai<-sapply(dim.vector,SVD.q5)
result.q5<-sapply(dim.vector,SVD.q5)
RMSE.q5$RMSE<-result.q5[1,]
RMSE.q5$MAE<-result.q5[2,]

g<-ggplot(RMSE.q5,aes(x=dimension))+
  geom_point(aes(y=RMSE,color='RED'))+
  geom_point(aes(y=MAE,color='BLUE'))+
  labs(title='Evolution du RMSE et MAE en fonction du nombre de dimensions',
       x='Dimensions',y='Erreur',color="Type d'erreur")+
  scale_color_manual(labels=c('RMSE',"MAE"),values=c('RED','BLUE'))

#Sans utilise de validation croise, le nombre maximal de dimension 
#est egale au nombre d'utilisateur

# Question 6 --------------------------------------------------------------

#Determinez le nombre optimal de dimensions, 
#mais en utilisant cette fois une validation croisee.

seed<-c(55,75,95,105,222,60,78,4,6,23)
n_fold<-length(seed)

dim.vector.q6<-seq(from = 2, to = 20, by =1)
ndimension<-length(dim.vector.q6)
result.SVD.q6<-rbind(rep(0,ndimension),rep(0,ndimension))
RMSE.q6<-data.frame(dimension=dim.vector.q6,RMSE=rep(0,ndimension),
                    MAE=rep(0,ndimension))

cross.validation = function (k){
set.seed(k)
#set.seed(35)
j<-m>0
j.test<-j
#table(as.vector(j.test),useNA='always')
j.test[sample(length(j),.90*length(j))]<-FALSE
#table(j.test,useNA='always')
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

SVD.cv=function (i)
{
  #print(i)
  Si<-sqrt(S.q6[1:i,1:i])
  Ui<-U.q6[,1:i]
  Vi<-V.q6[,1:i]
  
  m.pred.q6<-m.moy.q6+(Ui%*%t(Si))%*%Si%*%t(Vi)
  
  
  RMSE.q6<-sqrt(mean(as.matrix((m.test-m.pred.q6)^2),na.rm=TRUE))
  
  MAE.q6<-mean(as.matrix(abs(m.test-m.pred.q6)),na.rm=TRUE)
  return(list("RMSE"=RMSE.q6,"MAE"=MAE.q6))
}
result<-sapply(RMSE.q6$dimension,SVD.cv)
result<-matrix(nrow=2,ncol=length(RMSE.q6$dimension),as.double(result))
result.SVD.q6<-result.SVD.q6+result/n_fold
return(result.SVD.q6)
}
essai<-sapply(seed,cross.validation)


RMSE.q6$RMSE<-RMSE.q6$RMSE/n_fold
RMSE.q6$MAE<-RMSE.q6$MAE/n_fold

g2<-ggplot(RMSE.q6,aes(x=dimension))+
  geom_point(aes(y=RMSE))+
  geom_point(aes(y=MAE))+
  labs(title='Evolution du RMSE et MAE en fonction du nombre de dimensions',
       x='Dimensions',y='Erreur',color="Type d'erreur")+
  scale_colour_manual(values=c('black','blue'))
#Dimension optimal = 10


# Question 7 --------------------------------------------------------------
#Comparez la performance de cette approche avec celle d'une approche collaborative de votre choix
#(avec l'erreur quadratique et erreur absolue moyennes). Utilisez une validation croisee.
#Utilisons une methode User-User. Similaire au TP 1

seed.q7<-c(55,75,95,105,222,60,78,4,6,23)
n_fold.q7<-length(seed.q7)

error.q7<-data.frame(seed=seed.q7,RMSE.SVD=rep(0,n_fold.q7),
                    MAE.SVD=rep(0,n_fold.q7),RMSE.ii=rep(0,n_fold.q7),MAE.ii=rep(0,n_fold.q7))

for(s in seed.q7)
{
  set.seed(s)
  l<-m>0
  l.test<-l
  #table(as.vector(j.test),useNA='always')
  l.test[sample(length(l),.90*length(l))]<-FALSE
  #table(j.test,useNA='always')
  l.train<-!l.test
  m.test<-m.NA
  m.train<-m.NA
  m.train[j.test]<-NA
  m.test[!j.test]<-NA
  
  ## Utilisons SVD pour faire des predictions avec 10 dimensions
  #(minimum trouve au numero 6)
  
  moy.row.train<-rowSums(m.train,na.rm=TRUE)/rowSums(m>0,na.rm = TRUE)
  m.norm.q7<-m.train-moy.row.train
  m.norm.q7[is.na(m.norm.q7)]<-0
  m.norm.q7<-Matrix(m.norm.q7, sparse=TRUE)
  
  SVD.q7<-svd(m.norm.q7)
  
  S.q7<-SVD.q7$d
  S.q7<-diag(S.q7)
  U.q7<-SVD.q7$u
  V.q7<-SVD.q7$v
  
  m.moy.q7<-m.train
  m.moy.q7[]<-0
  m.moy.q7<-sweep(m.moy.q7,1,moy.row.train,'+')
  
  S7<-sqrt(S.q7[1:10,1:10])
  U7<-U.q7[,1:10]
  V7<-V.q7[,1:10]
  
  m.pred.q7<-m.moy.q7+(U7%*%t(S7))%*%S7%*%t(V7)
  
  error.q7$RMSE.SVD[which(error.q7$seed==s)]<-
    sqrt(mean(as.matrix((m.test[!j.train]-
                           m.pred.q7[!j.train])^2),na.rm=TRUE))
  
  error.q7$MAE.SVD[which(error.q7$seed==s)]<-
    mean(as.matrix(abs(m.test[!j.train]-
                         m.pred.q7[!j.train])),na.rm=TRUE)
  
  ##Prenons Maintenant une méthode Item-Item
  sums.q7<-colSums(m.NA^2)
  dist.eucl<-sweep(sweep(-2*t(m.NA) %*% m.NA,1,sums.q7,"+"),2,sums.q7,"+")
  
  #order.partial=function(vec){
  # idx<-which(vec<=sort(vec,partial=20)[20])
  #idx[order(vec[idx])][1:20]
  #}
  #neighbors<-t(apply(dist.euc,1,order.partial))
  
}











