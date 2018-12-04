library(curl)
library(Matrix)
library(ggplot2)
load(url('http://www.groupes.polymtl.ca/log6308/Public/20173/1486776.Rsave'))

max.nindex <- function(m, n=10) {
  i <- order(m, decreasing=TRUE)
  return(i[2:(n+1)])
}
min.nindex <- function(m, n=10) {
  i <- order(m, decreasing=FALSE)
  return(i[2:(n+1)])
}

cosinus.vm <- function(v,m) {
  n <- sqrt(colSums(m^2));k= sqrt(rowSums(t(v^2)));
  p=(t(v) %*% m)/k; a=t(p)/n
  return(a)
}

# Question 7 --------------------------------------------------------------
#Quels sont les 10 items les plus similaires à q item selon le cosinus
#Vérifiez la similarité avec les catégorie
cosinus.item <- cosinus.vm(m,m)

plus.similaire <- t(apply(cosinus.item,1,max.nindex))

#On utilise le ID de q.item pour trouver les 10 items les plus similaires

# Question 8 --------------------------------------------------------------
#Calculer les votes manquants en prenant les 10 utilisateurs les plus similaires

user.user <- function(m,n.voisins)
{
  #n.voisins <- 10
  votes.uu <- t(m)
  sums.2<-colSums(votes.uu^2)
  dist.eucl<-sweep(sweep(-2*t(votes.uu) %*% votes.uu,2,sums.2,"+"),1,sums.2,"+")
  neighbors.uu<-t(apply(dist.eucl,1,min.nindex,n=n.voisins))
  
  cosinus.uu <- cosinus.vm(votes.uu,votes.uu)
  votes.NA <- votes.uu
  votes.NA[votes.NA ==0] <- NA
  
  avg.user <- colMeans(votes.NA,na.rm=TRUE)
  votes.centre <- votes.NA - avg.user
  votes.centre[is.na(votes.centre)]<-0
  estimation <- votes.uu
  estimation[] <- NA
  
  for (i in 1 : ncol(votes.uu))
  {
    kappa <- 1/sum(cosinus.uu[i,neighbors.uu[i,]])
    
    estimation[,i] <- kappa * (votes.centre[,neighbors.uu[i,]] %*% 
                                 cosinus.uu[i,neighbors.uu[i,]])
    
    estimation[,i] <- avg.user[i] + estimation[,i]
    
    estimation[estimation>5] <- 5
  }
  return(estimation)
}
estimation.uu <- user.user(m,10)
RMSE.uu <- sqrt(mean(as.matrix((t(m)-estimation.uu)^2),na.rm=TRUE))

# Question 9 --------------------------------------------------------------
#Matrice m avec SVD et trouvez le nombre de dimensions optimales
#Utilisons la matrice originale

SVD.estimation <- function(m,n.dimensions)
{
  moy.row<-rowSums(m,na.rm=TRUE)/rowSums(m>0,na.rm = TRUE)
  m.NA <- m
  m.NA[m.NA ==0] <- NA
  m.norm<-m.NA-moy.row
  m.norm[is.na(m.norm)]<-0
  m.norm<-Matrix(m.norm, sparse=TRUE)
  m.SVD<-svd(m.norm)
  
  m.moy<-m
  m.moy[]<-0
  m.moy<-sweep(m.moy,1,moy.row,'+')
  
  S <-sqrt(diag(m.SVD$d))
  Si <- S[1:n.dimensions,1:n.dimensions]
  Ui<-m.SVD$u[,1:n.dimensions]
  Vi<-m.SVD$v[,1:n.dimensions]
  
  m.pred.i<-m.moy+(Ui%*%t(Si))%*%Si%*%t(Vi)
  m.pred.i[m.pred.i<0]=0
  m.pred.i[m.pred.i>5]=5
  return(m.pred.i)
  
}

seed<-c(55,75,95,105,222,60,78,4,6,23)
n_fold<-length(seed)

dim.vector<-seq(from = 2, to = ncol(m), by = 1)

cross.validation.svd <-function(k,m)
{
  set.seed(k)
  #set.seed(10)
  j<-m>0
  j.test<-j
  j.test[sample(length(j),0.85*length(j))]<-FALSE
  j.train<-!j.test
  m.NA <- m
  m.NA[m.NA ==0] <- NA
  m.test<-m.NA
  m.train<-m.NA
  m.train[j.test]<-NA
  m.test[!j.test]<-NA
  RMSE.SVD <- rep(0,length(dim.vector))
  
  for (i in 1:length(dim.vector) )
  {
    prediction.SVD <- SVD.estimation(m.train,dim.vector[i])
    RMSE.SVD[i] <- sqrt(mean(as.matrix((m.test-prediction.SVD)^2),na.rm=TRUE))
  }
  
  return(RMSE.SVD)
  
}
RMSE.SVD <-sapply(seed,cross.validation.svd,m=m)
RMSE.SVD.CV <- data.frame(dimensions = dim.vector,
                          RMSE = rowMeans(RMSE.SVD))

#Min dimension = 15
SVD.dim <- RMSE.SVD.CV[which(RMSE.SVD.CV$RMSE == min(RMSE.SVD.CV$RMSE)),1]

# Question 10 -------------------------------------------------------------
#Méthode alternative
#Utilisons une méthode item item, similaire au user user du numéro 1.

item.item <- function(m,n.voisins)
{
  #n.voisins <- 10
  votes.ii <- m
  sums.2<-colSums(votes.ii^2)
  dist.eucl<-sweep(sweep(-2*t(votes.ii) %*% votes.ii,2,sums.2,"+"),1,sums.2,"+")
  neighbors.ii<-t(apply(dist.eucl,1,min.nindex,n=n.voisins))
  
  cosinus.ii <- cosinus.vm(votes.ii,votes.ii)
  votes.NA <- votes.ii
  votes.NA[votes.NA ==0] <- NA
  
  avg.item <- colMeans(votes.NA,na.rm=TRUE)
  votes.centre <- votes.NA - avg.item
  votes.centre[is.na(votes.centre)]<-0
  estimation <- votes.ii
  estimation[] <- NA
  
  for (i in 1 : ncol(votes.ii))
  {
    kappa <- 1/sum(cosinus.ii[i,neighbors.ii[i,]])
    
    estimation[,i] <- kappa * (votes.centre[,neighbors.ii[i,]] %*% 
                                 cosinus.ii[i,neighbors.ii[i,]])
    
    estimation[,i] <- avg.item[i] + estimation[,i]
    
    estimation[estimation>5] <- 5
  }
  return(estimation)
}

estimation.ii <- item.item(m,10)
RMSE.ii <- sqrt(mean(as.matrix((m-estimation.ii)^2),na.rm=TRUE))

# Question 11 -------------------------------------------------------------
#Comparez méthode 8 9 10 à l'aide du RMSE

cross.validation.ii <- function(m,k)
{
  set.seed(k)
  #set.seed(10)
  j<-m>0
  j.test<-j
  j.test[sample(length(j),0.85*length(j))]<-FALSE
  j.train<-!j.test
  m.NA <- m
  m.NA[m.NA ==0] <- NA
  m.test<-m.NA
  m.train<-m.NA
  m.train[j.test]<-0
  m.test[!j.test]<-NA
  
  estimation <- item.item(m.train,10)
  RMSE <- sqrt(mean(as.matrix((m.test-estimation)^2),na.rm=TRUE))
  
}

cross.validation.uu <- function(m,k)
{
  set.seed(k)
  #set.seed(10)
  j<-m>0
  j.test<-j
  j.test[sample(length(j),0.85*length(j))]<-FALSE
  j.train<-!j.test
  m.NA <- m
  m.NA[m.NA ==0] <- NA
  m.test<-m.NA
  m.train<-m.NA
  m.train[j.test]<-0
  m.test[!j.test]<-NA
  
  estimation <- user.user(m.train,10)
  RMSE <- sqrt(mean(as.matrix((t(m.test)-estimation)^2),na.rm=TRUE))
  
}

comparaison.RMSE <- data.frame(fold = 1:length(seed),
                               RMSE.ii = sapply(seed,cross.validation.ii,m=m),
                               RMSE.uu = sapply(seed,cross.validation.uu,m=m),
                               RMSE.SVD = RMSE.SVD[SVD.dim,])
