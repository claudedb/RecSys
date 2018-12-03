library(curl)
library(Matrix)
library(ggplot2)
load(url('http://www.groupes.polymtl.ca/log6308/Public/20173/1486776.Rsave'))

max.nindex <- function(m, n=11) {
  i <- order(m, decreasing=TRUE)
  return(i[2:n])
}

cosinus.vm <- function(v,m) {
  n <- sqrt(colSums(m^2));k= sqrt(rowSums(t(v^2)));
  p=(t(v) %*% m)/k; a=t(p)/n
  return(a)
}

sums.2<-colSums(m^2)
dist.eucl<-sweep(sweep(-2*t(m) %*% m,2,sums.2,"+"),1,sums.2,"+")
neighbors<-t(apply(dist.eucl,1,max.nindex))
m.NA <- m
m.NA[m.NA ==0] <- NA
seed<-c(55,75,95,105,222,60,78,4,6,23)
n_fold<-length(seed)

# Question 7 --------------------------------------------------------------

cosinus <- cosinus.vm(m,m)
neighbors.cos <- t(apply(cosinus,1,max.nindex))


# Question 8 --------------------------------------------------------------
cross.validation.uu <- function(k)
{
  #k<-10
  set.seed(k)
  j<-m>0
  j.test<-j
  j.test[sample(length(j),0.85*length(j))]<-FALSE
  j.train<-!j.test
  m.test<-m.NA
  m.train<-m.NA
  m.train[j.test]<-0
  m.test[!j.test]<-NA
  m.NA <-m.train
  m.NA[m.NA ==0 ]<- NA
  
  votes.uu <- t(m.train)
  cosinus.uu <- cosinus.vm(votes.uu,votes.uu)
  neighbors.uu <- t(apply(cosinus.uu,1,max.nindex))
  
  avg.user <- rowMeans(m.NA,na.rm=TRUE)
  kappa <- 1/rowSums(neighbors.uu)
  m.centre <- m.NA - avg.user
  m.centre[is.na(m.centre)]<-0
  estimation <- m.train
  estimation[] <- NA
  
  for (i in 1 : nrow(m.train))
  {
    estimation[i,] <- as.vector(kappa[i] * 
                                  (cosinus.uu[i,neighbors.uu[i,]] %*% m.centre[neighbors.uu[i,],]))
    
    estimation[i,] <- avg.user[i] + estimation[i,]
    
    estimation[estimation>5] <- 5
  }
  
  
  
  RMSE <- sqrt(mean(as.matrix((m.test-estimation)^2),na.rm=TRUE))
  return(RMSE)
}
RMSE.uu <- sapply(seed,cross.validation.uu)


# Question 9 --------------------------------------------------------------

dim.vector<-seq(from = 2, to = ncol(m), by = 1)
cross.validation.SVD = function (k)
{
  
  set.seed(k)
  #set.seed(10)
  j<-m>0
  j.test<-j
  j.test[sample(length(j),0.85*length(j))]<-FALSE
  j.train<-!j.test
  m.test<-m.NA
  m.train<-m.NA
  m.train[j.test]<-NA
  m.test[!j.test]<-NA
  
  moy.row.train<-rowSums(m.train,na.rm=TRUE)/rowSums(m>0,na.rm = TRUE)
  m.norm<-m.train-moy.row.train
  m.norm[is.na(m.norm)]<-0
  m.norm<-Matrix(m.norm, sparse=TRUE)
  SVD.m<-svd(m.norm)
  
  m.moy<-m.train
  m.moy[]<-0
  m.moy<-sweep(m.moy,1,moy.row.train,'+')

  
  #La fonction qui calcule SVD pour les dimensions
  #donn??es en argument
  SVD <- function(i)
  {
    #i=2;
    S <-sqrt(diag(SVD.m$d))
    Si <- S[1:i,1:i]
    Ui<-SVD.m$u[,1:i]
    Vi<-SVD.m$v[,1:i]
    
    m.pred.i<-m.moy+(Ui%*%t(Si))%*%Si%*%t(Vi)
    m.pred.i[m.pred.i<0]=0
    m.pred.i[m.pred.i>5]=5
    
    RMSE<-sqrt(mean(as.matrix((m.test-m.pred.i)^2),na.rm=TRUE))
    # MAE<-mean(as.matrix(abs(m.NA-m.pred.i)),na.rm=TRUE)
    
    
    return(RMSE)
    
  }
  
  results<-sapply(dim.vector,SVD)
  return(results)
  
}


cv.result <- sapply(seed,cross.validation.SVD)
RMSE.SVD <- rowMeans(cv.result)

SVD.results <- data.frame(dimension = dim.vector, RMSE = RMSE.SVD)

g <- ggplot(SVD.results,aes(dimension,RMSE))+geom_line()
#Min at 16 dimension

# Question 10 -------------------------------------------------------------
#Let's use a item-item approach

cross.validation.ii <- function(k)
{
  #k<-10
  set.seed(k)
  j<-m>0
  j.test<-j
  j.test[sample(length(j),0.85*length(j))]<-FALSE
  j.train<-!j.test
  m.test<-m.NA
  m.train<-m.NA
  m.train[j.test]<-0
  m.test[!j.test]<-NA
  m.NA <-m.train
  m.NA[m.NA ==0 ]<- NA
  
  votes.ii <- m.train
  cosinus.ii <- cosinus.vm(votes.ii,votes.ii)
  neighbors.ii <- t(apply(cosinus.ii,1,max.nindex))
  
  avg.item <- colMeans(m.NA,na.rm=TRUE)
  kappa <- 1/rowSums(neighbors.ii)
  m.centre <- m.NA - avg.item
  m.centre[is.na(m.centre)]<-0
  estimation.ii <- m.train
  estimation.ii[] <- NA
  
  for (i in 1 : ncol(m.train))
  {
    estimation.ii[i,] <- as.vector(kappa[i] * 
                                  (cosinus.ii[i,neighbors.ii[i,]] %*% m.centre[neighbors.ii[i,],]))
    
    estimation.ii[i,] <- avg.item[i] + estimation.ii[i,]
    
    estimation.ii[estimation.ii>5] <- 5
  }
  
  
  
  RMSE <- sqrt(mean(as.matrix((m.test-estimation.ii)^2),na.rm=TRUE))
  return(RMSE)
}
RMSE.ii <- sapply(seed,cross.validation.ii)


# Question 11 -------------------------------------------------------------

comparaison.methode <- data.frame(seed = seed, uu = RMSE.uu, ii = RMSE.ii, SVD = cv.result[16,])
