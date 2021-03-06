library(Matrix)
library(bitops)
library(RCurl)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2)
rm(list=ls())
setwd('C:/Users/mikap/OneDrive/Documents/GitHub/RecSys/TP 3')

u.data<-read.csv('Data/u.data.csv',sep='|')
m <- as.matrix(m.sparse)
m.sparse <- sparseMatrix(u.data[,1],u.data[,2],x=u.data[,3])
rownames(m) <- paste('u', 1:nrow(m), sep='')
colnames(m) <- paste('i', 1:ncol(m), sep='')


m.na <- m
m.na[m.na==0] <- NA

# Question 1
# D�terminez un point de comparaison pour la pr�vision de votes (une performance minimale)
# On pr�dit les votes d'une autre mani�re : pour chaque film, on calcule la moyenne des votes re�us
# et on calcul pour chaque utilisateur la moyenne des votes attribu�s

moyenneVotesFilms <- matrix(colMeans(m.na, na.rm = TRUE), nrow = nrow(m.na), ncol = ncol(m.na), byrow = TRUE)
moyenneVotesUtilisateurs <- matrix(rowMeans(m.na, na.rm = TRUE), nrow = nrow(m.na), ncol = ncol(m.na), byrow = FALSE)

new.m <- (moyenneVotesFilms + moyenneVotesUtilisateurs) / 2

# On calcule l'erreur absolue moyenne pour v�rifier si les pr�dictions sont admissibles
mean(abs(new.m - m.na), na.rm = TRUE)
# On obtient une erreur d'environ 0.783 ce qui est assez admissible

# On calcule l'erreur quadratique moyenne pour v�rifier si les pr�dictions sont admissibles
sqrt(mean((new.m - m.na) * (new.m - m.na), na.rm = TRUE))
# On obtient une erreur d'environ 0.967 ce qui est loin d'�tre admissible


# Question 2
# Appliquer la d�composition SVD (en prenant soin de normaliser au pr�alable)
# Normalisation 
m.question2 <- m
m.question2[m == 0] <- moyenneVotesFilms[m == 0]
mNormalise <- m.question2 - moyenneVotesUtilisateurs

# D�composition SVD
# transformation svd peut prendre du temps... �tre patient
mSVD <- svd(mNormalise)
S <- mSVD$d
S <- as.matrix(diag(S,length(S)))
U <- mSVD$u
V <- mSVD$v


# Question 3
# Effectuez l'estimation des votes sur la base de SVD avec 10 dimensions.
# On r�duit la d�composition SVD � 10 dimensions
S10 <- S[1:10, 1:10]
S10 <- sqrt(S10)
U10 <- U[, 1:10]
V10 <- V[, 1:10]

# voir formule � la fin de la section 3.1.1 de l'article
UtS <- U10 %*% t(S10)
StV <- S10 %*% t(V10)

votesPredis <- moyenneVotesUtilisateurs + UtS %*% StV


# Question 4
# Calculez l'erreur absolue moyenne et l'erreur quadratique moyenne.

# Erreur absolue moyenne
mean(abs(votesPredis - m.na), na.rm = TRUE)
# On otient une erreur d'environ 0.701

# Erreur quadratique moyenne
sqrt(mean((votesPredis - m.na)^2, na.rm = TRUE))
# On obtient une erreur d'environ 0.887

# On obtient donc des erreurs inf�rieures � celles calcul�es pr�c�dement 
# Cette m�thode de pr�diction est donc plus pr�cise


# Question 5
# D�terminez le nombre de dimensions optimal (sans appliquer de validation crois�e). 
# Un graphique doit indiquer la performance par nombre de dimension (semblable au rapport Sarwar et al.)

errorArray <- matrix(data = NA, nrow = nrow(S), ncol = 2)
# Le while peut prendre du temps �tre patient...
palier <- 20
i <- palier
while (i <= nrow(S))
{
  # pour voir l'avanc�e si besoin...
  #print(i)
  S.i <- S[1:i, 1:i]
  S.i <- sqrt(S.i)
  U.i <- U[, 1:i]
  V.i <- V[, 1:i]
  
  UtS.i <- U.i %*% t(S.i)
  StV.i <- S.i %*% t(V.i)
  
  votesPredis.i <- moyenneVotesUtilisateurs + UtS.i %*% StV.i
  
  errorArray[i,1] <- mean(abs(votesPredis.i - m.na), na.rm = TRUE)
  errorArray[i,2] <- sqrt(mean((votesPredis.i - m.na)^2, na.rm = TRUE))
  
  i <- i + palier
}

# Graphique r�presentant l'�volution de l'erreur absolue moyenne en fonction du nombre de dimensions consid�r�es
plot(errorArray[,1], col="black", xlab = "Nombre de dimensions consid�r�es", ylab = "Erreur absolue moyenne", main = "Evolution de l'erreur absolue moyenne en fonction du nombre de dimensions consid�r�es", ylim = range(0:1))

# Graphique repr�sentant l'�volution de l'erreur quadratique moyenne en fonction du nombre de dimensions consid�r�es
plot(errorArray[,2], col="black", xlab = "Nombre de dimensions consid�r�es", ylab = "Erreur quadratique moyenne", main = "Evolution de l'erreur quadratique moyenne en fonction du nombre de dimensions consid�r�es", ylim = range(0:1))


# Question 6
# Refaites la pr�vision bas�e sur le nombre optimal de dimensions, mais en utilisant cette fois une validation crois�e.

getPredictionSVD = function(dimension, matriceNormalisee)
{
  matriceNormaliseeSVD <- svd(matriceNormalisee)
  S <- matriceNormaliseeSVD$d
  S <- as.matrix(diag(S,length(S)))
  U <- matriceNormaliseeSVD$u
  V <- matriceNormaliseeSVD$v
  
  S.dimension <- S[1:dimension, 1:dimension]
  S.dimension <- sqrt(S.dimension)
  U.dimension <- U[, 1:dimension]
  V.dimension <- V[, 1:dimension]
  
  UtS.dimension <- U.dimension %*% t(S.dimension)
  StV.dimension <- S.dimension %*% t(V.dimension)
  
  return(moyenneVotesUtilisateurs + UtS.dimension %*% StV.dimension)
}

getValidationCroisee = function(dimension = nrow(m), matrice = m)
{
  errorArray <- matrix(data = NA, nrow = 10, ncol = 2)
  
  # Index al�atoire des donn�es de tests
  i.observed <- which(matrice > 0)
  i.hasard <- sample(i.observed, length(i.observed))
  i.observed <- which(matrice > 0)
  i.hasard <- sample(i.observed, length(i.observed))
  fold.size <- round(length(i.hasard) / 10)
  
  for (i in 1:10)
  {
    # Cr�ation d'une liste de bool�ens �galent � FALSE
    i.false <- rep(FALSE, length(matrice))
    
    fold.number <- i
    
    # Cr�ation de la liste des cellules de test
    i.test.b <- i.false
    # Les cellules correspondant aux tests sont fix�es � TRUE et les autres restent � FALSE
    i.test.b[i.hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)]] <- TRUE
    
    # Cr�ation de la liste des cellules d'entrainement
    # Cette liste est l'oppos�e de la liste i.test.b 
    # Les cellules qui ne sont pas des cellules de test, sont des cellules d'entrainement donc � TRUE ici
    i.train.b <- !i.test.b
    
    matrice.train <- matrice
    # On supprime les donn�es test des donn�es d'entrainement
    matrice.train[i.test.b] <- 0  
    matrice.train[matrice.train==0] <- moyenneVotesFilms[matrice.train==0]
    matrice.norm.train <- matrice.train - moyenneVotesUtilisateurs
    
    prediction = getPredictionSVD(dimension,matrice.norm.train)
    
    # Calcul de l'erreur absolue moyenne
    errorArray[i,1] <- mean(abs(prediction[i.test.b] - matrice[i.test.b]), na.rm=T)
    # Calcul de l'erreur quadratique moyenne
    errorArray[i,2] <- sqrt(mean((prediction[i.test.b] - matrice[i.test.b])^2, na.rm=T))
  }
  # On retourne la moyenne de chaque erreur
  return(colMeans(errorArray))
}

# cette fonction prend du temps � s'ex�cuter
getValidationCroisee()


# Question 7
# Comparez la performance de cette approche avec celle d'une approche collaborative de votre choix 
# (avec l'erreur quadratique et erreur absolue moyennes)
# Utilisez une validation crois�e

# Validation crois�e pour trouver le nombre optimale de dimension
getValidationCroisee_dimensionOptimale = function(matrice = m, kmax, kpas) 
{
  erreurAbsolueMoyenne <- matrix(nrow=10, ncol=kmax)
  erreurQuadratiqueMoyenne <- matrix(nrow=10, ncol=kmax)
  
  # Index al�atoire des donn�es de tests
  i.observed <- which(matrice > 0)
  i.hasard <- sample(i.observed, length(i.observed))
  i.observed <- which(matrice > 0)
  i.hasard <- sample(i.observed, length(i.observed))
  fold.size <- round(length(i.hasard) / 10)
  
  # Test crois� : 9 cellules pour l'entrainement et 1 cellule pour le test
  for (i in 1:10) 
  {
    # Cr�ation d'une liste de bool�ens �galent � FALSE
    i.false <- rep(FALSE, length(matrice))
    
    fold.number <- i 
    
    # Cr�ation de la liste des cellules de test
    i.test.b <- i.false
    # Les cellules correspondant aux tests sont fix�es � TRUE et les autres restent � FALSE
    i.test.b[i.hasard[((fold.number-1) * fold.size):((fold.number) * fold.size)]] <- TRUE
    
    # Cr�ation de la liste des cellules d'entrainement
    # Cette liste est l'oppos�e de la liste i.test.b 
    # Les cellules qui ne sont pas des cellules de test, sont des cellules d'entrainement donc � TRUE ici
    i.train.b <- !i.test.b
    
    matrice.train <- matrice
    # On supprime les donn�es test des donn�es d'entrainement
    matrice.train[i.test.b] <- 0  
    matrice.train[matrice.train==0] <- moyenneVotesFilms[matrice.train==0]
    matrice.norm.train <- matrice.train - moyenneVotesUtilisateurs
    
    # Decomposition SVD
    matrice.svd <- svd(matrice.norm.train)
    S <- matrice.svd$d
    S <- as.matrix(diag(S,length(S)))
    U <- matrice.svd$u
    V <- matrice.svd$v
    
    for (k in seq(1, kmax, by=kpas)) 
    {
      #cat("Cluster ", i, " Dimension ", k, '\n')
      # On utilise k dimensions, donc on r�duit les 3 matrices D, U, et V pour tenir compte de cela
      Sk <- S[1:k,1:k]
      Sk <- sqrt(Sk)
      Uk <- U[,1:k]
      Vk <- V[,1:k]
      
      # Enfin on utilise la formule � la fin de la section 3.1.1 de l'article
      UtS <- Uk %*% t(Sk)
      StV <- Sk %*% t(Vk)
      
      # Pr�diction des votes selon l'article de Sarwar et al
      prediction <- moyenneVotesUtilisateurs + UtS %*% StV
      
      # Calcul de l'erreur absolue moyenne
      erreurAbsolueMoyenne[i,k] <- mean(abs(prediction[i.test.b] - matrice[i.test.b]), na.rm=T)
      # Calcul de l'erreur quadratique moyenne
      erreurQuadratiqueMoyenne[i,k] <- sqrt(mean((prediction[i.test.b] - matrice[i.test.b])^2, na.rm=T))
    }
  }
  # Moyenne des erreurs absolues pour une dimension
  eAM <- colMeans(erreurAbsolueMoyenne, na.rm=T)
  # Moyenne des erreurs quadratiques pour une dimension
  eQM <- colMeans(erreurQuadratiqueMoyenne, na.rm=T)
  
  # Graphique r�presentant l'�volution de l'erreur absolue moyenne en fonction du nombre de dimensions consid�r�es
  plot(eAM, col="black", xlab = "Nombre de dimensions consid�r�es", ylab = "Erreur absolue moyenne", main = "Variation de l'erreur absolue moyenne en fonction du nombre de dimensions consid�r�es", ylim = range(0:1))
  
  # Graphique repr�sentant l'�volution de l'erreur quadratique moyenne en fonction du nombre de dimensions consid�r�es
  plot(eQM, col="black", xlab = "Nombre de dimensions", ylab = "Erreur quadratique moyenne", main = "Evolution de l'erreur quadratique moyenne en fonction du nombre de dimensions consid�r�es", ylim = range(0:1))
  
  resultat <- (eAM + eQM)/2
  return (which.min(resultat))
}

getValidationCroisee_dimensionOptimale(m, 943, 10) 
# R�sultat = 11, donc on va chercher entre 1 et 30
getValidationCroisee_dimensionOptimale(m, 30, 1)   
# R�sultat variable � cause du hasard, on trouve 11, 14 ou 15


# Approche collaborative : item-item
cosinus.vm <- function(v,m) { 
  n <- sqrt(colSums(m^2))
  (v %*% m)/(n * sqrt(sum(v^2)))
}

min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

approcheItemItem <- function(size)
{
  errorArray <- matrix(nrow = size, ncol = 2)
  
  for (i in 1:size)
  {
    wcos.voisins <- as.vector(cosinus.vm(t(m[,i]), m))
    
    n.voisins <- 20 + 1
    
    # Nombre de votes communs
    votes.communs <- colSums((m[,i] * m) > 0) 
    
    # Distances avec les autres films
    distance <- sqrt(colSums((m.na[,i] - m.na)^2, na.rm=T))
    
    # Calcul des voisins
    i.distance <- min.nindex(distance, n.voisins)
    
    # La distance est �gale � 0 si les utilisateurs n'ont aucun vote en commun
    votes.communs[i.distance]
    
    # On supprime les distances �gales � 0
    distance[votes.communs == 0] <- NA
    
    i.distance <- min.nindex(distance, n.voisins)
    votes.communs[i.distance]  
    
    # Calcul des votes avec l'algorithme item-item
    w <- wcos.voisins[i.distance]
    names(w) <- names(votes.communs[i.distance])
    w <- w[w!=i] 
    
    # indice temporaire
    ii <- i.distance   
    
    # on enl�ve le film � pr�dire
    ii <- ii[ii!=i]
    
    # on cr�er une sous-matrice des voisins
    m.voisins <- m.na[,ii]
    
    m.centre <- t(t(m.na) - (colMeans(m.na, na.rm=T)))
    m.centre0 <- m.centre                   
    m.centre0[is.na(m.centre)] <- 0
    
    W0 <- t(m[,ii]>0) * w     
    W <- as.matrix(t(W0) / colSums(W0))
    W[is.nan(W)] <- 0
    
    votes <- mean(m.na[,i], na.rm=T) + rowSums(m.centre0[,ii] * W)
    
    # Calcul de l'erreur absolue moyenne
    errorArray[i,1] <- mean(abs(votes - m.na[,i]), na.rm=T)
    # Calcul de l'erreur quadratique moyenne
    errorArray[i,2] <- sqrt(mean((votes- m.na[,i])^2, na.rm=T))
  }
  return(errorArray)
}

# ncol(m) : nombre de films
resultat <- approcheItemItem(ncol(m))

resultatMoyenne <- colMeans(resultat)

# On consid�re que la dimension id�ale est �gale � 14
# c'est la dimension que l'on a obtenue le plus souvent
matrice_resultat <- getValidationCroisee(14, m)

comparaison <- resultatMoyenne - matrice_resultat