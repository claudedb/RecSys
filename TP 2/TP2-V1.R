#TP 2 Code
#Par Claude Demers-Bélanger (1534217) et Mikael Perreault (1741869)

# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et vérifier que les packages nécessaires sont bien installés
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2)

m = read.table("http://www.groupes.polymtl.ca/log6308/Public/citeseer.rtable")
max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

min.nindex <- function(m, n=5) {
  i <- order(m, decreasing=FALSE)
  return(i[1:n])
}
cosinus.vm <- function(v,m) {
  n <- sqrt(colSums(m^2)); a=(v %*% m)/(n * sqrt(sum(v^2))) 
  return(a)
  }

# Question 1 --------------------------------------------------------------
doc.id.q1<-'422908'
q2.id=747
#1a) On effectue des recommandations pour l'espace des
#références de premier niveau. 


#Algorithme page rank: on fixe d'abord un damping factor 
#
d<-0.85
#La matrice m est de la forme suivante : l'article de la 
#ligne i cite l'article de la ligne j si l'élément (i,j)=1
#Ainsi, puisque CDI représente le nombre de références de 
#la page i, on doit faire un rowSums.
CDi<-rowSums(m)+1
n<-dim(m)[1]
PageRank<-rep(1,n)
erreur<-1
itt<-1

#On applique l'algorithme PageRank de manière itérative,
#jusqu'à ce qu'on atteigne une erreur quadratique 
#différentielle inférieure à 10e-5. 
while(erreur>=0.00001)
{
  r<-(1-d)+(d*(t(as.matrix(m))%*%(PageRank/CDi)))
  erreur<-sqrt(sum((r-PageRank)^2));
  PageRank<-r
  itt<-itt+1
}

#Pour avoir les recommandations possibles, on prend le 
#vecteur des références de l'article visé et on le
#multiplie par le vecteur PageRank. On recommande ensuite
#les 10 plus grands PageRank. 
references.PageRank<-m[doc.id.q1,]*PageRank
recommendations.references<-
  references.PageRank[order(-references.PageRank)][1:10]

#Voici le output voulu
colnames(recommendations.references)


#1b) On étudie maintenant l'espace des références de degré
#2, c'est-à-dire, les références ainsi que les références
#des références. Voici la matrice de l'espace augmenté :  

m.prime=m+as.matrix(m)%*%as.matrix(m)

#Puisque le but d'une matrice d'adjacence est d'exprimer la
#présence ou l'absence d'un lien, on fixe à 1 les éléments 
#dont la valeur est supérieure à 1
m.prime[m.prime>=1]=1


#Pour obtenir l'espace réduit, on doit parcourir les lignes
#de la matrice m.prime et pour chaque valeur i nulle, on doit
#supprimer la colonne et la ligne i. 
id.zero=which(m.prime[doc.id.q1,]==0 & 1:1090!=q2.id)
m.deg2=m.prime[-id.zero,-id.zero]
n<-dim(m.deg2)[1]
CDi.deg2=rowSums(m.deg2)+1
PageRank.deg2=rep(1,n)
erreur.deg2<-1
itt.deg2<-1


#On réapplique l'algorithme Page Rank avec la matrice m.deg2
#comprenant l'espace réduit
while(erreur.deg2>=0.00001)
{
  r.deg2<-(1-d)+(d*(t(as.matrix(m.deg2))%*%(PageRank.deg2/CDi.deg2)))
  erreur.deg2<-sqrt(sum((r.deg2-PageRank.deg2)^2));
  PageRank.deg2<-r.deg2
  itt.deg2<-itt.deg2+1
}


#On retrouve par la suite les recommandations comme en 1a) 

references.PageRank.deg2<-m.deg2[doc.id.q1,]*PageRank.deg2
recommendations.references.deg2<-
  references.PageRank.deg2[order(-references.PageRank.deg2)][1:10]

#Voici le output voulu 
colnames(recommendations.references.deg2)


# Question 2 --------------------------------------------------------------
#Vecteur qui correspond a l'article recherché
doc.id.q2<-paste("X",doc.id.q1,sep="")


#Le but ici est de calculer le cosinus entre le vecteur 
#de références de l'article visé et ceux des autres 
#articles de la matrice d'adjacence. C'est pour cette 
#raison que l'on transpose la matrice, puisque le cosinus
#est calculé sur les colonnes par défaut. 
mat.t=t(as.matrix(m))

vecteur.q2<-mat.t[,q2.id]

cosinus.q2<-cosinus.vm(vecteur.q2,mat.t)



#On retourne par la suite les 10 plus grands cosinus 
#pour les recommandations
recommendations.ii.cos<-cosinus.q2[order(-cosinus.q2)][2:11]

recommendations.ii.cos.id=order(-cosinus.q2)[2:11]

#Voici le output voulu

colnames(m[recommendations.ii.cos.id])


#Discussion sur les résultats 

#On observe que les résultats des trois approches sont 
#différents mais similaires et c'est ce qui était attendu. 
#Pour le cosinus, il est possible que des recommandations 
#ne soit pas dans le même domaine des références de degré 
#1 et 2. En effet, deux articles peuvent citer les mêmes 
#citations sans pour autant s'entre-citer. Par contre, en 
#raison de la similarité des recommendations avec l'approche
#PageRank où on recommandait seulement des références de 
#l'article visé, on conclut que si un article possède des 
#références similaires à l'article visé selon le cosinus,
#les chances qu'il figure dans les références de ce dernier 
#sont élevées. 



# Question 3 --------------------------------------------------------------
#Pour la validation croisée, nous allons procéder d'une
#manière analogue au processus employé dans l'article 
#de McNee intitulé "On the recommending of citations for
#research papers". L'équipe utilisait les référence de
#degré 1 comme données de test et adoptait une approche
#"leave one out". En d'autres mots, on retire tour à
#tour une citation dans l'ensemble des références degré 1
#de l'article visé et on tente de le recommander avec la 
#mesure du cosinus. On choisira dans ce TP d'utiliser 
#seulement les citations de degré 1 pour pouvoir comparer 
#nos résultats avec ceux de l'article.

#Tous les id de citations de l'article
id.un.cv=which(m[q2.id,]!=0)



recommendations.cv=matrix(0,nrow=25,ncol=17)
recommendations.cv.id=matrix(0,nrow=25,ncol=17)
data.hist=setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("id", "rank"))
k=1
s=1

#On applique l'approche "leave one out" et on effectue les 
#recommandations avec le cosinus. On considère qu'une 
#citation est recommandée si elle figure dans le top 25 
#des plus hauts cosinus. 
for (i in id.un.cv) {
  mat.cv=mat.t
  mat.cv[i,q2.id]=0
  cosinus.cv=cosinus.vm(vecteur.q2,mat.cv)
  recommendations.cv[,k]=cosinus.cv[order(-cosinus.cv)][2:26]
  recommendations.cv.id[,k]=order(-cosinus.cv)[2:26]
  if (any(recommendations.cv.id[,k]==i)){
    data.hist[s,]=c(i,which(recommendations.cv.id[,k]==i))
    s=s+1
  }
  
  k=k+1
}


#On regroupe les données pour le graphique: top5=1,
#top10=2, top15=3, top20=4, top25=5
data.hist.graph=data.hist
data.hist.graph[which(data.hist[,2]<=5),2]=1
data.hist.graph[which(data.hist[,2]>5&data.hist[,2]<=10),2]=2
data.hist.graph[which(data.hist[,2]>10&data.hist[,2]<=15),2]=3
data.hist.graph[which(data.hist[,2]>15&data.hist[,2]<=20),2]=4
data.hist.graph[which(data.hist[,2]>20&data.hist[,2]<=25),2]=5




longueur.precision=dim(data.hist)[1]
longueur.recall=length(id.un.cv)
pourcentage.precision=rep(1,5)
pourcentage.recall=rep(1,5)


#CALCUL ET FORMATTAGE DES DONNÉES DE GRAPH
#Similairement à l'article de McNee, on construit 2 
#graphiques qui seront décrits plus loin. Pour construire,
#ces graphiques, on a besoin des pourcentages cumulatifs 
#des rangs. En d'autres mots, on a besoin de savoir le 
#pourcentage des articles figurant dans le top 5, 10, etc.
#On parle de pourcentage cumulatif, puisqu'un catégorie de 
#rang contient les recommandations dans sa catégorie, ainsi 
#que toutes les recommandations dans les catégories 
#inférieures. Par exemple, la catégorie "top 15", contient 
#les recommandations figurant entre les positions 
#11 et 15 ainsi que toutes les recommandations dans le
#top 10. 

pourcentage.precision[1]=(sum(data.hist.graph[,2]==1)/longueur.precision)*100
pourcentage.recall[1]=(sum(data.hist.graph[,2]==1)/longueur.recall)*100
for (j in c(2:5)){
  pourcentage.precision[j]=((sum(data.hist.graph[,2]==j)
                             /longueur.precision)*100)+pourcentage.precision[j-1]
  pourcentage.recall[j]=((sum(data.hist.graph[,2]==j)
                          /longueur.recall)*100)+pourcentage.recall[j-1]
}


rank.graph=c("5","10","15","20","25")

par(mfrow=c(1,2))


#Construction du graphique 1 
#Description du graphique 1: Il s'agit d'une mesure de 
#précision: Pour toutes citations qui ont été recommandées,
#on affiche quel pourcentage de celles-ci figure dans quel 
#rang. Donc, comme une citation est considérée comme étant 
#recommandées si elle figure dans le top 25, la catégorie 
#"Top 25" devrait contenir 100% des articles, puisque les 
#pourcentages sont cumulatifs. 


barplot(names=rank.graph,pourcentage.precision,
        col="blue",main="Graphique 1: Pourcentage des citations 
        recommandées dans le top 5, 10, 15, 20, 25")
mtext(side=1,"Rang",line=2.5)
mtext(side=2,"Pourcentage",line=2.5)


#Construction du graphique 2 
#Description du graphique 2 : Il s'agit d'une mesure de 
#rappel: Pour toutes les citations retirées, le 
#pourcentage de celles-ci qui ont été recommandées en 
#fonction de leur rang respectif. La catégorie "Top 25"
#ici ne devrait pas nous donner 100%, mais bien une mesure
#de la couverture de l'algorithme. 


barplot(names=rank.graph,pourcentage.recall,
        col="red",main="Graphique 2: Parmi les citations retirées, 
        celles qui ont été recommandées en fonction de leur rang")
mtext(side=1,"Rang",line=2.5)
mtext(side=2,"Pourcentage",line=2.5)


#Discussion de la validation croisée 

#Puisque nous ne possédons pas de ground truth pour ce 
#problème, il est difficile d'évaluer la méthode et 
#de conclure si celle-ci est meilleure qu'une autre 
#(on ne peut pas calculer d'erreur quadratique). 
#Par contre, on peut évaluer si notre méthode donne 
#des résultats de même ordre de grandeur que ce qu'il est 
#obtenu dans l'article de McNee. Cependant, plusieurs 
#facteur sont à considérer avant de faire cette comparaison.
#Le plus important est la taille de l'échantillon: nous
#utilisons une base de données de 1090 articles alors que 
#l'article en utilise une de 186 000 articles. De plus,  
#on ne possède pas le seuil de recommandation pour l'équipe 
#de McNee (ils présentent les résultats jusqu'au top 40, mais
#en regardant leur graph de précision, on constate que le seuil
#est encore moins sévère, possiblement jusqu'au top 100) 
#alors que pour nous, la citation doit être dans le top 25 
# pour être recommandée afin de tenir compte 
#du plus petit échantillon. Pour ce qui est de la précision,
#nos citations recommandées sont à 33.3% dans le top 5, 
#tandis que 18% des citations recommandées de l'approche
#item-item du papier sont dans le top 1 (on ne possède
#aucune citation dans le top 1). Si on compare 
#les deux top 10, on a une précision de 55.6% alors que
#la précision du papier est de 52%. On a une précision
#de 100% pour le top 25 alors que le papier en a une de 
#75% pour le top 40. Alors, pour la précison, il semblerait
# que notre méthode soit moins performante que l'article 
#pour les meilleurs rang et plus performante pour les rangs 
#éloignés. 

#Pour ce qui est du rappel, nous avons une couverture 
#maximale (pourcentage dans le top 25) de 52.9%, alors que
#l'article a une couverture dans le top 40 de 39%. 

#Bref, même si plusieurs facteurs nous empêche de comparer
#les résultats entre notre méthode et celle de l'article 
#de manière rigoureuse à cause des nombreuses variables 
#importantes des expériences, on peut au moins dire que 
#l'ordre de grandeur de nos résultats de  précision et 
#rappel ont du sens. 



## Outputs -----------------------------------------------------------------
#Pour les outputs, on ne pouvait répartir le code sur 
#plusieurs lignes, car cela détruisait l'affichage dans 
#la console.

print("Question 1a)")
print("Voici les recommandations pour l'approche  PageRank généralisée avec les références de degré 1:")
print(colnames(recommendations.references))
print("Question 1b)")
print("Voici les recommandations pour l'approche PageRank ciblée avec les références de degré 1 et 2:")
print(colnames(recommendations.references.deg2))
print("Question 2)")
print("Voci les recommendations pour l'approche de similarité par cosinus:")
print(colnames(m[recommendations.ii.cos.id]))
print("Et voici les cosinus associées à ces recommandations:")
print(recommendations.ii.cos)
print("Question 3)")
print("Voici les pourcentages cumulatifs du graphique de précision (1) et reliés, en ordre, au top 5, 10, 15, 20, 25: ")
print(pourcentage.precision)
print("Voici les pourcentages cumulatifs du graphique de rappel (2) et reliés, en ordre, au top 5, 10, 15, 20, 25: ")
print(pourcentage.recall)
