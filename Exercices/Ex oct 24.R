# Pre Code ----------------------------------------------------------------

#On utilise ces deux lignes pour charger et vérifier que les packages nécessaires sont bien installés
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Matrix,data.table,tidyr,readr,dplyr,ggplot2)

m = read.table("http://www.groupes.polymtl.ca/log6308/Seances/MU/Data/unix.csv")
Q= read.table('http://www.groupes.polymtl.ca/log6308/Seances/MU/Data/qm.csv')

