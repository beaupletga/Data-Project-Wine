#!/usr/bin/Rscript
library(FactoMineR)
library(Rcmdr)#je sais pas si elle est necessaire


smp <- read.csv("winequality-red.csv",header=TRUE,sep=",")



(smp[,1])->fixed_acid_col
(smp[,12])->quality_col


as.numeric(unlist(smp[,2]))->volatile_acid_col
as.numeric(unlist(smp[,3]))->citric_acid_col
as.numeric(unlist(smp[,4]))->residual_sugar_col
as.numeric(unlist(smp[,5]))->chlorides_col
as.numeric(unlist(smp[,6]))->free_sulfur_dioxide_col
as.numeric(unlist(smp[,7]))->total_sulfur_dioxide_col
as.numeric(unlist(smp[,8]))->density_col
as.numeric(unlist(smp[,9]))->ph_col
as.numeric(unlist(smp[,10]))->sulphates_col
as.numeric(unlist(smp[,11]))->alcohol_col

a_matrix=matrix(c(1:30),nrow=10,ncol=3,byrow = TRUE)
quality_list=c("Mauvaise","Moyenne","Bonne")
for(i in 1:3)
{
  indice_list=which(smp[,12]==quality_list[i])#liste d'indice qui respecte la condition
  for(j in 2:11)
  {
    b=c()
    for(k in 1:length(indice_list))
    {
      b[k]=smp[k,j]
    }
    a_matrix[j-1,i]=median(b)#on inverse lignes et colonnes car dans le bar plot les variables sont desormais les lignes et non les colonnes
  }
}


##########################################################################################
#Analyse Univariée

#L'essentiel des vins de l'echantillons sont de qualité moyenne
pct <- round(summary(quality_col)/sum(summary(quality_col))*100)
pie(summary(quality_col),labels=paste(names(summary(quality_col)),pct,"%",sep=" "),main="Quality Repartition")

#La repartition de l'acidité fixée n'est pas homogène (faible 51%)
pct <- round(summary(fixed_acid_col)/sum(summary(fixed_acid_col))*100)
pie(summary(fixed_acid_col),labels=paste(names(summary(fixed_acid_col)),pct,"%",sep=" "),main="Fixed acidity Repartition")

hist(alcohol_col,xlab="Degrés d'alcool",main="Histo_alcool.pdf")
#Pas très utile mais sert à montrer un histogramme tout con



#On voit desormais que 3 variables sont dans le meme ordre de grandeur (le dixieme)
tab=cbind(volatile_acid_col,sulphates_col,citric_acid_col)
boxplot(tab,outline=FALSE,range=0.5)

#L'ordre de grandeur de la présence de chlorides est le centieme
boxplot(chlorides_col,outline=FALSE,range=0.5)

#Tandis que l'odre de variation de la densité est du millieme
boxplot(density_col,outline=FALSE,range=0.5)

#On peut donc conclure en disant que l'on ne peut pas comparer toutes les données de la meme manière car elles ont toutes une echelles completement différentes


#########################################################################################
#Analyse bivariée

#On ne voit pas de réelle tendance entre le niveau d'alcool et la qualité du vin
barplot(a_matrix[10,],names.arg=c("Mauvaise", "Moyenne", "Bonne"),xlab="Qualité")

#En revanche on peut croire qu'il y a une relation entre la quantité de free sulfure dioxide et la qualité du vin
barplot(a_matrix[5,],names.arg=c("Mauvaise", "Moyenne", "Bonne"),xlab="Qualité")


#On utilise les coefficients de correlations de pearson pour deceler une correlation entre des variables
#ce coefficient permet de remarquer la présence d'une relation lineaire

#On s'apercoit que l'acididité volatile et la quantité acide citric sont liées négativement
#Par ailleurs, l'acide citrique et le ph sont aussi liés négativement

#Finalement le free sulfure de dioxidet la quantité totale de sulfure de dioxide sont liées positivement
#ce dernier résultat est logique puisque la quantité totale inclut la quantité de free

for(i in 2:11)
{
  for(j in 2:11)
  {
    if(i!=j)
    {
      #if(cor(smp[,i],smp[,j], method="pearson")>(0.5) || cor(smp[,i],smp[,j], method="pearson")<(-0.5))
      #print(paste(cor(smp[,i],smp[,j], method="pearson"),i,j,sep=" "))
    }
  }
}


#Faisons maintenant le test de spearman pour deceler l'existence d'une relation monotone (croissante ou decroissante)

#Les relations entre les variables sont renforcées par le teste de spearman
for(i in 2:11)
{
  for(j in 2:11)
  {
    if(i!=j)
    {
      if(cor(smp[,i],smp[,j], method="spearman")>(0.5) || cor(smp[,i],smp[,j], method="spearman")<(-0.5))
      print(paste(cor(smp[,i],smp[,j], method="spearman"),i,j,sep=" "))
    }
  }
}

smp_frame=data.frame(smp)


#On trouve un p tres proche de 0, cela implique que les variable sont corrélé de rho

#On remarque la une croissance monotone entre la quantité de free sulfure dioxide et la quantité total de sulfure
#ce résultat est parfaitement normal
#On retrouve une fonction croissante montone
plot(total.sulfur.dioxide~free.sulfur.dioxide,data=smp_frame)
abline(lm(total.sulfur.dioxide~free.sulfur.dioxide, data=smp_frame), col='red')
#On voit clairement que l'on a une courbe de tendance croissante monotone


#Voyons maintenant avec le ph et l'acide citrique
#On discerne une tenance décroissante monotone
plot(pH~citric.acid,data=smp_frame)
abline(lm(pH ~ citric.acid, data=smp_frame), col='red')
#On voit clairement que l'on a une courbe de tendance décroissante de maniere montone



##########################################################################################"
#Analyse Mutlivariée


#Profil lignes
barplot(prop.table(a_matrix,margin=2),names.arg=c("Mauvaise", "Moyenne", "Bonne"),xlab="Qualité",main="Repartition des composants",width=c(0.1,0.1,0.1),legend = names(smp[,2:11]))


#Profil colonnes
barplot(prop.table(t(a_matrix),margin=2),names.arg=c("vol acid","citric acid","resi sugar","chlorides","fs dioxide","ts dioxide","density","ph","sulphates","alcohol"),xlab="Variables",main="Qualité du vin en fonction des Variables")

#Melange de toutes les variables
res.pca = PCA(smp[1:100,2:12],scale.unit=TRUE,ncp=5,quali.sup=11, graph=T)
plot.PCA(res.pca, axes=c(1, 2), choix="ind",habillage=11)
#En ne prenant que les 100 premiers tests, on s'apercoit que les vins de bonne qualité sont clairement dispercé au sein des vins moyens
#Neanmoins cles vins de bonne qualité sont globalement au sud ouest du graphe


# en analysant le "Variables factor map (PCA)", on en déduit que :
#l'axe 1 est caractérisé par : acid.citric, sulphates et qui sont opposée au sulphates
#l'axe 2 est caractérisé par :residualt sugar, free sulphure dioxide


dimdesc(res.pca, axes=c(1,2))
#Notre analyse précédente est confirmée par cette analyse


#il faut ensuite que l'on relie les deux graphes pour conclure




#Analyse factorielle (je suis pas sur qu'on l'ai vu en cours)
res= CA(a_matrix)#On remarque que la quantité totale de sulfure de dioxide est determinante pour la production d'un bon vin.





#############################################################################################
#Autre
skewness <-function(colonne)
{
  col_length=length(colonne)
  col_average=mean(colonne)
  somme=0
  for(i in colonne)
  {
    somme=somme+(i-col_average)**3
  }
  somme=somme/(col_length*(sd(colonne)**3))
  return(somme)
}


kurtosis <-function(colonne)
{
  col_length=length(colonne)
  col_average=mean(colonne)
  somme=0
  for(i in colonne)
  {
    somme=somme+(i-col_average)**4
  }
  somme=somme/(col_length*(sd(colonne)**4))
  return(somme-3)
}
#names(summary(quality_col))




hist(volatile_acid_col,xlab="DMC",main="Histo_DMC.pdf")
boxplot(volatile_acid_col,horizontal=TRUE,col="brown",main="Repartition of Volatile Acidity",outline=FALSE)
#summary(dmc_col)
dmc_skewness=skewness(volatile_acid_col)
dmc_kurtosis=kurtosis(volatile_acid_col)


#
