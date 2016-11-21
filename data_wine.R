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


pdf("Quality.pdf")
pct <- round(summary(quality_col)/sum(summary(quality_col))*100)
pie(summary(quality_col),labels=paste(names(summary(quality_col)),pct,"%",sep=" "),main="Quality Repartition")
dev.off()


pdf("Fixed acidity.pdf")
pct <- round(summary(fixed_acid_col)/sum(summary(fixed_acid_col))*100)
pie(summary(fixed_acid_col),labels=paste(names(summary(fixed_acid_col)),pct,"%",sep=" "),main="Fixed acidity Repartition")
dev.off()


pdf("Histo Volatile Acidity.pdf")
hist(volatile_acid_col,xlab="DMC",main="Histo_DMC.pdf")
boxplot(volatile_acid_col,horizontal=TRUE,col="brown",main="Repartition of Volatile Acidity",outline=FALSE)
#summary(dmc_col)
dmc_skewness=skewness(volatile_acid_col)
dmc_kurtosis=kurtosis(volatile_acid_col)
dev.off()


###############################################################################
#ACP


#Melange de toutes les variables
#res.pca = PCA(smp[,2:11],scale.unit=TRUE, ncp=5, graph=T)


#Permet de distinguer les trois types de vins
#plot.PCA(res.pca,axes=c(1:2),choix="ind",habillage=11)



#Permet d'interpreter les axes. Ex: que l'axe 1 est fortement influencé par la citric.acid
#dimdesc(res.pca, axes=c(1,2))


#plot(sort(volatile_acid_col),alcohol_col)

#barplot(smp, col="darkgreen")



summary(smp)



















#
