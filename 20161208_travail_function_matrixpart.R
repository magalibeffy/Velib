###################################################
###### PGM AVEC UN ASPECT MATRICIEL 
###### APPLICATION DE FDA EN MATRICIEL
###### PLUTOT QU AVEC DATA TABLE DIRECTEMENT
###### LES DONNEES ONT ETE FINALEMENT CYLINDREES
###################################################
###################################################
install.packages('fda')
library(fda)
install.packages('data.table')
library(data.table)
install.packages('ggplot2')
library(ggplot2)
install.packages("glmnet")
library(glmnet)

require(dplyr)
install.packages('kml')
require(kml)
install.packages('reshape2')
require(reshape2)

install.packages('gridExtra')
library(gridExtra)

install.packages('lattice')
library(lattice)

path_file = "C:/UserM/ML/201611_Project/Data/"
##path_file = "D:/Utilisateurs/mbeffy/Documents/Recherche/Formation/25112016_Project/Data/"
load(paste0(path_file,"2016.oct.RData"))
##load(paste0(path_file,"2016.aoseptoct.RData"))
# m'a bien gard? la structure souhait?e
str(essai)
print(essai[,summary(decompte)])


## 
setkey(essai,Index1,download_date)


###################### ETAPE 0. PREPARATION DES SIGNAUX SOUS FORME MATRICIELLE EN LIGNE ########################################
###################### DIFFERENTE DE MON APPROCHE DATA TABLE PRECEDENTE                 ########################################
################################################################################################################################
# --> travail sur des donn?es mises en forme matricielle
weekday <- c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")
essaiJ <- list()
for (i in 1:length(weekday)){
  essaiJ[[i]] <- copy(essai[Daym==weekday[i],])
}
essaiL <- essaiJ[[1]]
essaiL <- essaiL[!is.na(tx.occup),]

# pour ne pas tout modifier et travailler tout de meme que sur le lundi
sauv.essai <- essai
essai <- essaiL
setkey(essai,Index1,download_date)


#CREATION DE LA MATRICE DES SIGNAUX TEMPORELS EN LIGNE SEULEMENT

## creation de variables auxiliaires sur les signaux
essai[,mean.tx.occup:=mean(tx.occup),by=Index1]
essai[,sd.tx.occup:=sd(tx.occup),by=Index1]
essai[,max.tx.occup:=max(tx.occup),by=Index1]
essai[,min.tx.occup:=min(tx.occup),by=Index1]
essai[,extreme.v.tx.occup:=(abs(tx.occup)>mean.tx.occup+2*sd.tx.occup),by=Index1]
essai[,extreme.tx.occup:=sum(extreme.v.tx.occup),by=Index1][,extreme.v.tx.occup:=NULL]

essaiT <- essai[,comptage:=seq(1,.N,1),by=Index1]
essaiT <- essaiT[,.(Index1,comptage,tx.occup)]
essaiT <- melt(essaiT,id=c("Index1","comptage"))
essaiT <- acast(essaiT, Index1 ~ comptage ~ variable)
coef.e1 <-as.data.table(rownames(essaiT))
coef.e1 <- coef.e1[,Index1:=V1][,V1:=NULL]
coef.e1 <- cbind(coef.e1,as.data.table(essaiT[,,1]))
nom <- names(coef.e1)
test <- nom[2:73]
test <- as.character(c(nom[1],lapply(test, function(x) paste0("tx.occup.",as.character(x)))))
setnames(coef.e1,test)
View(coef.e1)
DT.Matrix <- coef.e1
rm(coef.e1)
coef.e1 <- unique(essai[,.(Index1,mean.tx.occup,sd.tx.occup,max.tx.occup,min.tx.occup,extreme.tx.occup)])
setkey(coef.e1,Index1)
setkey(DT.Matrix,Index1)
DT.Matrix <- DT.Matrix[coef.e1]
DT.Matrix <- DT.Matrix[apply(DT.Matrix,1,function(x) !any(is.na(x))),]
rm(coef.e1)


###################### ETAPE 1. REGRESSION PENALISEE DES SIGNAUX CYLINDRES SUR LA BASE DE SPLINES #######################
#########################################################################################################################
X <- seq(0,24*60*60,20*60)
X <- X[-73]/86400
argvals <- X
n=length(X) ##nombre de points d'observation
N=length(DT.Matrix) ## nombre de signaux

norder <- 4
nbasis <- length(X)+4-2
rg <- c(min(X),max(X))
bbasis <- create.bspline.basis(rg,nbasis,norder)
lambda <- 10^(-5)
dpenal <- 2
fdParobj <- fdPar(fdobj=bbasis, Lfdobj=dpenal, lambda=lambda)  ## penalisation par D2 et lambda=0.01
y <- DT.Matrix[,2:73]
y <- as.matrix(y)
y <- t(y)
smoothsignal <- smooth.basis(argvals,y,fdParobj)
xfd <- smoothsignal$fd
df <- smoothsignal$df
gcv <- smoothsignal$gcv
RMSE <- sqrt(mean((eval.fd(argvals,xfd)-y)^2))
# Display the results, note that a gcv value is returned for EACH curve,
# and therefore that a mean gcv result is reported
cat(round(c(df,RMSE,mean(gcv)),3),"\n")
# the fits are plotted interactively by plotfit.fd ... click to advance
# plot fit commenté sinon tu dois appuyer sur entree chaque fois
                                      ##### plotfit.fd(y, argvals, xfd)
# Repeat these results for a range of log10(lambda) values
loglamout = plotGCVRMSE.fd(-12, -1, 0.25, argvals, y, fdParobj)
# donc on voit que plus plat entre -8 et -4
loglamout = plotGCVRMSE.fd(-12, -4, 0.1, argvals, y, fdParobj)
loglamout[which.min(loglamout[,4]),]
loglamout[(loglamout[,1]==-5.0),]
loglamout[(loglamout[,1]==-4.9),]
loglamout[(loglamout[,1]==-4.0),]
# ---> ETAPE 1.2. CHOIX DU NOMBRE DE SPLINES A RETENIR EN FONCTION DE GCVE
# Choix plutot de 14.71 splines pour reconstituer les signaux (trade off entre le nombre et le min car 37 trop ?lev?)

# ---> ETAPE 1.3. REESTIMATION AVEC CE NOMBRE DE SPLINES POUR AVOIR LES BONS COEFF DE LISSAGE
lambda.chosen <- 10^(-5) ##exp(loglamout[(loglamout[,1]==-5),1])
#nbasis.chosen <-round(loglamout[(loglamout[,1]==-5),2])
nbasis.chosen <- 18 #round(loglamout[which.min(loglamout[,4]),2])

## je refais tourner avec nbasischosen splines
X <- seq(0,24*60*60,20*60)
X <- X[-73]/86400
argvals <- X
n=length(X) ##nombre de points d'observation
N=length(DT.Matrix) ## nombre de signaux

norder <- 4
nbasis <- nbasis.chosen
rg <- c(min(X),max(X))
bbasis <- create.bspline.basis(rg,nbasis,norder)
lambda <- lambda.chosen
dpenal <- 2
fdParobj <- fdPar(fdobj=bbasis, Lfdobj=dpenal, lambda=lambda)  ## penalisation par D2 et lambda=0.01

y <- DT.Matrix[,2:73]
y <- as.matrix(y)
y <- t(y)
smoothsignal <- smooth.basis(argvals,y,fdParobj)
xfd <- smoothsignal$fd
df  <- smoothsignal$df
gcv <- smoothsignal$gcv
yspline <- eval.fd(argvals,xfd) ## 72,3579 --> a garder!!!!
RMSE <- sqrt(mean((yspline-y)^2))

yspline <- cbind(DT.Matrix[,1],t(yspline)) ### 3579,73
nom <- names(yspline)
test <- nom[2:73]
test <- as.character(c(nom[1],lapply(test, function(x) paste0("Spline.tx.occup.",as.character(substr(x,2,stop=5))))))
setnames(yspline,test)
DTS <- melt(as.data.table(yspline), id=1) #
DTS[,yspline:=value]
DTS[,value:=NULL]
setkey(DTS,Index1,variable)
DTS[,comptage:=seq(1,.N,1),by=Index1][,variable:=NULL]
setkey(DTS,Index1,comptage)




coef.fd <- cbind(DT.Matrix[,1],t(smoothsignal$fd$coefs))
dim(coef.fd)
coef.fd <- as.data.table(coef.fd,key=Index1)
cat(round(c(df,RMSE,mean(gcv)),3),"\n")
                                     ##### plotfit.fd(y, argvals, xfd)

## je trie les signaux par gcve croissant j'en fais des classes par quartile puis j'applique les kmeans
gcv <- cbind(DT.Matrix[,1],gcv)
gcv <-  as.data.table(gcv,key=Index1)
gcv <- gcv[, quartile.gcv:=as.character(ntile(gcv, 4))][,gcv:=NULL]
gcv[,table(quartile.gcv)]


###################### ETAPE 2. DETERMINATION DES FONCTIONS PROPRES SUR LES SIGNAUX LISSES ##############################
#########################################################################################################################
pcalist    = pca.fd(smoothsignal$fd,5,centerfns = FALSE)
rotpcalist = varmx.pca.fd(pcalist)
#####                                      plot.pca.fd(rotpcalist)
print(pcalist$values)
# part expliquée par les 5 premieres 98,5%
100*sum(pcalist$values[1:5])/sum(pcalist$values)
#####                                      plot.pca.fd(pcalist)
# et si je regresse sur ces 5 fonctions vecteurs propres 


#### ----  pb cela ne regresse pas sur mes vecteurs propres mais à nouveau sur mes 18 coefficients
#y <- DT.Matrix[,2:73]
#y <- as.matrix(y)
##y <- t(y)
###smoothsignal <- smooth.basis(argvals,y,pcalist$harmonics)
##xfd <- smoothsignal$fd
### pb car il repart sur 18 coefficients et donc non sur la base de fonctions propres determines
#df  <- smoothsignal$df
#gcv <- smoothsignal$gcv
#RMSE <- sqrt(mean((eval.fd(argvals,xfd)-y)^2))
#test.coh <- xfd$coefs ## [18,3579]
#test.coh <- t(pcalist$harmonics$coefs) %*% test.coh ### [5,18]*[18,3759]
#TEST <- (test.coh == t(pcalist$scores))


#### ---- 
dim(coef.fd)
coef.fd <- pcalist$scores
# aevc les variables en plus coef.fd <- cbind(DT.Matrix[,c(1,74:78)],coef.fd)
coef.fd <- cbind(DT.Matrix[,1],coef.fd)
coef.fd <- as.data.table(coef.fd,key=Index1)

tmp1 <- getbasismatrix(X,bbasis,nderiv=0,returnMatrix = TRUE) ## size [72,18]
tmp2 <- pcalist$scores                                        ## size [3579,5]
tmp3 <- pcalist$harmonics$coefs                              ## size [18,5]
## donc si je veux la valeur du signal estime par regression sur 5 signaux seulement 
## bon ce n'est pas top en terme d'approximation mais surement meilleur que si slt 5 splines basiques
yhat <- tmp2 %*% t(tmp3) %*% t(tmp1)  ## me renverra [3579,72]
rm(tmp1)
rm(tmp2)
rm(tmp3)
yhat <- cbind(DT.Matrix[,1],yhat)
nom <- names(yhat)
test <- nom[2:73]
test <- as.character(c(nom[1],lapply(test, function(x) paste0("FPCA.tx.occup.",as.character(substr(x,2,stop=5))))))
setnames(yhat,test)
DT <- melt(as.data.table(yhat), id=1) #
DT[,yPCA:=value]
DT[,value:=NULL]
setkey(DT,Index1,variable)
DT[,comptage:=seq(1,.N,1),by=Index1][,variable:=NULL]
setkey(DT,Index1,comptage)
DT <- DT[DTS]


setkey(essai,Index1,comptage)
essai <- essai[DT]


## forte réduction de la dimension de 15 A 5


###################### ETAPE 3. DETERMINATION DES CLASSES EN FONCTION DES AXES FPCA ET/OU D AUTRES CARACTERISTIQUES STATION ##############################
##########################################################################################################################################################

coef.fd.t <- coef.fd
don <- coef.fd.t[,-1]

## jsute avec les fonctions propres 5 coefficients
##don <- coef.fd
##maintenant que je n'ai que 5 coefficients je peux rajouter des variables explicatives autre sur les stations
## a priori capacité de la station // moyenne du taux occupation sur la journee // variabilité de ce taux d'occupation 
## // min max ou rapport entre les deux // erreur de mesure lors du lissage 



res=vector("numeric", 19)
for(k in 2:20){
  kmeans.k=kmeans(don, k,iter.max = 100,nstart=10)
  res[k-1]=kmeans.k$tot.withinss/kmeans.k$totss
}
plot(2:20, res, type="b")

gp5  <- kmeans(don,5,iter.max = 100,nstart=10)
gp7  <- kmeans(don,7,iter.max = 100,nstart=10)
gp10 <- kmeans(don,10,iter.max = 100,nstart=10)

tmp <- cbind(coef.fd.t[,1],gp5$cluster,gp7$cluster,gp10$cluster)
names(tmp)[2:4] <- c("cluster5","cluster7","cluster10")
tmpdt <- data.table(tmp,key='Index1')
rm(tmp)

essai_t <- essai[(Index1 %in% tmpdt[,Index1]),]
essai_t <- essai_t[tmpdt]
setkey(essai_t,Index1,comptage,cluster10)
#mynames = c("moybruteC10","moySplineC10","moyFPCAC10")
#tmp1 <- essai_t[,(mynames) := list(mean(tx.occup),mean(yspline),mean(yPCA)),by=.(Freq_m,cluster10)][,.(Freq_m,cluster10,moybruteC10,moySplineC10,moyFPCAC10)][,Index1:="MoyC"]
tmp1 <- essai_t[, m_tx.occup:= mean(tx.occup),by=.(comptage,cluster10)][,.(comptage,cluster10,m_tx.occup)][,Index1:="MoyBC"]
tmp1 <- tmp1[,tx.occup:=m_tx.occup][,m_tx.occup:=NULL]
setkey(tmp1,cluster10,comptage)
tmp1 <- unique(tmp1)
tmp2 <- essai_t[, m_yspline:= mean(yspline),by=.(comptage,cluster10)][,.(comptage,cluster10,m_yspline)][,Index1:="MoySC"]
tmp2 <- tmp2[,yspline:=m_yspline][,m_yspline:=NULL]
setkey(tmp2,cluster10,comptage)
tmp2 <- unique(tmp2)
tmp3 <- essai_t[, m_yPCA:= mean(yPCA),by=.(comptage,cluster10)][,.(comptage,cluster10,m_yPCA)][,Index1:="MoyPCC"]
tmp3 <- tmp3[,yPCA:=m_yPCA][,m_yPCA:=NULL]
setkey(tmp3,cluster10,comptage)
tmp3 <- unique(tmp3)

##essai_t <- rbind(essai_t,tmp,fill=TRUE)


### avec 10 clusters
essaiL5.s1   <- essai_t[(cluster10==1),]
presentL5.s1 <- sample(unique(essaiL5.s1$Index1),5)
essaiL5.s1   <- essaiL5.s1[(Index1 %in% presentL5.s1),.(Index1,comptage,cluster10,yspline)]
essaiL5.s1   <- rbind(essaiL5.s1,tmp2[(cluster10==1),])
gp           <- ggplot(essaiL5.s1, aes(x = comptage, y = yspline,colors=Index1)) + geom_line() + geom_point(data=tmp2[(cluster10==1),], aes(x=comptage,y=yspline),color="red")
gp


for (i in 1:10){
  essaiL5.s1 <- copy(essai_t[(cluster10==i)])
  presentL5.s1 <- sample(unique(essaiL5.s1$Index1),25)
  essaiL5.s1 <- copy(essaiL5.s1[(Index1 %in% presentL5.s1),])
  gp[[i]] <- ggplot(essaiL5.s1, aes(x = comptage, y = yspline,colors=Index1)) + geom_line() + geom_point(data=tmp2[(cluster10==i),], aes(x=comptage,y=yspline),color="red")
}
grid.arrange(gp[[1]], gp[[2]], gp[[3]], gp[[4]], gp[[5]],gp[[6]], gp[[7]], gp[[8]], gp[[9]], gp[[10]], ncol=3, nrow = 4)

## les cpa ne sont qu un outil apres on peut garder les courbes lisses

### avec 10 clusters
essaiL5.s1   <- essai_t[(cluster10==1),]
presentL5.s1 <- sample(unique(essaiL5.s1$Index1),5)
essaiL5.s1   <- essaiL5.s1[(Index1 %in% presentL5.s1),.(Index1,comptage,cluster10,yPCA)]
essaiL5.s1   <- rbind(essaiL5.s1,tmp3[(cluster10==1),])
gp           <- ggplot(essaiL5.s1, aes(x = comptage, y = yPCA,colors=Index1)) + geom_line() + geom_point(data=tmp3[(cluster10==1),], aes(x=comptage,y=yPCA),color="red")
gp


for (i in 1:10){
  essaiL5.s1 <- copy(essai_t[(cluster10==i)])
  presentL5.s1 <- sample(unique(essaiL5.s1$Index1),25)
  essaiL5.s1 <- copy(essaiL5.s1[(Index1 %in% presentL5.s1),])
  gp[[i]] <- ggplot(essaiL5.s1, aes(x = comptage, y = yPCA,colors=Index1)) + geom_line() + geom_point(data=tmp3[(cluster10==i),], aes(x=comptage,y=yPCA),color="red")
}
grid.arrange(gp[[1]], gp[[2]], gp[[3]], gp[[4]], gp[[5]],gp[[6]], gp[[7]], gp[[8]], gp[[9]], gp[[10]], ncol=3, nrow = 4)


## part de chaque classe
tmp$cluster10

## SI JE FAIS EN PLUSIEURS TEMPS


##########################################################################################
################## ESSAI DU PACKAGE KLM TROP LENT SI JE PRENDS LES 72 POINTS A VOIR
##########################################################################################
y <- DT.Matrix[,-1]
##y <- t(as.matrix(sample_n(y,100)))
y <- as.matrix(y)
#y <- y[apply(y,1,function(x) !any(is.na(x))),]
#y <- t(y) pour ce package il ne faut pas transposer
X <- seq(0,24*60*60,20*60)
X <- X[-73]/86400
# construction of the object only
cldSDQ <- cld(traj=y,idAll=DT.Matrix[,Index1],X)
kml(cldSDQ,c(5,7,9),3,toPlot='both')
plot(cldSDQ,4,parTraj=parTRAJ(col="clusters"))




#####################################################################################
#####################################################################################
#####################################################################################
## ANNEXES : FONCTIONS UTILITAIRES
#####################################################################################
#####################################################################################
#####################################################################################
# First, we define a little display function for showing how
# df, gcv and RMSE depend on the log10 smoothing parameter
plotGCVRMSE.fd = function(lamlow, lamhi, lamdel, argvals, y,
                          fdParobj, wtvec=NULL, fdnames=NULL, covariates=NULL) {
  loglamvec = seq(lamlow, lamhi, lamdel)
  loglamout = matrix(0,length(loglamvec),4)
  m = 0
  for (loglambda in loglamvec) {
    m = m + 1
    loglamout[m,1] = loglambda
    fdParobj$lambda = 10^(loglambda)
    smoothlist = smooth.basis(argvals, y, fdParobj, wtvec=wtvec,
                              fdnames=fdnames, covariates=covariates)
    xfd = smoothlist$fd # the curve smoothing the data
    loglamout[m,2] = smoothlist$df
    # degrees of freedom in the smoothing curve
    loglamout[m,3] = sqrt(mean((eval.fd(argvals, xfd) - y)^2))
    loglamout[m,4] = mean(smoothlist$gcv) # the mean of the N gcv values
  }
  cat("log10 lambda, deg. freedom, RMSE, gcv\n")
  for (i in 1:m) {
    cat(format(round(loglamout[i,],6)))
    cat("\n")
  }
  par(mfrow=c(3,1))
  plot(loglamvec, loglamout[,2], type="b")
  title("Degrees of freedom")
  plot(loglamvec, loglamout[,3], type="b")
  title("RMSE")
  plot(loglamvec, loglamout[,4], type="b")
  title("Mean gcv")
  par(mfrow=c(1,1))
  return(loglamout)
}
