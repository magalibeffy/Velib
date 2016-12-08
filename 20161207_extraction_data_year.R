####################### extraction des données de toute l'année ####################### 
## data extraction 
####################### ####################### ####################### ##############
install.packages("RCurl")
install.packages("curl")
install.packages("rjson")
install.packages("jsonlite")
install.packages("stringr", dependencies=TRUE)

install.packages("dplyr")
install.packages("data.table")
library(RCurl)
library(curl)
library(rjson)
library(ggplot2)
library(dplyr)
library(data.table)
library(stringr)

## chargement des données de l'année 2016

path_file = "C:/UserM/ML/201611_Project/Data/"
##path_file = "D:/Utilisateurs/mbeffy/Documents/Recherche/Formation/25112016_Project/Data/"

data.list <- c("data_all_Paris.jjson_2016-10-01-1475295963") ## ,"data_all_Paris.jjson_2016-09-01-1472703936","data_all_Paris.jjson_2016-11-01-1477977947")
names_v <- c("status","bike_stands","number","download_date","available_bike_stands","available_bikes")
ll <- length(data.list)
test2 <- list()

for (i in 1:ll){
  
  datab <- readLines(con = paste0(path_file,data.list[i]))
  test  <- as.array(datab)
  test <-  apply(test,1,jsonlite::fromJSON)

  test2 <- c(test2,lapply(test,function(df) df<- df[,names_v]))
}

test2 <- lapply(test2,function(df) dplyr::mutate(df,Jour = as.Date(as.POSIXct(download_date,origin="1970-01-01"))))
test2 <- lapply(test2,function(df) dplyr::mutate(df,Index1 = paste(as.character(Jour),as.character(number),sep="-")))

# nombre d'éléments d'une liste, length()
essai <- rbindlist(test2,use.names=FALSE,idcol=TRUE)
setkey(essai,Index1,download_date) # donc data table ordonnée par Jour station, si je veux l'indexer différemment je change la key
setkey(essai,Index1)
# si je voulais tracer maintenant l'utilisation d'une station par Index1 / .id ou Index1 / download_date

#names_v <- c("status","bike_stands","number","download_date","available_bike_stands","available_bikes")
essai <-essai[!is.na(available_bike_stands),]
essai <-essai[!is.na(available_bikes),]
essai <-essai[!is.na(bike_stands),]

# tracé de la première distribution pour les stations d'un même jour
# essai[,Freq := str_sub(as.character(as.POSIXct(download_date - first(download_date),origin="1970-01-01")),-8,-1),by=Index1]
# point de mesure en seconde du jour rapport ? [0,1]
essai[,Freq_m := (download_date - first(download_date))/86400,by=Index1]
essai[,decompte := .N,by=.(Jour,number)]#compte le nombre de mesures par jour et station
print(essai[,summary(decompte)])
print(essai[,sum(decompte<72)])

essai <- essai[(decompte>=72),]
essai[, I_Dec_st:=(sum((status=="CLOSED"))>=1),by=Index1]
print(essai[,summary(I_Dec_st)])
essai <- essai[(I_Dec_st==FALSE),]

print(essai[,summary(I_Dec_st)])
essai <- essai[,I_Dec_st:=NULL]
str(essai)

essai[,Daym:=weekdays(Jour)]
print(essai[,table(Daym)])

essai <- essai[(decompte==72),]

essai[,tx.occup:=available_bikes/bike_stands,by=Index1]
essai <- essai[(tx.occup<=1),]
essai <- essai[(tx.occup>=0),]
essai <- essai[!is.na(tx.occup),]

save(essai, file=paste0(path_file,"2016.oct.RData"), ascii=TRUE)

## si je veux garder seulement une mesure toutes les 20 minutes
temps <- seq(0,24*60*60,20*60)
temps <- temps[-73]
## je ne garde que les signaux où j'ai bien l'équalité de Freq_m à temps
##test <- essai[1:72,][,var1:=(Freq_m==temps/86400)]

# load("data.RData") pour le relire ? nouveau
## maintenant que mes signaux sont cylindrés je peux vouloir transposer la base 
## et créer autant de variables explicatives que ce qu'il y a de points d'observations au lieu de la garder en ligne
## est ce que cela ne simplifierait pas le calcul de certaines fonctions?
## maintenant je dois récuperer jour, 

