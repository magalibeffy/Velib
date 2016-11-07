## data extraction 
install.packages("leaflet")
install.packages("RCurl")
install.packages("curl")
install.packages("rjson")
install.packages("stringr", dependencies=TRUE)

install.packages("mongolite")
install.packages("ggmap")
install.packages("RgoogleMaps")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("data.table")
library(leaflet)
library(RCurl)
library(curl)
library(rjson)
library(ggplot2)
library(ggmap)
library(RgoogleMaps)
library(mongolite)
library(dplyr)
library(data.table)
library(stringr)
## chargement des données de l'année 2016
path_file = "C:/Users/Pierrot/Documents/Magali/Velib/data/"
path_file = "D:/Utilisateurs/mbeffy/Documents/recherche/Formation/Project/Project/data/"
datab <- readLines(con = paste0(path_file,"data_all_Paris.jjson_2016-10-01-1475295963"))
test  <- as.array(datab)
test <- apply(test,1,jsonlite::fromJSON)

names_v <- names(test[[1]])
names_v <- c("status","bike_stands","number","download_date","available_bike_stands","available_bikes")
test2 <- lapply(test,function(df) df<- df[,names_v])
test2 <- lapply(test2,function(df) dplyr::mutate(df,Jour = as.Date(as.POSIXct(download_date,origin="1970-01-01"))))
test2 <- lapply(test2,function(df) dplyr::mutate(df,Index1 = paste(as.character(Jour),as.character(number),sep="-")))
test2 <- lapply(test2,function(df) dplyr::mutate(df,Key = paste(Index1,as.character(download_date),sep="-")))


# nombre d'éléments d'une liste, length()
essai <- rbindlist(test2,use.names=FALSE,idcol=TRUE)
setkey(essai,Index1,download_date)

# si je voulais tracer maintenant l'utilisation d'une station par Index1 / .id ou Index1 / download_date

# tracé de la première distribution pour les stations d'un même jour
# essai[,Freq := str_sub(as.character(as.POSIXct(download_date - first(download_date),origin="1970-01-01")),-8,-1),by=Index1]
# point de mesure en seconde du jour rapporté à [0,1]
essai[,Freq_m := (download_date - first(download_date))/86400,by=Index1]
essai[,.N,by=.(number,Jour)]#compte le nombre de jour d'observations pour number


dt <- essai[1:1000,]
ggplot(dt, aes(x = Freq, y = available_bike_stands, col = Index1)) + geom_line()

dt <- essai[number==10001]
ggplot(dt, aes(x = Freq_m, y = available_bike_stands, col = Jour)) + geom_line() +geom_smooth(method=loess,aes(fill = Jour))

# interpolation ou comment gérer le fait que les relevés ne soient aps toujours faits au même moment


# dimanche : creation d'un data frame par jour considéré avec autant de variables que de mesures,
# merger les data frame by number et Jour puis les ordonner selon download_date croissant
# une fois que j'aurais un data frame par jour, je pourrais calculer mes occupations selon mes deux variables
# av_bs and av_b

# si je voulais l'écrire avec une boucle

