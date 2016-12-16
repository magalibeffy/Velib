require(jsonlite)
require(plyr)

# Donnees historisees recuperees sur http://vlsstats.ifsttar.fr/rawdata/

# Recuperer la liste des fichiers de donnees
liste.fichiers <- list.files("Donnees/Fichiers_sources")

# Parser les fichiers de la liste et les coller ensemble
for(f in liste.fichiers){
  con <- file(paste0("Donnees/Fichiers_sources/", f))
  data.histo.lines <- readLines(con)
  data.histo <- lapply(data.histo.lines, function(x) jsonlite::fromJSON(x))
  data.histo <- ldply(data.histo, data.frame)
  if(exists("df.annee")){
    df.annee <- rbind(df.annee, data.histo)
  }else{
    df.annee <- data.histo
  }
}

str(df.annee)

df.annee$number <- as.character(df.annee$number)
df.annee$last_update <- as.POSIXct(df.annee$last_update/1000, origin="1970-01-01")
df.annee$download_date <- as.POSIXct(df.annee$download_date, origin="1970-01-01")

# Donnees statiques
stations <- jsonlite::fromJSON("Donnees/Paris.json")
str(stations)
stations$number <- as.character(stations$number)

save(df.annee, file="Donnees/df.annee.RData")
save(stations, file="Donnees/stations.RData")
