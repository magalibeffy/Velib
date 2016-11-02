require(jsonlite)
require(plyr)

# Donnees historisees recuperees sur http://vlsstats.ifsttar.fr/rawdata/
con <- file("Donnees/data_all_Paris.jjson_2016-10-01-1475295963")

data.histo.lines <- readLines(con)

data.histo <- lapply(data.histo.lines, function(x) jsonlite::fromJSON(x))

df.octobre <- ldply(data.histo, data.frame)

str(df.octobre)

df.octobre$number <- as.character(df.octobre$number)
df.octobre$last_update <- as.POSIXct(df.octobre$last_update/1000, origin="1970-01-01")
df.octobre$download_date <- as.POSIXct(df.octobre$download_date, origin="1970-01-01")

# Donnees statiques
stations <- jsonlite::fromJSON("Donnees/Paris.json")
str(stations)
stations$number <- as.character(stations$number)

save(df.octobre, file="Donnees/df.octobre.RData")
save(stations, file="Donnees/stations.RData")
