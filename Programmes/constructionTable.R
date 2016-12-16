require(dplyr)
require(lubridate)
require(reshape2)
require(ggplot2)

load("Donnees/df.annee.RData")

# On construit la variable taux de velos disponibles
df.constr.table <- df.annee %>%
  mutate(taux_dispo = available_bikes / bike_stands)

# On enleve les station.jour fermes et les station.jour qui ont un taux_dispo > 1
df.constr.table <- df.constr.table %>%
  filter(status == "OPEN" & taux_dispo <= 1) %>%
  mutate(download_date_trunc = as.Date(download_date, tz="Europe/Paris"))

# On compte le nombre d'enregistrements par station.jour, on ne garde que ceux
# qui ont au moins 72 enregistrements
station.jour.complets <- df.constr.table %>%
  group_by(number, download_date_trunc) %>%
  dplyr::summarise(nb_enreg = n()) %>%
  ungroup %>%
  filter(nb_enreg >= 72) %>%
  select(-nb_enreg)

df.constr.table <- inner_join(df.constr.table, station.jour.complets)

# On va selectionner le 1er enregistrement de chaque heure
df.constr.table <- df.constr.table %>%
  mutate(download_hour = hour(download_date)) %>%
  group_by(number, download_date_trunc, download_hour) %>%
  mutate(rank = row_number(download_date)) %>%
  ungroup %>%
  filter(rank == 1) %>%
  select(-rank) %>%
  mutate(download_minute = minute(download_date))

table(df.constr.table$download_minute)

# On enleve les station.jour ou le 1er enregistrement
# d'au moins une des 24 heures est apres la 5e minute
station.jour.ko <- df.constr.table %>%
  filter(download_minute > 5) %>%
  select(number, download_date_trunc) %>%
  distinct

df.constr.table <- anti_join(df.constr.table, station.jour.ko)

# On verifie qu'on a bien 24 enregistrements par jour
nb.enreg <- df.constr.table %>%
  group_by(number, download_date_trunc) %>%
  summarise(nb_enreg = n()) %>%
  ungroup

table(nb.enreg$nb_enreg)

station.jour.ko <- nb.enreg %>%
  filter(nb_enreg < 24) %>%
  select(number, download_date_trunc) %>%
  distinct

df.constr.table <- anti_join(df.constr.table, station.jour.ko)

# On vérifie qu'on a bien gardé des enregistrements tout au long de l'annee
ggplot(df.constr.table) + aes(x=download_date_trunc) + geom_bar()

# On ajoute la variable day_of_week (1=dimanche)
df.constr.table$day_of_week <- wday(df.constr.table$download_date_trunc, label=T)

# On construit la variable station.jours
df.constr.table$station.jour <- paste(df.constr.table$number, df.constr.table$download_date_trunc)
station.jour.vector <- unique(df.constr.table$station.jour)

table.appr <- dcast(df.constr.table, number + download_date_trunc ~ download_hour, value.var = "taux_dispo")
names(table.appr) <- c("number", "download_date_trunc", paste0("X",seq(0,23,1)))

save(table.appr, file="Donnees/table.appr.RData")
save(df.constr.table, station.jour.vector, file="Donnees/df.constr.table.RData")

# On selectionne des echantillons pour ne pas minpuler trop de données pour faire les tests
station.jour.vector <- sample(station.jour.vector, 10000)
df.constr.table <- df.constr.table %>%
  filter(station.jour %in% station.jour.vector)

table.appr <- dcast(df.constr.table, number + download_date_trunc ~ download_hour, value.var = "taux_dispo")
names(table.appr) <- c("number", "download_date_trunc", paste0("X",seq(0,23,1)))

save(table.appr, file="Donnees/table.appr.ech.RData")
save(df.constr.table, station.jour.vector, file="Donnees/df.constr.table.ech.RData")
