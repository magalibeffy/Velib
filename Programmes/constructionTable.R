require(dplyr)
require(lubridate)
require(reshape2)

load("Donnees/df.octobre.RData")

# On construit la variable taux de velos disponibles
df.constr.table <- df.octobre %>%
  mutate(taux_dispo = available_bikes / bike_stands)

# On enleve les station.jour fermes et les station.jour qui ont un taux_dispo > 1
df.constr.table <- df.constr.table %>%
  filter(status == "OPEN" & taux_dispo <= 1) %>%
  mutate(download_date_trunc = as.Date(download_date, tz="Europe/Paris"))

# On compte le nombre d'enregistrements par station.jour, on ne garde que ceux
# qui ont au moins 72 enregistrements
station.jour.complets <- df.constr.table %>%
  group_by(number, download_date_trunc) %>%
  summarise(nb_enreg = n()) %>%
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

# On enleve les station.jour ou le 1er enregistrement d'au moins une des 24 heures est a la 20e min
station.jour.ko <- df.constr.table %>%
  filter(download_minute == 20) %>%
  select(number, download_date_trunc) %>%
  distinct

df.constr.table <- anti_join(df.constr.table, station.jour.ko)

# On verifie qu'on a bien 24 enregistrements par jour
nb.enreg <- df.constr.table %>%
  group_by(number, download_date_trunc) %>%
  summarise(nb_enreg = n()) %>%
  ungroup

table(nb.enreg$nb_enreg)

table.appr <- dcast(df.constr.table, number + download_date_trunc ~ download_hour, value.var = "taux_dispo")
names(table.appr) <- c("number", "download_date_trunc", paste0("X",seq(0,23,1)))

save(table.appr, file="Donnees/table.appr.RData")
save(df.constr.table, file="Donnees/df.constr.table.RData")
