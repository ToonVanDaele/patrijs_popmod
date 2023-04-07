# Preprocessing

library(tidyverse)

# load data
df_det <- readRDS("./data/interim/df_det.RDS")
df_hist <- readRDS("./data/interim/df_hist.RDS")

# Check consistency

unique(df_hist$Gebied)
unique(df_det$Waarneming)

# nog verder in te vullen!!


# filter and wrangling

df_det2 <- df_det %>%
  filter(!substr(Koppel_Naam, start = 1, stop = 4) == "RAMS") %>%
  group_by(Ringnummer) %>%
  arrange(desc(Datum)) %>%
  slice(1)

df_det2

df_hist2 <- df_hist %>%
  dplyr::select(Ringnummer, Datum_Vangst, Geslacht, Gebied, End)

df_hist2
# Prepare data in vector format

# Marking time

# start of study
startdate <- min(df_hist2$Datum_Vangst)

# End of study (or end of data for the moment)
lastdate <- max(df_det2$Datum)

# Marking
df_mark <- df_hist2 %>%
  mutate(m_date = Datum_Vangst)

# survival & truncationg

df_det2

# 1 - met zender en levend
# 2 - individu dood gevonden met zender -> dood
# 3 - einde studie (laatst waarneming in studiegebied)  -> truncated
# 4 - missing data (laatste datum van en individu dat nadien niet meer terguggevonden is) -> truncated
# 5 - zender los zonder verdere info (mogelijk levend) -> truncated
# 6 - zender los met bijtsporen -> dood


df_dead <- df_det2 %>%
  filter(Waarneming %in% c(2, 6)) %>%
  mutate(s_date = Datum) %>%
  dplyr::select(Ringnummer, Waarneming, s_date)

df_trunc <- df_det2 %>%
  filter(Waarneming %in% c(3, 4, 5)) %>%
  mutate(l_date = Datum) %>%
  dplyr::select(Ringnummer, Waarneming, l_date)

# Join survival and truncation to mark data

df_lh <- df_mark %>%
  left_join(df_dead, by = "Ringnummer") %>%
  left_join(df_trunc, by = "Ringnummer") %>%
  dplyr::select(Ringnummer, Gebied, Geslacht, m_date, s_date, l_date)

# We remove mortality within one week after ringing

df_lh <- df_lh %>%
  filter(is.na(s_date) | s_date > m_date + 7)

#view(df_lh)

# Check consistency

df_lh <- df_lh %>%
  mutate(check_ms = ifelse(s_date <= m_date, "!", NA),
         check_ml = ifelse(l_date <= m_date, "!", NA),
         check_sl = ifelse(l_date > s_date, "!", NA))

df_lh %>%
  filter(!(is.na(check_ml) & is.na(check_ms) & is.na(check_sl)))

# We remove inconsistent data
df_lh <- df_lh %>%
  filter((is.na(check_ml) & is.na(check_ms) & is.na(check_sl)))

# date ->days since start of study
# All NA after last detection are truncated (end of study)
df_lh <- df_lh %>%
  mutate(m_time = m_date - startdate,
         s_time = s_date - startdate,
         l_time = l_date - startdate) %>%
  mutate(l_time = ifelse(is.na(l_time), lastdate - startdate + 1, l_time),
         s_time = ifelse(is.na(s_time), 999, s_time))

#view(df_lh)

saveRDS(df_lh, "./data/interim/df_lh.RDS")

# Observations

df_obs <- df_det2 %>%
  mutate(obs = Datum - startdate)

saveRDS(df_obs, "./data/interim/df_obs.RDS")


# Die direct doodgaan mogen weg. 7 dagen nemen als minimum overleving.

