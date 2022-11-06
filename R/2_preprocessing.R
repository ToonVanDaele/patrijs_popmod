# Preprocessing

library(tidyverse)

# load data
df_det <- readRDS("../data/interim/df_det.RDS")
df_hist <- readRDS("../data/interim/df_hist.RDS")

# Check consistency

unique(df_det$Waarneming)

# nog in te vullen!!


# filter and wrangling

df <- df_det %>%
  filter(!substr(Koppel_Naam, start = 1, stop = 4) == "RAMS") %>%
  mutate(wk = strftime(Datum, format = "%V")) %>%
  group_by(Ringnummer, Geslacht, wk) %>%
  summarise(Waarneming = max(Waarneming), .groups = "drop")

view(df)

df_lh <- df_hist %>%
  dplyr::select(Ringnummer, Datum_Vangst, Geslacht, Gebied, End)

unique(df_lh$Ringnummer)


x# Prepare format for LDLDLD 'known fate model RMArk'


