# Load data

library(tidyverse)

# Load detecties overleving

df_in <- read.csv("./data/input/Detecties_overleving.csv", quote = "\"'", sep = ";")
#str(df_in)


unique(df_in$Ringnummer)

df_det <- df_in %>%
  as_tibble() %>%
  mutate(Ringnummer = as.factor(Ringnummer),
         Koppel_Naam = as.factor(Koppel_Naam),
         Geslacht = as.factor(Geslacht),
         Datum = as.Date(Datum))

saveRDS(df_det, "./data/interim/df_det.RDS")

# Load life history
#df_in <- readxl::read_excel("./data/input/Samenvatting_levensloop_Eenvoudig.xlsx")
df_in_roes <- readxl::read_excel("./data/input/Samenvatting_levensloop.xlsx", sheet = "ROES")
df_in_midd <- readxl::read_excel("./data/input/Samenvatting_levensloop.xlsx", sheet = "MIDD")

str(df_in_roes)
str(df_in_midd)


rqm <- function(x) ifelse(x)

df_hist <- df_in_roes %>%
  bind_rows(df_in_midd) %>%
  filter(!is.na(Ringnummer)) %>%
  rename(Datum_Vangst = Datum) %>%
  mutate(across(End_code:`2nd_Nest_Survival`,function(x) ifelse(x == "NA", NA, x))) %>%
  mutate(End = ifelse(End == "ALIVE", NA, End)) %>%
  mutate_at(.vars = c("1st_Nest_Stop", "2nd_Nest_Stop", "1st_Nest_eggs", "1st_Nest_Hatched"),
            .funs = function(x) ifelse(x == "?", NA, x)) %>%
  mutate(End = as.numeric(End),
         `1st_Nest_Stop` = as.numeric(`1st_Nest_Stop`),
         `2nd_Nest_Stop` = as.numeric(`2nd_Nest_Stop`)) %>%
  mutate(Ringnummer = as.factor(Ringnummer),
         Datum_Vangst = as.Date(Datum_Vangst),
         Nickname = as.factor(Nickname),
         Geslacht = as.factor(Geslacht),
         Koppel_Naam = as.factor(Koppel_Naam),
         Koppel_nr = as.factor(Koppel_nr),
         Gebied = as.factor(Gebied),
         End = as.Date(End, origin = "1899-12-30"),
         End_code = as.factor(End_code),
         `1st_Nest_Attempt` = as.factor(`1st_Nest_Attempt`),
         `1st_Nest_Stop` = as.Date(`1st_Nest_Stop`, origin = "1899-12-30"),
         `1st_Nest_Succes` = as.factor(`1st_Nest_Succes`),
         `1st_Nest_eggs` = as.numeric(`1st_Nest_eggs`),
         `1st_Nest_Hatched` = as.numeric(`1st_Nest_Hatched`),
         `1st_Nest_Survival` = as.numeric(`1st_Nest_Survival`),
         `2nd_Nest_Attempt` = as.factor(`2nd_Nest_Attempt`),
         `2nd_Nest_Stop` = as.Date(`2nd_Nest_Stop`, origin = "1899-12-30"),
         `2nd_Nest_Succes` = as.factor(`2nd_Nest_Succes`),
         `2nd_Nest_eggs` = as.numeric(`2nd_Nest_eggs`),
         `2nd_Nest_Hatched` = as.numeric(`2nd_Nest_Hatched`),
         `2nd_Nest_Survival` = as.numeric(`2nd_Nest_Survival`))

#str(df_hist)

saveRDS(df_hist, "./data/interim/df_hist.RDS")

