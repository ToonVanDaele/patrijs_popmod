#################################################################
##   Model patrijs ##
#################################################################

setwd("G:/Mijn Drive/INBO/Team_FAUNABEHEER/Adviezen/2018/Adviezen 2018/Jachtverbod patrijs, gaai en ekster/Opmerkingen draft1")


# 1. Opzet

# Op basis van het model van Bro et al 2000 proberen de voorjaarsstand om te zetten naar een najaarsstand om op deze manier het afschot te kunnen vergelijken met de geschatte najaarsstand.
# Opmerking: het model heeft enkel betrekking op hennen, waardoor de hanen (en dan ook de helft van de kuikens) niet meegerekend worden. Daar moet je dus voor corrigeren.


# 2. Model parameters

# Maak hier de keuze welke van de 3 parametersets je wil gebruiken (zie de tabel in het rapport)

# 2.1. Maximale parameters:

# b kans op broeden:
b <- 1
# n1 nestsucces eerste legsel:
n1 <- 0.73
# u1 aantal uitgekomen eieren per eerste legsel:
u1 <- 13.8
# Sk1 overleving van kuikens uit een eerste legsel:
Sk1 <- 0.78
# h kans op herleg
h <- 0.78
# nh nestsucces herlegsel
nh <- 0.53
# uh aantal uitgekomen eieren per herlegsel
uh <- 10
# Skh overleving van kuikens uit een herlegsel
Skh <- 0.78
# Sai1 overleving hen tijdens incubatie 1e legsel
Sai1 <- 0.78
# Saz1 overleving hen zomer zonder herlegsel
Saz1 <- 0.8
# Saih overleving hen tijdens incubatie herlegsel
Saih <- 1
# Sazh overleving hen met herlegsel
Sazh <- 0.9
# Sjw overleving juvenielen tijdens winter
Sjw <- 0.68
# Saw overleving adulten tijdens winter
Saw <- 0.68


# 2.2. Bro parameters:
b <- 1
n1 <- 0.46
u1 <- 12.3
Sk1 <- 0.4
h <- 0.78
nh <- 0.4
uh <- 9
Skh <- 0.4
Sai1 <- 0.78
Saz1 <- 0.8
Saih <- 1
Sazh <- 0.9
Sjw <- 0.68
Saw <- 0.68
# Roodbergen: Bij Bro is er inderdaad iets misgegaan en heb ik in de berekening van Lambda 0.57 (ipv 0.68 in de tabel) gebruikt voor de (juveniele en adulte) overleving in de winter. Welke van de twee waarden de correcte is weet ik zo gauw niet, maar dit zou te maken kunnen hebben met dat de winteroverleving niet meer is uitgesplitst naar overleving van het jachtseizoen en de rest van de winter, zoals bij Bro (0.8*0.68=0.57).
# Model zonder jacht is dus met 0.68, dus is de Lambda zoals in de tabel verkeerd en die dat wij uitkomen correct
# Sjw <- 0.57
# Saw <- 0.57


# 2.3. East Anglia parameters:
b <- 0.93
n1 <- 0.63
u1 <- 13.5
Sk1 <- 0.31
h <- 0.78
nh <- 0.4
uh <- 9
Skh <- 0.31
Sai1 <- 0.78
Saz1 <- 0.8
Saih <- 1
Sazh <- 0.9
Sjw <- 0.52
Saw <- 0.52




# 3. De startpopulatie:

# maak een keuze welke startpopulatie je wil gebruiken

# 3.1. Devos broedvogelatlas
Nj_t <- 0
Na_t <- 5000 #-> broedparen, dus evenveel hennen

# 3.2. WBE-gegevens 2016 in wildrapport 2017
# 3.2.1. Alle 185 WBE's
Nj_t <- 0
Na_t <- 24698 #-> broedparen, dus evenveel hennen
# 3.2.2. Enkel 117 WBE's met afschot
Nj_t <- 0
Na_t <- 21791 #-> broedparen, dus evenveel hennen

# 4. Model

# In het model beschouwen we de populatie als gesloten,
# wat betekent dat er geen immi- of emigratie
# plaatsvindt. We modelleren alleen het vrouwelijke
# deel van de populatie, en veronderstellen daarbij
# een geslachtsverhouding van 1:1 (reproductiecijfers
# worden om deze reden door 2 gedeeld). Het model
# is geparametriseerd als een pre-breeding survey:
# het beschrijft de aantallen en leeftijdsopbouw van de
# populatie in het voorjaar, direct voorafgaande aan
# het broedseizoen.
#
# We onderscheiden twee leeftijdsklassen: vogels
# geboren in het voorafgaande jaar, die voor het eerst
# gaan broeden (‘eenjarige’), en oudere vogels (‘adulte’).
# Beide leeftijdsgroepen nemen deel aan de reproductie,
# waarbij we aannemen dat broedsucces en
# overleving bij de twee leeftijdsgroepen gelijk zijn.

# Opmering Thomas: aangezien beide gelijk zijn, heeft dit weining zin... Vandaar dat we bij de startpopulatie de schatting van de voorjaarsstand bij Na kunnen zetten en Nj op nul

# Dit betekent dat, gegeven het aantal vogels in de twee
# leeftijdsklassen in jaar t, het aantal in het volgende
# jaar (t+1) wordt beschreven als:

# Nj_tplus1 <- (Pjr / 2) * (Nj_t + Na_t)
#
# Na_tplus1 <- Sab * Saw * (Nj_t + Na_t)

# Hierin zijn Nj en Na respectievelijk het aantal eenjarige
# en adulte vogels, Pjr het aantal uitgevlogen
# jongen geproduceerd per adult per jaar, Sab de overleving
# van hennen tijdens het broedseizoen en Saw de
# overleving van hennen tijdens de winter.

# Opmerking Thomas: Pjr / 2 omdat ze enkel de hennen willen hebben en van een 1:1 geslachtsratio uitgaan

# Populatiemodellen worden vaak in een matrixnotatie
# geschreven, onder meer omdat dit het berekenen
# van een aantal afgeleide grootheden, zoals de populatiegroeisnelheid,
# eenvoudig maakt (Caswell 2001).
# In matrixnotatie ziet hetzelfde model er zo uit:

# -> zie publicatie blz 14

# De middelste term is de transitiematrix. De 2 x 2
# elementen van de matrix beschrijven wat de bijdrage
# is van elk van de twee leeftijdsklassen in jaar t aan
# elk van de twee klassen in jaar t+1.

# De jongenproductie Pjr bestaat uit de som van twee
# termen: de productie van jongen uit eerste legsels
# plus de productie van jongen uit herlegsels. Beide
# worden beschreven als product van de overleving
# van de hen tijdens de incubatie van het eerste legsel,
# de kans op broeden/leggen van een herlegsel (b
# en h), het nestsucces (n1 en nh), aantal uitgekomen
# eieren per legsel (u1 en uh), de kuikenoverleving tot
# een leeftijd van 6 weken (Sk1 en Skh) en de juvenielen
# overleving in de winter, tot het censusmoment. De
# productie van jongen uit herlegsels moet bovendien
# vermenigvuldigd worden met de overleving van de
# hen tijdens de incubatie van het herlegsel en de kans
# dat een eerste legsel mislukt (1-n1), aangezien een
# herlegsel pas wordt gemaakt als het eerste legsel is
# mislukt. In formulevorm ziet Pjr er dan als volgt uit:

Pjr <- Sai1 * ((b * n1 * u1 * Sk1) + Saih * (1 - n1) * h * nh * uh * Skh) * Sjw

# De overleving van hennen tijdens het broedseizoen
# is op te splitsen als product van de overleving
# tijdens de incubatie van eerste legsels en de overleving
# daarna. De laatste bestaat uit de overleving
# van hennen met jongen uit het eerste legsel plus de
# overleving van hennen die hun eerste legsel hebben
# verloren, plus de overleving van hennen tijdens en
# na de incubatie van herlegsels. In formulevorm ziet
# de overleving van hennen in de broedperiode er als
# volgt uit:

Sab <- Sai1 * ((1 - h + n1 * h) * Saz1 + (1 - n1) * h * Saih * Sazh)

# De transitiematrix wordt daarmee:

#-> zie pagina 15

# Nu de jongenproductie Pjr en overleving van hennen tijdens het broedseizoen bekend zijn kunnen we de nieuwe voorjaarsstand berekenen:

# Opnieuw:
Nj_tplus1 <- (Pjr / 2) * (Nj_t + Na_t)
#-> aantal uitgekomen vrouwlijke kuikens die overleven tot het nieuwe voorjaar
Na_tplus1 <- Sab * Saw * (Nj_t + Na_t)
#-> aantal hennen die broeden overleven en winter overleven tot het nieuwe voorjaar


# De nieuwe voorjaarsstand is dan gelijk aan:
Ntotaal_tplus1 <- Nj_tplus1 + Na_tplus1
Ntotaal_tplus1

# Lambda is gelijk aan:
Ntotaal_tplus1 / (Nj_t + Na_t)

# Voor maximale set: 2.750246 -> klopt met publicatie: 2.75
# Voor Bro: 1.207712 -> klopt niet met publicatie: 1.01 #-> omdat Roodbergen fout gemaakt heeft, dit is zonder jacht
# Nu voor Bro: 1.012347 -> klopt met de publicatie: 1.01 #-> let wel, hier zit jacht in
# Voor East Anglia: 0.9361953 -> klopt niet met publicatie: 0.90 #-> foute waarde voor b (niet 1 maar 0,93!)
# Nu voor East Anglia: 0.8987668

#-> vergelijk deze waarden met de waarden in de tabel van het rapport: dit klopt enkel voor het maximaal scenario...



# 5. Schatting van de najaarspopulatie

# In geval je najaarsstand wil berekenen ipv de nieuwe voorjaarsstand

#Als je de najaarsstand wil bekomen kan je de winteroverleving op 1 zetten, zodat alles de winter overleeft

# Sjw overleving juvenielen tijdens winter
Sjw <- 1
# Saw overleving adulten tijdens winter:
Saw <- 1

# Model opnieuw runnen:
Pjr <- Sai1 * ((b * n1 * u1 * Sk1) + Saih * (1 - n1) * h * nh * uh * Skh) * Sjw
Sab <- Sai1 * ((1 - h + n1 * h) * Saz1 + (1 - n1) * h * Saih * Sazh)

# Najaarstand schatten:

# Het aantal kuikens die overleven tot het najaar is dan (zowel mannelijk als vrouwelijk, dus niet delen door 2)
Nj_tnajaar <- (Pjr) * (Nj_t + Na_t) #-> zowel de hennen als de hanen kuikens nu

# Het aantal hennen die overleven:
Na_tnajaar <- Sab * Saw * (Nj_t + Na_t)  #-> aandeel hennen die overleven

# Extra: het aantal hanen die overleven
# we gaan er van uit dat alle gekoppelde hanen overleven tot het najaar. Het aantal gekoppelde hanen is dan gelijk aan het aantal hennen, maw het aantal broedparen
Na_hanen_tnajaar <- 1 * (Nj_t + Na_t) #-> aandeel gekoppelde hanen die overleven met kans op 1
# We missen nu de niet-gekoppelde hanen in de schatting
#-> dus overschatting door overleving op 1 te zetten van gekoppelde hanen, maar onderschatting door niet-gekoppelde hanen niet mee te nemen

# Totaal schatting najaarsstand:
Ntotaal_tnajaar <- Nj_tnajaar + Na_tnajaar + Na_hanen_tnajaar
Ntotaal_tnajaar

# Het percentage afschot:
# -> het afschot in Vlaanderen in 2016 was 14.348 patrijzen (wildrapport 2017)
14348/Ntotaal_tnajaar*100

# Bro set & Devos: 73.67016 %
# Bro set & WBE: 16.90367 %



# 6. Schatting van de najaarspopulatie per WBE
voorjaar <- read.csv2("G:/Mijn Drive/INBO/Team_FAUNABEHEER/Adviezen/2018/Adviezen 2018/Jachtverbod patrijs, gaai en ekster/Grafieken + kaarten patrijs/voorjaarsstand_en_afschot.csv")

voorjaar$Na_t <- voorjaar$Voorjaar

# Najaarstand schatten:

#Nj_t is nul
Nj_t <- 0

# Het aantal kuikens die overleven tot het najaar is dan (zowel mannelijk als vrouwelijk, dus niet delen door 2)
voorjaar$Nj_tnajaar <- (Pjr) * (Nj_t + voorjaar$Na_t) #-> zowel de hennen als de hanen kuikens nu

# Het aantal hennen die overleven:
voorjaar$Na_tnajaar <- Sab * Saw * (Nj_t + voorjaar$Na_t)  #-> aandeel hennen die overleven

# Extra: het aantal hanen die overleven
# we gaan er van uit dat alle gekoppelde hanen overleven tot het najaar. Het aantal gekoppelde hanen is dan gelijk aan het aantal hennen, maw het aantal broedparen
voorjaar$Na_hanen_tnajaar <- 1 * (Nj_t + voorjaar$Na_t) #-> aandeel gekoppelde hanen die overleven met kans op 1
# We missen nu de niet-gekoppelde hanen in de schatting
#-> dus overschatting door overleving op 1 te zetten van gekoppelde hanen, maar onderschatting door niet-gekoppelde hanen niet mee te nemen

# Totaal schatting najaarsstand:
voorjaar$Ntotaal_tnajaar <- voorjaar$Nj_tnajaar + voorjaar$Na_tnajaar + voorjaar$Na_hanen_tnajaar

# Het percentage afschot:
voorjaar$Jachtintensiteit <- voorjaar$Afschot / voorjaar$Ntotaal_tnajaar * 100


library(ggplot2)
library(INBOtheme)
library(plyr)

ggplot(voorjaar, aes(y=Jachtintensiteit)) +
  geom_boxplot(outlier.size=2) +
  labs(y = "jachtintensiteit (%)") +
  scale_y_continuous(limits=c(0, 120), breaks=seq(0,120,20)) + # maximum 112%
  geom_hline(yintercept=20, linetype="dotted", color = "darkblue", size=1)

# -> minder dan de helft van de WBE's zitten boven de 20%

#ggsave("boxplot_jachtintensiteit_Vlaanderen.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)

ggplot(voorjaar, aes(x = Provincie, y=Jachtintensiteit)) +
  geom_boxplot(outlier.size=2) +
  labs(x = "Provincie", y = "jachtintensiteit (%)") +
  scale_y_continuous(limits=c(0, 120), breaks=seq(0,120,20)) + # maximum 112%
  geom_hline(yintercept=20, linetype="dotted", color = "darkblue", size=1)

#ggsave("boxplot_jachtintensiteit_Provincie.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)

# beter werken met histogram, want nu zie je de staart niet goed:

#eerst NA's er uit:
temp <- subset(voorjaar, !is.na(Jachtintensiteit))

# Bin maken per 5%:
min(temp$Jachtintensiteit)
max(temp$Jachtintensiteit)
#-> max 85.5745
bins <- round(max(temp$Jachtintensiteit)/5,0) + 1 #plus één want anders zit de laatste er niet in
temp$Intensiteitsklasse <- NA

for (i in 1:bins)
{ # start loop
  temp$Intensiteitsklasse[temp$Jachtintensiteit < i*5 & is.na(temp$Intensiteitsklasse)] <- i
} # end loop

table(temp$Intensiteitsklasse)

#controle:
sum(!is.na(temp$Intensiteitsklasse)) - dim(temp)[1] #-> moet nul zijn

temp2 <- ddply(temp, c("Intensiteitsklasse"), summarise, n = sum(!is.na(Intensiteitsklasse)))

ggplot(temp2, aes(x= Intensiteitsklasse, y = n)) +
  geom_bar(stat="identity") +
  labs(y = "Aantal wildbeheereenheden", x = "Jachtintensiteit (percentage afschot t.o.v. gemodelleerde najaarsstand)") +
  scale_x_continuous(breaks=seq(0.5, 24.5, 1), labels=seq(0, 120, 5)) + # omdat de klasses per 500m gaan, dus 4 klasses is 2 km
  scale_y_continuous(breaks=seq(0, 30, 2)) +
  geom_vline(xintercept=4.5, linetype="dashed", color = "darkblue", size=1)

# ggsave("histogram_jachtintensiteit_Vlaanderen.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)

#op basis van hoeveel WBE's:
sum(temp2$n)

#hoeveel WBE's boven 20%:
dim(subset(temp, Jachtintensiteit > 20))[1]
#hoeveel WBE's onder 20%:
dim(subset(temp, Jachtintensiteit < 20))[1]

write.csv2(voorjaar , file = "voorjaar_jachtintensiteit.csv")

#Per provincie:

temp3 <- ddply(temp, c("Intensiteitsklasse", "Provincie"), summarise, n = sum(!is.na(Intensiteitsklasse)))

ggplot(temp3, aes(x= Intensiteitsklasse, y = n)) +
  geom_bar(stat="identity") +
  labs(y = "Aantal wildbeheereenheden", x = "Jachtintensiteit (percentage afschot t.o.v. gemodelleerde najaarsstand)") +
  scale_x_continuous(breaks=seq(0.5, 24.5, 1), labels=seq(0, 120, 5)) + # omdat de klasses per 500m gaan, dus 4 klasses is 2 km
  scale_y_continuous(breaks=seq(0, 30, 2)) +
  geom_vline(xintercept=4.5, linetype="dashed", color = "darkblue", size=1) +
  facet_grid(Provincie ~ .)

# ggsave("histogram_jachtintensiteit_provincie.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)

#op basis van hoeveel WBE's:
sum(temp3$n)

#hoeveel WBE's boven 20%:
dim(subset(temp, Jachtintensiteit > 20))[1]
#hoeveel WBE's onder 20%:
dim(subset(temp, Jachtintensiteit < 20))[1]


# Kaart drempel 20 patrijzen per 100 ha:

# De najaarsstand per 100ha:
voorjaar$RelNajaar <- voorjaar$Ntotaal_tnajaar / voorjaar$VoorjaarOpp * 100

# Figuur histogram Relnajaarsstand

#eerst NA's er uit:
temp <- subset(voorjaar, !is.na(RelNajaar))

# Bin maken per 0.5:
min(temp$RelNajaar)
max(temp$RelNajaar)
summary(temp$RelNajaar)
#-> max 27.23777
bins <- round(max(temp$RelNajaar)/1,0) + 1 #plus één want anders zit de laatste er niet in
temp$Intensiteitsklasse <- NA

for (i in 1:bins)
{ # start loop
  temp$Intensiteitsklasse[temp$RelNajaar < i*1 & is.na(temp$Intensiteitsklasse)] <- i
} # end loop

table(temp$Intensiteitsklasse)

#controle:
sum(!is.na(temp$Intensiteitsklasse)) - dim(temp)[1] #-> moet nul zijn

temp2 <- ddply(temp, c("Intensiteitsklasse"), summarise, n = sum(!is.na(Intensiteitsklasse)))

ggplot(temp2, aes(x= Intensiteitsklasse, y = n)) +
  geom_bar(stat="identity") +
  labs(y = "Aantal wildbeheereenheden", x = "Aantal patrijzen najaar per 100 ha") +
  scale_x_continuous(breaks=seq(1, 29, 1), labels=seq(0, 28, 1)) +
  scale_y_continuous(breaks=seq(0, 30, 1))

# ggsave("histogram_drempel_najaarsstand_WBE.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)

write.csv2(temp , file = "voorjaar_relnajaarstand.csv")

# scatterplot opp:

temp3 <- voorjaar[order(voorjaar$VoorjaarOpp),]
temp3$seq <- seq(1,185,1)

ggplot(temp3, aes(x = seq)) +
  geom_point(aes(y = VoorjaarOpp), colour = "blue") +
  geom_point(aes(y = AfschotOpp), colour = "red") +
  scale_y_continuous(breaks=seq(0, 30000, 2000)) +
  geom_hline(yintercept=2000, size = 1) +
  labs(y = "Oppervlakte (ha)", x = "WBE")

# ggsave("histogram_drempel_opp_WBE.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)
remove(temp3)

# scatterplot voorjaarsstand:

temp3 <- voorjaar[order(voorjaar$Voorjaar),]
temp3$seq <- seq(1,185,1)

ggplot(temp3, aes(x = seq)) +
  geom_point(aes(y = Voorjaar), colour = "blue") +
  scale_y_continuous(breaks=seq(0, 1000, 100)) +
  geom_hline(yintercept=50, size = 1) +
  geom_hline(yintercept=100, size = 1) +
  geom_hline(yintercept=200, size = 1) +
  labs(y = "Aantal koppels", x = "WBE")

# ggsave("histogram_drempel_koppels_WBE.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)
remove(temp3)


# 7. Voorjaarstand WBE


# Figuur histogram voorjaarsdensiteit

#eerst NA's er uit:
temp <- subset(voorjaar, !is.na(RelVoorjaar))

# Bin maken per 0.5:
min(temp$RelVoorjaar)
max(temp$RelVoorjaar)
summary(temp$RelVoorjaar)
#-> max 6.992574
bins <- round(max(temp$RelVoorjaar)/0.5,0) + 1 #plus één want anders zit de laatste er niet in
temp$Intensiteitsklasse <- NA

for (i in 1:bins)
{ # start loop
  temp$Intensiteitsklasse[temp$RelVoorjaar < i*0.5 & is.na(temp$Intensiteitsklasse)] <- i
} # end loop

table(temp$Intensiteitsklasse)

#controle:
sum(!is.na(temp$Intensiteitsklasse)) - dim(temp)[1] #-> moet nul zijn

temp2 <- ddply(temp, c("Intensiteitsklasse"), summarise, n = sum(!is.na(Intensiteitsklasse)))

ggplot(temp2, aes(x= Intensiteitsklasse, y = n)) +
  geom_bar(stat="identity") +
  labs(y = "Aantal wildbeheereenheden", x = "Aantal koppels per 100 ha") +
  scale_x_continuous(breaks=seq(0.5, 14.5, 1), labels=seq(0, 7, 0.5)) +
  scale_y_continuous(breaks=seq(0, 30, 5))

# ggsave("histogram_voorjaarsstand_WBE.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)

write.csv2(temp , file = "voorjaar_relvoorjaarsstand.csv")


# Nu van deze met correctie open ruimte:
temp <- read.csv2("G:/Mijn Drive/INBO/Team_FAUNABEHEER/Adviezen/2018/Adviezen 2018/Jachtverbod patrijs, gaai en ekster/Opmerkingen draft1/Berekening_patrijs_2017_csv.csv")

# opm: bij het vorige bestand had Yasmine de voorjaarstand van WBE Dendervallei-West verkeerd gezet: 367 patrijzen ipv 37
# ook blijkbaar wijziging Middenkempen: van 7 naar 8
# dit is dus juist


#nodige kolommen
temp <- temp[,c("WbeNummer.2", "Naam.2", "Jaar.2", "TellingOppervlakte.2", "Patrijs.2", "Opp_open_2016", "patrijs_per_open_2016")]

temp$RelVoorjaar <- temp$Patrijs.2 / temp$TellingOppervlakte.2 * 100

# Beter ook de rel voorjaar opnieuw maken:

# Figuur histogram voorjaarsdensiteit
summary(temp$RelVoorjaar)

#eerst NA's er uit:
temp2 <- subset(temp, !is.na(RelVoorjaar))

# Bin maken per 0.5:
min(temp2$RelVoorjaar)
max(temp2$RelVoorjaar)
summary(temp2$RelVoorjaar)
#-> max 6.992574
bins <- round(max(temp2$RelVoorjaar)/0.5,0) + 1 #plus één want anders zit de laatste er niet in
temp2$Intensiteitsklasse <- NA

for (i in 1:bins)
{ # start loop
  temp2$Intensiteitsklasse[temp2$RelVoorjaar < i*0.5 & is.na(temp2$Intensiteitsklasse)] <- i
} # end loop

table(temp2$Intensiteitsklasse)

#controle:
sum(!is.na(temp2$Intensiteitsklasse)) - dim(temp2)[1] #-> moet nul zijn

write.csv2(temp2 , file = "voorjaar_relvoorjaarsstand_2.csv")

temp2 <- ddply(temp2, c("Intensiteitsklasse"), summarise, n = sum(!is.na(Intensiteitsklasse)))

ggplot(temp2, aes(x= Intensiteitsklasse, y = n)) +
  geom_bar(stat="identity") +
  labs(y = "Aantal wildbeheereenheden", x = "Aantal koppels per 100 ha") +
  # scale_x_continuous(breaks=seq(0.5, 14.5, 1), labels=seq(0, 7, 0.5)) +
  scale_x_continuous(breaks=seq(0.5, 18.5, 1), labels=seq(0, 9, 0.5)) + # om vergelijkbaar te zijn met open ruimte
  expand_limits(x=c(0.5, 18.5)) +
  scale_y_continuous(breaks=seq(0, 30, 5))

# ggsave("histogram_voorjaarsstand_WBE.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)


# Nu Figuur histogram voorjaarsdensiteit open ruimte
summary(temp$patrijs_per_open_2016)
#-> beter even opnieuw bereken door afronding:
temp$patrijs_per_open_2016 <- temp$Patrijs.2 / temp$Opp_open_2016 * 100

#eerst NA's er uit:
temp2 <- subset(temp, !is.na(patrijs_per_open_2016))

# Bin maken per 0.5:
min(temp2$patrijs_per_open_2016)
max(temp2$patrijs_per_open_2016)
summary(temp2$patrijs_per_open_2016)
#-> max 8.842866
bins <- round(max(temp2$patrijs_per_open_2016)/0.5,0) + 1 #plus één want anders zit de laatste er niet in
temp2$Intensiteitsklasse <- NA

for (i in 1:bins)
{ # start loop
  temp2$Intensiteitsklasse[temp2$patrijs_per_open_2016 < i*0.5 & is.na(temp2$Intensiteitsklasse)] <- i
} # end loop

table(temp2$Intensiteitsklasse)

#controle:
sum(!is.na(temp2$Intensiteitsklasse)) - dim(temp2)[1] #-> moet nul zijn

write.csv2(temp2 , file = "voorjaar_relvoorjaarsstand_open_ruimte.csv")

temp2 <- ddply(temp2, c("Intensiteitsklasse"), summarise, n = sum(!is.na(Intensiteitsklasse)))

ggplot(temp2, aes(x= Intensiteitsklasse, y = n)) +
  geom_bar(stat="identity") +
  labs(y = "Aantal wildbeheereenheden", x = "Aantal koppels per 100 ha open ruimte") +
  scale_x_continuous(breaks=seq(0.5, 18.5, 1), labels=seq(0, 9, 0.5)) +
  scale_y_continuous(breaks=seq(0, 30, 5))

# ggsave("histogram_voorjaarsstand_WBE_open_ruimte.jpg", width = 22.4 /2.54, height = 15 / 2.54, dpi=300)



