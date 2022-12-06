library(foreign)
library(car)
library(tidyverse)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename


# Import data -------------------------------------------------------------

# make vector of demographic variables to include in final datafile
demogr.vars<-read.csv("../data/demographicVariables.csv")
demogr.vars <- demogr.vars$demogr.vars

# all analyzed items
allAnalyzedItems <- read.csv("../data/allitems.csv")



## Stockholm stad ----------------------------------------------------------

# read and combine data
df.1420 <- read.spss("../data/SE 2014-2020 KI Leifman.sav", to.data.frame = TRUE)
df.0612 <- read.spss("../data/2006-2012 Stockholmsenkäten 201126.sav", to.data.frame = TRUE)
df.sthlm <- rbind(df.0612,df.1420)

# subset selected demographic variables and items
df.sthlm <- df.sthlm %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))

# to match Vaxholms variables
df.sthlm <- df.sthlm %>% 
  add_column(SkolID_gammal = NA) %>% 
  add_column(DIDkommun = 'Stockholm')

## Vallentuna ----------------------------------------------------------
df.vtuna1618 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Vallentuna/Sthlmsenk/Stockholmsenkäten 2018 Vallentuna 2016-2018.sav"))
df.vtuna20 <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Vallentuna/Sthlmsenk/Stockholmsenkäten 2020 Vallentuna.sav"))

df.vtuna1618 <- df.vtuna1618 %>% 
  rename(ARSKURS = Arskurs)

df.vtuna20 <- df.vtuna20 %>% 
  rename(Kön = F2)

df.vtuna1618 <- df.vtuna1618 %>% 
  rename(Skolnamn = SkolID) %>% 
  add_column(Skolenhetskod = NA)

## subset variables
df.vtuna1 <- df.vtuna1618 %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr)))
df.vtuna2 <- df.vtuna20 %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr)))

# Columns `F67`, and `F68` don't exist.
df.vtuna1<-df.vtuna1 %>% 
  add_column(F67 = NA, F68 = NA, F3_Omkodad = NA)

df.vtuna <- rbind(df.vtuna1,df.vtuna2)

# to match Vaxholms variables
df.vtuna <- df.vtuna %>% 
  add_column(SkolID_gammal = NA, SkolSDO = NA) %>% 
  add_column(DIDkommun = 'Vallentuna')

## Vaxholm ----------------------------------------------------------

df.vaxholm <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Vaxholm/Sthlmsenk/Stockholmsenkäten 2008-2022 Vaxholm (1).sav"))

df.vaxholm <- df.vaxholm %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(DIDkommun = 'Vaxholm')

## Danderyd ----------------------------------------------------------------

df.danderyd <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Danderyd/Stockholmsenkäten 2014-2022 Danderyd.sav"))

df.danderyd <- df.danderyd %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(DIDkommun = 'Danderyd')


## Täby --------------------------------------------------------------------

df.täby <- as.data.frame(read.spss("~/Library/CloudStorage/OneDrive-SharedLibraries-RISE/SHIC - Data i Dialog - Data i Dialog/data/Täby/Stockholmsenkäten 2016 & 2020-2022 Täby (2).sav"))

df.täby <- df.täby %>% 
  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO"))) %>% 
  add_column(DIDkommun = 'Täby')

# Combine all data --------------------------------------------------------

## find out which, if any, variables differ between df's
# df.sthlm %>%
#  select_at(vars(names(df.vaxholm)))

df <- rbind(df.sthlm,
            df.vtuna,
            df.vaxholm,
            df.danderyd,
            df.täby)

# df <- rbind(df.vtuna, 
#             df.vaxholm,
#             df.danderyd,
#             df.täby)

# General recode preprocess -----------------------------------------------


#replace all 99* codes with missing (NA)
for (i in 1:ncol(df)) {
  df[,i]<-car::recode(df[,i],"990:999=NA;'<NA>'=NA;'Ej svar'=NA")
}

### sanitize demographic variables, SkolSDO, Årskurs, m.m.
#df %>% distinct(Kön)
df$SkolSDO <- recode(df$SkolSDO,"'Övrigt ospecificerat'=NA;'<NA>'=NA")
df$ARSKURS <- recode(df$ARSKURS,"'Ej svar'=NA;'Åk 7'=NA")
df$Kön <- recode(df$Kön,"'<NA>'=NA")
df$f6a <- recode(df$f6a,"'<NA>'=NA") # Mammas utbildningsnivå
df$f6b <- recode(df$f6b,"'<NA>'=NA") # Pappas utbildningsnivå


# Recoding ----------------------------------------------------------------

## 02 Individfaktorer-----
# koda om svaren för items som ingår i individfaktorer, F66a-F66u i data, fråga 67 i PDF
# variabler df[191:211]
# hög poäng = hög risk
# definiera svarskategorierna för att förenkla recode-koden
smd<-'Stämmer mycket dåligt'
sgd<-'Stämmer ganska dåligt'
sgb<-'Stämmer ganska bra'
smb<-'Stämmer mycket bra'

if.positiva.items <- c("f66h", "f66m", "f66p", "f66u") # create vector with reverse scored items

if.itemlabels <- allAnalyzedItems %>% 
  filter(Index == 'Individfaktorer') %>% 
  select(!Index)

if.negativa.items <- if.itemlabels %>% # vector with items that are not reversed
  filter(!itemnr %in% if.positiva.items) %>%
  pull(itemnr)

### recode IF items
for (i in if.positiva.items) {
  df[[i]]<-car::recode(df[[i]],"smb=0;sgb=1;sgd=2;smd=3",as.factor=FALSE)
}

for (i in if.negativa.items) {
  df[[i]]<-car::recode(df[[i]],"smb=3;sgb=2;sgd=1;smd=0",as.factor=FALSE)
}


## 03 Skola-----

skola.itemlabels <- allAnalyzedItems %>% 
  filter(Index == 'Skola') %>% 
  select(!Index)

#skola.negativa<-names(df[c(156:157,160,162,164,166,168)])
skola.negativa <- c("f54e", "f54f", "f54i", "f54k", "f54m", "f54o", "f54q")
#skola.positiva<-names(df[c(152:155,158:159,161,163,165,167,169)])
skola.positiva <- c("f54a", "f54b", "f54c", "f54d", "f54g", "f54h", "f54j", "f54l", "f54n", "f54p", "f54r")

# for (i in c(152:155,158:159,161,163,165,167,169)) {
#   df[,i]<-recode(df[,i],"smb=0;sgb=1;sgd=2;smd=3",as.factor=FALSE)
# }
# for (i in c(156:157,160,162,164,166,168)) {
#   df[,i]<-recode(df[,i],"smb=3;sgb=2;sgd=1;smd=0",as.factor=FALSE)
# }

for (i in skola.positiva) {
  df[[i]]<-recode(df[[i]],"smb=0;sgb=1;sgd=2;smd=3",as.factor=FALSE)
}

for (i in skola.negativa) {
  df[[i]]<-recode(df[[i]],"smb=3;sgb=2;sgd=1;smd=0",as.factor=FALSE)
}

#responsesF55<-read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/responsesF55.csv", 
#                       fileEncoding = "ISO-8859-1")

for (i in c("F55","F56","F59")){
  df[[i]]<-recode(df[[i]],"'Nej'=0;
               'Ja, 1 gång'=1;
               'Ja, 2-3 gånger'=2;
               'Ja, 4-10 gånger'=3;
               'Ja, 11-20 gånger'=4;
               'Ja, mer än 20 gånger'=5;
               '<NA>'=NA",
                  as.factor=FALSE)
}

df$F61<-recode(df$F61,"'Jag har inte blivit mobbad'=0;
               'Det har hänt någon enstaka gång'=1;
               '2 eller 3 gånger i månaden'=2;
               'Ungefär en gång i veckan'=3;
               'Flera gånger i veckan'=4;
               '<NA>'=NA;
               'Ska ej besvaras'=NA",
               as.factor=FALSE)


## 04 PSF-----

# koda om svaren för items som ingår i psykiska/psykosomatiska besvär, F88-F99 i data, fråga 90-101 i PDF
# variabler df[289:300]
# låg poäng = låg risk 
df$F88<-recode(df$F88,"'Aldrig'=1;'Ungefär 1 gång/termin'=2;'Ungefär 1 gång/månad'=3;'Ungefär 1 gång/vecka'=4;'Flera gånger i veckan'=5",as.factor=FALSE)
df$F89<-recode(df$F89,"'Väldigt ofta'=5;'Ganska ofta'=4;'Ibland'=3;'Någon enstaka gång'=2;'Sällan'=1",as.factor=FALSE)
df$F90<-recode(df$F90,"'Sällan'=1;'Någon enstaka gång'=2;'Ibland'=3;'Ganska ofta'=4;'Väldigt ofta'=5",as.factor=FALSE)
df$F91<-recode(df$F91,"'Aldrig'=1;'Ungefär 1 gång/termin'=2;'Ungefär 1 gång/månad'=3;'Ungefär 1 gång/vecka'=4;'Flera gånger i veckan'=5",as.factor=FALSE)
df$F92<-recode(df$F92,"'Inte alls'=1;'Ganska lite'=2;'En del'=3;'Ganska mycket'=4;'Väldigt mycket'=5",as.factor=FALSE)
df$F93<-recode(df$F93,"'Aldrig'=1;'Ungefär 1 gång/termin'=2;'Ungefär 1 gång/månad'=3;'Ungefär 1 gång/vecka'=4;'Flera gånger i veckan'=5",as.factor=FALSE)
df$F94<-recode(df$F94,"'Väldigt ofta'=5;'Ganska ofta'=4;'Ibland'=3;'Någon enstaka gång'=2;'Nästan aldrig'=1",as.factor=FALSE)
df$F95<-recode(df$F95,"'Aldrig'=1;'Ungefär 1 kväll/termin'=2;'Ungefär 1 kväll/månad'=3;'Ungefär 1 kväll/vecka'=4;'Flera gånger i veckan'=5;'Flera kvällar i veckan'=5",as.factor=FALSE)
df$F96<-recode(df$F96,"'Nästan aldrig'=5;'Någon enstaka gång'=4;'Ibland'=3;'Ganska ofta'=2;'Oftast'=1",as.factor=FALSE)
df$F97<-recode(df$F97,"'Sällan'=1;'Någon enstaka gång'=2;'Ibland'=3;'Ganska ofta'=4;'Väldigt ofta'=5",as.factor=FALSE)
df$F98<-recode(df$F98,"'Aldrig'=1;'Ungefär 1 natt/termin'=2;'Ungefär 1 natt/månad'=3;'Ungefär 1 natt/vecka'=4;'Flera nätter i veckan'=5",as.factor=FALSE)
df$F99<-recode(df$F99,"'Sällan'=5;'Någon enstaka gång'=4;'Ibland'=3;'Ganska ofta'=2;'Väldigt ofta'=1",as.factor=FALSE)

# flytta ankare från 1 till 0 för lägsta kategori, behövs för vissa Rasch-program
psfitems <- allAnalyzedItems %>% 
  filter(Index == 'Psykiska/psykosomatiska besvär') %>% 
  pull(itemnr)
# flytta ankare från 1 till 0 för lägsta kategori, behövs för vissa Rasch-program
for (i in psfitems) {
  df[[i]]<-recode(df[[i]],"1=0;2=1;3=2;4=3;5=4",as.factor=FALSE)
}


## 05 Föräldrafrågor-----

# 1. Anknytning - anknytningindex=mean(pf83g,pf83h,pf82)
# 2. Uppmärksamhet - uppmarksamindex=mean(pf83a,pf83c,pf83e)
# 3. Inkonsekvens – inkonsekvensindex=mean(pf83b,pf83d,pf83f)
# 4. Föräldrakontroll - forkontrollindex=mean(pf79,pf80,pf81)
# f82 också intressant: "Om du har ett personligt problem, kan du be någon av dina föräldrar/vårdnadshavare om hjälp?" (ja, nej, vet inte)
# Fråga i PDF som borde vara med? 72. Vem bestämmer när du ska vara hemma på kvällen?

# 81-84, and 85 (multiple questions) in PDF file

df$F79<-recode(df$F79,"'Alltid'=0;
               'Ibland'=1;
               'Sällan'=2;
               'Aldrig'=3;
               'Vet inte'=NA;
               '<NA>'=NA",
               as.factor=FALSE)

df$F80<-recode(df$F80,"'Ja, alla'=0;
               'Ja, de allra flesta'=1;
               'Ja, några'=2;
               'Nej, ingen'=3;
               'Vet inte'=NA;
               '<NA>'=NA",
               as.factor=FALSE)

df$F81<-recode(df$F81,"'Ja, helt och hållet'=0;
               'Ja, till större delen'=1;
               'Ja, en liten del'=2;
               'Nej, inte alls'=3;
               'Vet inte'=NA;
               '<NA>'=NA",
               as.factor=FALSE)

df$F82<-recode(df$F82,"'Ja'=0;
               'Nej'=1;
               'Vet inte'=NA;
               '<NA>'=NA",
               as.factor=FALSE)

df$f83a<-recode(df$f83a,"'Stämmer mycket bra'=0;
               'Stämmer ganska bra'=1;
               'Stämmer ganska dåligt'=2;
               'Stämmer mycket dåligt'=3;
               '<NA>'=NA",
                as.factor=FALSE)
df$f83b<-recode(df$f83b,"'Stämmer mycket bra'=3;'Stämmer ganska bra'=2;'Stämmer ganska dåligt'=1;'Stämmer mycket dåligt'=0;
              '<NA>'=NA",as.factor=FALSE)
df$f83c<-recode(df$f83c,"'Stämmer mycket bra'=0;'Stämmer ganska bra'=1;'Stämmer ganska dåligt'=2;'Stämmer mycket dåligt'=3;
              '<NA>'=NA",as.factor=FALSE)
df$f83d<-recode(df$f83d,"'Stämmer mycket bra'=3;'Stämmer ganska bra'=2;'Stämmer ganska dåligt'=1;'Stämmer mycket dåligt'=0;
              '<NA>'=NA",as.factor=FALSE)
df$f83e<-recode(df$f83e,"'Stämmer mycket bra'=0;'Stämmer ganska bra'=1;'Stämmer ganska dåligt'=2;'Stämmer mycket dåligt'=3;
              '<NA>'=NA",as.factor=FALSE)
df$f83f<-recode(df$f83f,"'Stämmer mycket bra'=3;'Stämmer ganska bra'=2;'Stämmer ganska dåligt'=1;'Stämmer mycket dåligt'=0;
              '<NA>'=NA",as.factor=FALSE)
df$f83g<-recode(df$f83g,"'Stämmer mycket bra'=0;'Stämmer ganska bra'=1;'Stämmer ganska dåligt'=2;'Stämmer mycket dåligt'=3;
               '<NA>'=NA", as.factor=FALSE)
df$f83h<-recode(df$f83h,"'Stämmer mycket bra'=0;'Stämmer ganska bra'=1;'Stämmer ganska dåligt'=2;'Stämmer mycket dåligt'=3;
               '<NA>'=NA", as.factor=FALSE)

## 06 Kamrater och fritid, samt prosocialt index-----

#Prosocialt index	F70, f86a c f
#Kamrater	f86b d e ghij
# fritidsfrågor

### Från syntax-filen från Sthlm Stad
# ** Prosocialt index. Baseras på:
# Fråga 70) deltagande i någon ledarledd fritisaktivitet eller träning,
# Fråga 86a) Motionerar och tränar dina kamrater regelbundet?
# 86c) Är kamraterna med i någon förening?
# 86f) Är kamraterna duktiga i skolan?
# Ett högt indexvärde indikerar att man har "goda sociala förutsättningar".
#kamrater.items <- names(df[c(1,8,10,11,215,278:287)])

# 70 i PDF, F69 i data, ingår inte enl spec, men relevant?
# "Brukar du vara på fritidsgård eller ”träffpunkt”?"
# OBS oklart hur detta item ska kodas
#df$F69<-recode(df$F69,"'Ofta'=0;'Ibland'=1;'Sällan'=2;'Aldrig'=3",as.factor=FALSE)

# lärarledd fritidsaktivitet, 71 i PDF, F70 i data
# "Brukar du delta i någon ledarledd fritidsaktivitet eller träning?"
df$F70<-recode(df$F70,"'Ofta'=0;'Ibland'=1;'Sällan'=2;'Aldrig'=3",as.factor=FALSE)


# Item 88 in PDF file, "Hur många av dina kamrater (inom och utom skolan)"
# f86a-j in datafile

df$f86a<-recode(df$f86a,"'Ingen'=3;
               'Någon enstaka'=2;
               'Ungefär hälften'=1;
               'De flesta'=0;
               'Vet inte'=NA;
               '<NA>'=NA",
                as.factor=FALSE)

df$f86b<-recode(df$f86b,"'Ingen'=0;
               'Någon enstaka'=1;
               'Ungefär hälften'=2;
               'De flesta'=3;
               'Vet inte'=NA;
               '<NA>'=NA",
                as.factor=FALSE)

df$f86c<-recode(df$f86c,"'Ingen'=3;'Någon enstaka'=2;'Ungefär hälften'=1;'De flesta'=0;'Vet inte'=NA;'<NA>'=NA",as.factor=FALSE)
df$f86d<-recode(df$f86d,"'Ingen'=0;'Någon enstaka'=1;'Ungefär hälften'=2;'De flesta'=3;'Vet inte'=NA;'<NA>'=NA",as.factor=FALSE)
df$f86e<-recode(df$f86e,"'Ingen'=0;'Någon enstaka'=1;'Ungefär hälften'=2;'De flesta'=3;'Vet inte'=NA;'<NA>'=NA",as.factor=FALSE)
df$f86f<-recode(df$f86f,"'Ingen'=3;'Någon enstaka'=2;'Ungefär hälften'=1;'De flesta'=0;'Vet inte'=NA;'<NA>'=NA",as.factor=FALSE)
df$f86g<-recode(df$f86g,"'Ingen'=0;'Någon enstaka'=1;'Ungefär hälften'=2;'De flesta'=3;'Vet inte'=NA;'<NA>'=NA",as.factor=FALSE)
df$f86h<-recode(df$f86h,"'Ingen'=0;'Någon enstaka'=1;'Ungefär hälften'=2;'De flesta'=3;'Vet inte'=NA;'<NA>'=NA",as.factor=FALSE)
df$f86i<-recode(df$f86i,"'Ingen'=0;'Någon enstaka'=1;'Ungefär hälften'=2;'De flesta'=3;'Vet inte'=NA;'<NA>'=NA",as.factor=FALSE)
df$f86j<-recode(df$f86j,"'Ingen'=0;'Någon enstaka'=1;'Ungefär hälften'=2;'De flesta'=3;'Vet inte'=NA;'<NA>'=NA",as.factor=FALSE)

## 07 Närsamhälle omkodning-----

# Frågorna heter f101a till f101l i datafilen och 103 i PDF-filen.
# samt F100 i datafilen som är 102 i PDF-filen.
# hög poäng = hög risk

# definiera svarskategorierna för att förenkla recode-koden
smd<-'Stämmer mycket dåligt'
sgd<-'Stämmer ganska dåligt'
sgb<-'Stämmer ganska bra'
smb<-'Stämmer mycket bra'
vetej<-'Vet inte' # kodas som missing/NA
mtrygg<-'Mycket trygg'
gtrygg<-'Ganska trygg'
gotrygg<-'Ganska otrygg'
motrygg<-'Mycket otrygg'
garej1<-'Går ej ut på kvällen av oro för att utsättas för brott' # kodas som missing/NA pga ej användbart i ordinala data. Skulle ev. kunna ses som likvärdigt som Mycket Otrygg, eller som ännu "värre", ordinalt ett steg över.
garej2<-'Går ej ut på kvällen av andra orsaker' # kodas som missing/NA pga ej användbart i ordinala data

df$F100<-car::recode(df$F100,"mtrygg=0;gtrygg=1;gotrygg=2;motrygg=3;garej1=NA;garej2=NA",as.factor=FALSE)

ns.positiva.items <- c("f101b","f101c","f101g","f101h","f101i","f101j","f101k","f101l")
ns.negativa.items <- c("f101a","f101d","f101e","f101f")
ns.items <- c(ns.positiva.items,ns.negativa.items,"F100")

for (i in ns.positiva.items) {
  df[[i]]<-car::recode(df[[i]],"smb=0;sgb=1;sgd=2;smd=3;vetej=NA",as.factor=FALSE)
}
for (i in ns.negativa.items) {
  df[[i]]<-car::recode(df[[i]],"smb=3;sgb=2;sgd=1;smd=0;vetej=NA",as.factor=FALSE)
}


# Re-recoding based on psychometric evaluations  ----------------------------------
# fixa svarskategorier, item-eliminering och slutgiltiga item-uppsättningar


## 02 Individfaktorer-----

df$f66p<-recode(df$f66p,"2=1;3=2", as.factor = F)

##----- 03 Skola-----

# positiva
df$f54a<-recode(df$f54a,"3=2",as.factor=FALSE)
df$f54b<-recode(df$f54b,"3=2",as.factor=FALSE)
# negativa
df$f54o<-recode(df$f54o,"2=1;3=2",as.factor=FALSE)
df$F61 <- recode(df$F61,"3=2;4=2",as.factor=F)

## 04 PSF-----

rcat1 <- c("F89", "F90", "F91")
rcat2 <- c("F94","F95","F97")
rcat3 <- c("F92","F93","F96","F98","F99")

for (i in rcat1) {
  df[[i]]<-recode(df[[i]],"2=1;3=2;4=2",as.factor=FALSE)
}
for (i in rcat2) {
  df[[i]]<-recode(df[[i]],"1=0;2=1;3=2;4=3",as.factor=FALSE)
}
for (i in rcat3) {
  df[[i]]<-recode(df[[i]],"4=3",as.factor=FALSE)
}

## 05 Föräldrafrågor-----

rcat1 <- c("F79", "f83a", "f83e", "f83g")

for (i in rcat1) {
  df[[i]]<-recode(df[[i]],"3=2",as.factor=FALSE)
}

# 59. Hur skulle dina föräldrar reagera om du hade skolkat?

df$F58<-recode(df$F58,"'De skulle reagera mycket kraftigt'=0;
               'De skulle reagera ganska mycket'=1;
               'De skulle inte reagera så mycket'=2;
               'De skulle inte reagera alls'=3;
               'Vet inte'=NA;
               '<NA>'=NA",
               as.factor=FALSE)


## 07 Närsamhälle-----

recoded.ordning <- c("f101g","f101j")

for (i in recoded.ordning) {
  df[[i]]<-car::recode(df[[i]],"2=1;3=2",as.factor=FALSE)
}

## Välbefinnande ----

df$F70 <- recode(df$F70,"1=0;2=1;3=1",as.factor=FALSE)

# 68. Tycker du att det är viktigt vad du kommer att jobba med när du blir stor eller spelar det ingen roll?
df$F67<-recode(df$F67,"'Är mycket viktigt'=0;
               'Är ganska viktigt'=1;
               'Är varken viktigt eller oviktigt'=2;
               'Spelar nästan ingen roll'=3;
               'Spelar ingen roll alls'=4;
               '<NA>'=NA",
               as.factor=FALSE)
df$F67 <- recode(df$F67,"4=2;3=2", as.factor=F)

#  69. Om du jämför dina framtidsutsikter med de flesta andras i din ålder, tror du då att dina är sämre, lika bra
# eller bättre?
df$F68<-recode(df$F68,"'Mycket bättre'=0;
               'Lite bättre'=1;
               'Lika bra'=2;
               'Lite sämre'=3;
               'Mycket sämre'=4;
               '<NA>'=NA",
               as.factor=FALSE)
df$F68 <- recode(df$F68,"4=3", as.factor=F)


# Write new datafile ------------------------------------------------------

write_parquet(df, sink = glue("../data/{Sys.Date()}_recodedData.parquet"))
