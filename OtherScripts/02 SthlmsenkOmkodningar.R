library(foreign)
library(car)
library(tidyverse)
library(glue)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

# General recode preprocess -----------------------------------------------

### NOTE:
# if you need to retain any variable as it is in the raw unprocessed data, 
# you should make a copy of it (with a new variable name) prior to running the code below.

#replace all 99* codes with missing (NA)
for (i in 1:ncol(df)) {
  df[,i]<-car::recode(df[,i],"990:999=NA;'<NA>'=NA;'Ej svar'=NA")
}

### sanitize demographic variables, SkolSDO, Årskurs, m.m.
#df %>% distinct(Kön)
df$SkolSDO <- recode(df$SkolSDO,"'Övrigt ospecificerat'=NA;'<NA>'=NA")
df$ARSKURS <- recode(df$ARSKURS,"'Ej svar'=NA;'Åk 7'=NA")
df$Kön <- recode(df$Kön,"'<NA>'=NA")
df$f6a <- recode(df$f6a,"'<NA>'=NA;'Vet inte'=NA") # Mammas utbildningsnivå
df$f6b <- recode(df$f6b,"'<NA>'=NA;'Vet inte'=NA") # Pappas utbildningsnivå

### Create new single variable for highest parent education level
# first set factor levels
df <- df %>% 
  mutate(f6a = factor(f6a, levels = c("Folkskola eller grundskola (max 9 år i skolan)",
                                      "Gymnasium",
                                      "Universitet och högskola")),
         f6b = factor(f6b, levels = c("Folkskola eller grundskola (max 9 år i skolan)",
                                      "Gymnasium",
                                      "Universitet och högskola"))
  )

# create numerical variable to enable logical comparisons
df <- df %>% 
  mutate(f6aNum = car::recode(f6a,"'Folkskola eller grundskola (max 9 år i skolan)'=0;
                              'Gymnasium'=1;
                              'Universitet och högskola'=2", as.factor = F),
         f6bNum = car::recode(f6b,"'Folkskola eller grundskola (max 9 år i skolan)'=0;
                              'Gymnasium'=1;
                              'Universitet och högskola'=2", as.factor = F)
  )

# create new composite variable
df <- df %>% 
  mutate(f6ab = case_when(f6aNum > f6bNum ~ f6a,
                          f6aNum < f6bNum ~ f6b,
                          f6aNum == f6bNum ~ f6a,
                          is.na(f6aNum) ~ f6b,
                          is.na(f6bNum) ~ f6a),
         .after = "F5")
# clean up, removing variables not needed
df$f6aNum <- NULL
df$f6bNum <- NULL
df$f6a <- NULL
df$f6b <- NULL


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

## 08 Mobbning ----------------------------------------------------------------

# 61. Har du känt dig mobbad eller trakasserad i skolan det här läsåret?
# f60a till i, antingen förkryssad eller inte

items.mobbning <- df %>% 
  select(starts_with("f60")) %>% 
  names()

for (i in items.mobbning) {
  df[[i]] <- recode(df[[i]],"'Chosen'=1;
                    'Not chosen'=0",
                    as.factor = FALSE)
}

# 62. Hur ofta har du blivit mobbad eller trakasserad i skolan det här läsåret?
# 64. Har du blivit mobbad eller trakasserad via internet eller SMS/MMS det här läsåret?

df$F61<-recode(df$F61,"'Jag har inte blivit mobbad'=0;
               'Det har hänt någon enstaka gång'=1;
               '2 eller 3 gånger i månaden'=2;
               'Ungefär en gång i veckan'=3;
               'Flera gånger i veckan'=4;
               '<NA>'=NA",
               as.factor=FALSE)

df$F63<-recode(df$F63,"'Ja'=1;
               'Nej'=0;
               'Vet inte'=NA;
               '<NA>'=NA",
               as.factor=FALSE)

## 09 ANDTS ----------------------------------------------------------------
itemsANDTSegen <- c("F14","FNY12020","F18","F34","F41","F47","F48","f53a","F73")
itemsANDTSdebut <- c("F16","F20","F37","F44","F51")
itemsANDTSfldr <- c("F17","F21","f22a","F40","FNY22020")

df$F14 <- recode(df$F14,"'Nej, jag har aldrig rökt'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har rökt men slutat'=2;
                 'Ja, ibland men inte varje dag'=3;
                 'Ja, dagligen'=4;
                 '<NA>'=NA", 
                 as.factor = F)
df$F14r <- recode(df$F14,"0:2=0;3=1;4=2", as.factor = F) # possible part of frequency based index

df$FNY12020 <- recode(df$FNY12020,"'Nej, jag har aldrig rökt e-cigaretter'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har rökt e-cigaretter men slutat'=2;
                 'Ja, ibland men inte varje dag'=3;
                 'Ja, dagligen'=4;
                 'Ska ej besvaras'=NA;
                 '<NA>'=NA", as.factor = F)

df$FNY12020r <- recode(df$FNY12020,"0:2=0;3=1;4=2", as.factor = F)

df$F18 <- recode(df$F18,"'Nej, jag har aldrig snusat'=0;
                 'Nej, bara provat hur det smakar'=1;
                 'Nej, jag har snusat men slutat'=2;
                 'Nej, jag har slutat'=2;
                 'Ja, ibland men inte varje dag'=3;
                 'Ja, dagligen'=4;
                 '<NA>'=NA", 
                 as.factor = F)

df$F18r <- recode(df$F18,"0:2=0;3=1;4=2", as.factor = F)

df$F34 <- recode(df$F34,"'Dricker inte alkohol'=0;
                 'Aldrig'=0;
                 'Ytterst sällan'=1;
                 'Någon gång per år'=2;
                 'Någon gång i månaden'=3;
                 'Ett par gånger i månaden'=4;
                 'Någon gång i veckan'=5;
                 '<NA>'=NA", 
                 as.factor = F)

df$F41 <- recode(df$F41,"'Nej, ingen gång'=0;
                 'Ja, 1 gång'=1;
                 'Ja, 2-4 gånger'=2;
                 'Ja, 5-10 gånger'=3;
                 'Ja, 11-20 gånger'=4;
                 'Ja, 21-50 gånger'=5;
                 'Ja, mer än 50 gånger'=6;
                 '<NA>'=NA", 
                 as.factor = F)

df$F47 <- recode(df$F47,"'Ingen gång'=0;
                 '1 gång'=1;
                 '2-4 gånger'=2;
                 '5-10 gånger'=3;
                 '11-20 gånger'=4;
                 '21-50 gånger'=5;
                 'Mer än 50 gånger'=6;
                 '<NA>'=NA", 
                 as.factor = F)

df$F48 <- recode(df$F48,"'Ingen gång'=0;
                 '1 gång'=1;
                 '2-4 gånger'=2;
                 '5-10 gånger'=3;
                 '11-20 gånger'=4;
                 '21-50 gånger'=5;
                 'Mer än 50 gånger'=6;
                 '<NA>'=NA", 
                 as.factor = F)

# f53-frågorna handlar om huruvida man sökt hjälp, och vi skiljer ej på var man sökt hjälp, 
# bara om man sökt hjälp (1) eller ej (0)
df <- df %>% 
  mutate(f53 = case_when(
    f53a == "Chosen" ~ 0,
    f53b == "Chosen" ~ 1,
    f53c == "Chosen" ~ 1,
    f53d == "Chosen" ~ 1,
    f53e == "Chosen" ~ 1,
    f53f == "Chosen" ~ 1,
    TRUE ~ NA_real_)
  )

df$F73 <- recode(df$F73,"'Har inte spelat de senaste 30 dagarna'=0;
                 'Mindre än 50 kronor'=1;
                 '50-99 kronor'=2;
                 '100-199 kronor'=3;
                 '200-299 kronor'=4;
                 '300-399 kronor'=5;
                 '400 kronor eller mer'=6;
                 '<NA>'=NA", 
                 as.factor = F)


# c("F17","F21","f22a","F40","FNY22020")

df$F17 <- recode(df$F17,"'Nej'=0;
                 'Ja'=2;
                 'Vet inte'=1;
                 '<NA>'=NA", 
                 as.factor = F)

df$F21 <- recode(df$F21,"'Nej'=0;
                 'Ja'=2;
                 'Vet inte'=1;
                 '<NA>'=NA", 
                 as.factor = F)

df$F40 <- recode(df$F40,"'Nej'=0;
                 'Ja'=2;
                 'Vet inte'=1;
                 '<NA>'=NA", 
                 as.factor = F)

# f22 har kodats så att Nej = 0, Vet ej = 1, och sedan ökande nivå för varje person som kryssats för, d.v.s. max 5

df$f22b <- recode(df$f22b,"'Chosen'=1;
                 'Not chosen'=0;
                 '<NA>'=NA", 
                  as.factor = F)
df$f22c <- recode(df$f22c,"'Chosen'=1;
                 'Not chosen'=0;
                 '<NA>'=NA", 
                  as.factor = F)
df$f22d <- recode(df$f22d,"'Chosen'=1;
                 'Not chosen'=0;
                 '<NA>'=NA", 
                  as.factor = F)
df$f22e <- recode(df$f22e,"'Chosen'=1;
                 'Not chosen'=0;
                 '<NA>'=NA", 
                  as.factor = F)
df <- df %>% 
  mutate(f22 = case_when(
    f22a == "Chosen" ~ 0,
    f22f == "Chosen" ~ 1,
    (f22b+f22c+f22d+f22e) == 1 ~ 2,
    (f22b+f22c+f22d+f22e) == 2 ~ 3,
    (f22b+f22c+f22d+f22e) == 3 ~ 4,
    (f22b+f22c+f22d+f22e) == 4 ~ 5,
    TRUE ~ NA_real_
  ))

df$FNY22020 <- recode(df$FNY22020,"'Nej'=0;
                 'Ja'=2;
                 'Vet inte'=1;
                 '<NA>'=NA", 
                      as.factor = F)


# 37. Hur många gånger har du druckit så mycket alkohol att du känt dig berusad under den senaste 4-veckorsperioden?
# df %>% count(as.numeric(df$f36)) visar en konstig fördelning, vi kodar om den enligt nedan
df$f36r <- recode(as.numeric(as.character(df$f36)),"4:12=3;13:31=NA")
# 50. Hur många gånger har du använt narkotika (cannabis eller annan narkotika) den senaste 4-veckors perioden?
df$F49r <- recode(as.numeric(as.character(df$F49)),"4:12=3;13:100=NA")

# 36. Hur många gånger har du druckit så mycket alkohol att du känt dig berusad?
# 1) Ingen gång   gå till fråga 39
# Vi vill koda om F35, svar "Ingen gång" till att motsvara 0 för f36r
# Samma gäller för F45 om narkotika inkl cannabis, med F49 om senaste 4v.
df <- df %>% 
  mutate(F35r = case_when(F35 == "Ingen gång" ~ 0,
                          TRUE ~ NA_real_
  )) %>% 
  mutate(F36new = coalesce(f36r,F35r)) %>% 
  mutate(F45r = case_when(F45 == "Nej" ~ 0,
                          TRUE ~ NA_real_
  )) %>% 
  mutate(F49new = coalesce(F49r,F45r))


# ta fram ett indexvärde för bruk under senaste 4v
senaste4v <- c("F14r","F18r","F36new","F49new","FNY12020r")
df <- df %>% 
  mutate(Senaste4v = rowSums(df %>% select(all_of(senaste4v)), na.rm = T))

df <- df %>% 
  mutate(across(itemsANDTSdebut, as.character))

df <- df %>% 
  mutate(across(itemsANDTSdebut, as.numeric))


## 10 Brott/kriminalitet ---------------------------------------------------

# brott, items f75a till s (fråga 77 i PDF)
# "Hur många gånger har du gjort följande saker under de senaste 12 månaderna?"

items.brott <- df %>% 
  select(starts_with("f75")) %>% 
  names()

for (i in items.brott) {
  df[[i]] <- recode(df[[i]],"'Ingen gång'=0;
                    '1-2 gånger'=1;
                    '3-5 gånger'=2;
                    '6-10 gånger'=3;
                    'Mer än 10 gånger'=4",
                    as.factor = FALSE)
}

# utsatt för brott, 80 i PDF, f78aa till f78ea

items.brott2 <- df %>% 
  select(starts_with("f78")) %>% 
  select(ends_with("a")) %>% 
  names()

for (i in items.brott2) {
  df[[i]] <- recode(df[[i]],"'Nej'=0;
                    'Ja, antal gånger'=1;
                    'Ja'=1",
                    as.factor = FALSE)
}

#write_parquet(df, sink = glue("../data/{Sys.Date()}_recodedData1.parquet"))
