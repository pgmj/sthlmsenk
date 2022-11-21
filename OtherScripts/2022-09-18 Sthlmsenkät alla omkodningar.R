

library(arrow)
library(car)
library(grateful)
library(kableExtra)
library(readxl)
library(tidyverse)
library(eRm)
library(mirt)
library(psych)
library(ggplot2)
library(psychotree)
library(matrixStats)
library(reshape)
library(knitr)
library(cowplot)
library(formattable)
library(RISEkbmRasch)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

setwd("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022")


#----- Create item information dataframe-----

# read item information from all separate analyses
individf<-read_excel("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/IF_itemlabels.xls")
skola<-read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/Skola_itemlabels.csv")
psf<-read_excel("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/PSFitems.xls")
parenting<-read_excel("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/PARENTSitems.xls")
kamrfritid<-read_excel("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/KAMRATERitems.xls")
nsamh<-read_excel("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/NärsamhälleItemlabels.xls")

# add index variable
individf <- individf %>% add_column(Index = "Individfaktorer")
skola <- skola %>% add_column(Index = "Skola")
psf <- psf %>% add_column(Index = "Psykiska/psykosomatiska besvär")
parenting <- parenting %>% add_column(Index = "Föräldraskap")
kamrfritid <- kamrfritid %>% add_column(Index = "Kamrater och fritid")
nsamh <- nsamh %>% add_column(Index = "Närsamhälle")

# write all item data to the same file
allitems<-rbind(individf,skola,psf,parenting,kamrfritid,nsamh)
library(xlsx)
write.xlsx(allitems, file = "C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/allitems.xlsx")
write.csv(allitems, file = "C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/allitems.csv", row.names = F)



#----- read item/variable information-----
allitems <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/allitems.csv")



#----- Import all data-----
library(foreign)
df.1420 <- read.spss("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/SE 2014-2020 KI Leifman.sav", to.data.frame = TRUE)
df.0612 <- read.spss("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2006-2012 Stockholmsenkäten 201126.sav", to.data.frame = TRUE)
df<-rbind(df.0612,df.1420)

#replace all 99* codes with missing (NA)
for (i in 1:ncol(df)) {
  df[,i]<-car::recode(df[,i],"990:999=NA;'<NA>'=NA;'Ej svar'=NA")
}

### sanitize demographic variables, SkolSDO, Årskurs, m.m.
df %>% distinct(Kön)
df$SkolSDO <- recode(df$SkolSDO,"'Övrigt ospecificerat'=NA;'<NA>'=NA")
df$ARSKURS <- recode(df$ARSKURS,"'Ej svar'=NA;'Åk 7'=NA")
df$Kön <- recode(df$Kön,"'<NA>'=NA")
df$f6a <- recode(df$f6a,"'<NA>'=NA") # Mammas utbildningsnivå
df$f6b <- recode(df$f6b,"'<NA>'=NA") # Pappas utbildningsnivå

###

#----- Recoding character data to numeric-----


#----- 02 Individfaktorer-----
# koda om svaren för items som ingår i individfaktorer, F66a-F66u i data, fråga 67 i PDF
# variabler df[191:211]
# hög poäng = hög risk
# definiera svarskategorierna för att förenkla recode-koden
smd<-'Stämmer mycket dåligt'
sgd<-'Stämmer ganska dåligt'
sgb<-'Stämmer ganska bra'
smb<-'Stämmer mycket bra'

if.positiva.items <- c("f66h", "f66m", "f66p", "f66u") # create vector with reverse scored items

if.itemlabels <- allitems %>% 
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

#if.removed.items <- c("f66d","f66j","f66p","f66l","f66b","f66t","f66q","f66a","f66g","f66e","f66h")

###

#----- 03 Skola-----

skola.itemlabels <- allitems %>% 
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

###


#----- 04 PSF-----

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
df$F95<-recode(df$F95,"'Aldrig'=1;'Ungefär 1 kväll/termin'=2;'Ungefär 1 kväll/månad'=3;'Ungefär 1 kväll/vecka'=4;'Flera gånger i veckan'=5",as.factor=FALSE)
df$F96<-recode(df$F96,"'Nästan aldrig'=5;'Någon enstaka gång'=4;'Ibland'=3;'Ganska ofta'=2;'Oftast'=1",as.factor=FALSE)
df$F97<-recode(df$F97,"'Sällan'=1;'Någon enstaka gång'=2;'Ibland'=3;'Ganska ofta'=4;'Väldigt ofta'=5",as.factor=FALSE)
df$F98<-recode(df$F98,"'Aldrig'=1;'Ungefär 1 natt/termin'=2;'Ungefär 1 natt/månad'=3;'Ungefär 1 natt/vecka'=4;'Flera nätter i veckan'=5",as.factor=FALSE)
df$F99<-recode(df$F99,"'Sällan'=5;'Någon enstaka gång'=4;'Ibland'=3;'Ganska ofta'=2;'Väldigt ofta'=1",as.factor=FALSE)

psfitems <- allitems %>% 
  filter(Index == 'Psykiska/psykosomatiska besvär') %>% 
  pull(itemnr)
# flytta ankare från 1 till 0 för lägsta kategori, behövs för vissa Rasch-program
for (i in psfitems) {
  df[[i]]<-recode(df[[i]],"1=0;2=1;3=2;4=3;5=4",as.factor=FALSE)
}

###

#----- 05 Föräldrafrågor-----

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

parents.negativa.items <- c("f83b","f83d","f83f")
parents.itemlabels <- allitems %>% 
  filter(Index == 'Föräldraskap') %>% 
  select(!Index)


#----- 06 Kamrater och fritid, samt prosocialt index-----

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

#positiva.items <- c("f88a","f86c","f86f")



#----- 07 Närsamhälle omkodning-----

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

write_parquet(df, sink = "2022-08-22 sthlmsenkat data.parquet") # 8.3mb
#write_csv(df, file = "2022-08-19 sthlmsenkat data.csv") # 287mb




#----- Samlade omkodningar av svarskategorier, sammanställning av item-eliminering och slutgiltiga item-uppsättningar-----

# read data (recoded above, saved in file to save time)
df <- read_parquet("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2022-08-22 sthlmsenkat data.parquet")
allitems <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/allitems.csv")


#----- 02 Individfaktorer-----

#df$f66p<-recode(df$f66p,"2=1;3=2", as.factor = F)

if.normb.items <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/rapporter/02 Individfaktorer/2022-09-16 IFoptimalItems.csv")
if.normb.items <- if.normb.items$x

if.pos.items <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/rapporter/02 Individfaktorer/2022-09-18 IFpositiveItems.csv")
if.pos.items <- if.pos.items$x

# if.itemlabels <- allitems %>% 
#   filter(Index == 'Individfaktorer') %>% 
#   select(!Index)
# 
# if.removed.items <- if.itemlabels %>% 
#   filter(!itemnr %in% c(if.normb.items,if.pos.items)) %>% 
#   pull(itemnr)



#----- 03 Skola-----

df$f54a<-recode(df$f54a,"3=2",as.factor=FALSE)
df$f54b<-recode(df$f54b,"3=2",as.factor=FALSE)
df$f54o<-recode(df$f54o,"2=1;3=2",as.factor=FALSE)

skola.pos.items <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/rapporter/03 Skola/SkolaPositiva.csv")
skola.pos.items <- skola.pos.items$itemnr

skola.neg.items <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/rapporter/03 Skola/SkolaNegativa.csv")
skola.neg.items <- skola.neg.items$itemnr

# skola.itemlabels <- allitems %>% 
#   filter(Index == 'Skola') %>% 
#   select(!Index)
# 
# skola.removed.items <- skola.itemlabels %>% 
#   filter(!itemnr %in% c(skola.pos.items,skola.neg.items)) %>% 
#   pull(itemnr)

df$F61<-recode(df$F61,"'Jag har inte blivit mobbad'=0;
               'Det har hänt någon enstaka gång'=1;
               '2 eller 3 gånger i månaden'=2;
               'Ungefär en gång i veckan'=2;
               'Flera gånger i veckan'=2;
               '<NA>'=NA;
               'Ska ej besvaras'=NA",
               as.factor=FALSE)


#----- 04 PSF-----

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

psf.final.items <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/rapporter/04 PSF/2022-08-23 PSFfinalitems.csv")
psf.final.items <- psf.final.items$x

#psf.removed.items <- c("F90","F96","F94","F89","F91","F98")


###


#----- 05 Föräldrafrågor-----

rcat1 <- c("F79", "f83a", "f83e", "f83g")

for (i in rcat1) {
  df[[i]]<-recode(df[[i]],"3=2",as.factor=FALSE)
}

parent.final.items <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/rapporter/05 Parenting/2022-09-18 ParentingFinalitems.csv")
parent.final.items <- parent.final.items$itemnr

# parents.allitems <- allitems %>% 
#   filter(Index == 'Föräldraskap')
# 
# parent.removed.items <- parents.itemlabels %>% 
#   filter(!itemnr %in% parent.final.items) %>% 
#   pull(itemnr)

# 59. Hur skulle dina föräldrar reagera om du hade skolkat?

df$F58<-recode(df$F58,"'De skulle reagera mycket kraftigt'=0;
               'De skulle reagera ganska mycket'=1;
               'De skulle inte reagera så mycket'=2;
               'De skulle inte reagera alls'=3;
               'Vet inte'=NA;
               '<NA>'=NA",
               as.factor=FALSE)

###

#----- 06 Kamrater och fritid-----

# rcat1 <- df %>% 
#   select(!F70) %>% 
#   names()
# rcat1 <- c("f86a", "f86b", "f86c", "f86d", "f86e", "f86f", "f86g", "f86h", "f86i", "f86j", "F87")
# 
# for (i in rcat1) {
#   df[[i]]<-recode(df[[i]],"2=1;3=2",as.factor=FALSE)
# }
# 
df$F70<-recode(df$F70,"1=0;2=1;3=1",as.factor=FALSE)



#----- 07 Närsamhälle-----

recoded.ordning <- c("f101g","f101j")

for (i in recoded.ordning) {
  df[[i]]<-car::recode(df[[i]],"2=1;3=2",as.factor=FALSE)
}

ns.final.items <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/rapporter/07 Community/2022-09-16 NSAMFinalitems.csv")
ns.final.items <- ns.final.items$x

# ns.allitems <- allitems %>% 
#   filter(Index == 'Närsamhälle')
# 
# ns.removed.items <- ns.allitems %>% 
#   filter(!itemnr %in% ns.final.items) %>% 
#   pull(itemnr)

#---- 08 Välbefinnande----
# 68. Tycker du att det är viktigt vad du kommer att jobba med när du blir stor eller spelar det ingen roll?
df$F67<-recode(df$F67,"'Är mycket viktigt'=0;
               'Är ganska viktigt'=1;
               'Är varken viktigt eller oviktigt'=2;
               'Spelar nästan ingen roll'=3;
               'Spelar ingen roll alls'=4;
               '<NA>'=NA",
               as.factor=FALSE)

#  69. Om du jämför dina framtidsutsikter med de flesta andras i din ålder, tror du då att dina är sämre, lika bra
# eller bättre?
df$F68<-recode(df$F68,"'Mycket bättre'=0;
               'Lite bättre'=1;
               'Lika bra'=2;
               'Lite sämre'=3;
               'Mycket sämre'=4;
               '<NA>'=NA",
               as.factor=FALSE)

df$F67 <- recode(df$F67,"4=2;3=2", as.factor=F)
df$F68 <- recode(df$F68,"4=3", as.factor=F)

###

# write data file with recoded responses
write_parquet(df, sink = "C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2022-09-24 sthlmsenkat recoded responses.parquet") # 8.3mb

##### read item/variable information
#allitems <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/allitems.csv")

# create df with final items for all indices
# final.allitems <- allitems %>% 
#   filter(itemnr %in% c(if.pos.items,if.normb.items,skola.pos.items,skola.neg.items,psf.final.items,parent.final.items,ns.final.items))
# 
# write_csv(final.allitems, file = "C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2022-09-18_FINALallitems.csv")


####
####
####

#---- Adding potential indices----

# read all data, with items from indices 01-07 already recoded
##################### this file has encoding error for f66q:
#dftest <- read_parquet("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2022-08-23 sthlmsenkat recoded responses.parquet")

##----Föräldra-relaterade frågor----
# 59. Hur skulle dina föräldrar reagera om du hade skolkat?

df$F58<-recode(df$F58,"'De skulle reagera mycket kraftigt'=0;
               'De skulle reagera ganska mycket'=1;
               'De skulle inte reagera så mycket'=2;
               'De skulle inte reagera alls'=3;
               'Vet inte'=NA;
               '<NA>'=NA",
               as.factor=FALSE)

# 72. Vem bestämmer när du ska vara hemma på kvällen?
# OBS inte självklar ordning på kodningen!!!
df$F71<-recode(df$F71,"'Vi diskuterar oss fram till det'=0;
               'Ofta föräldrarna som bestämmer'=2;
               'Föräldrarna bestämmer nästan helt'=3;
               'Det är oftast jag som bestämmer'=4;
               'Jag bestämmer själv om det'=1;
               '<NA>'=NA",
               as.factor=FALSE)

# 16. Hur får du tag (eller har fått tag) på cigarretter/e-cigaretter?
#finns inte i data??

###----Brott----

# brott, items f75a til s, question 77 in PDF
# Hur många gånger har du gjort följande saker under de senaste 12 månaderna?

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
                    'Ja, antal gånger'=1",
                    as.factor = FALSE)
}

###----Mobbning----

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

items.mobbning2 <- df %>% 
  select(F61,F63) %>% 
  names()

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
#write_csv(df, file = "data/2022-09-19 sthlmsenkat recoded responses.csv")
write_parquet(df, sink = "C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2022-09-18 sthlmsenkat recoded responses.parquet")

###---- ANDTS----

# 5. Bjudvanor - ANforalderindex=mean(F84,F40,F17,F21)
# ANTf – ANTindex - 1. mean(ANTf36,ANTf13,ANTf18,ANTf45)

## Egen användning
# 14. Röker du?
# 15. Röker du e-cigaretter?
# 19. Snusar du?
# 35. Hur ofta dricker du vid ett och samma tillfälle alkohol motsvarande minst:
# 37. Hur många gånger har du druckit så mycket alkohol att du känt dig berusad under den senaste 4-veckorsperioden?
# 42. Har du sniffat/boffat någon gång?
# 48. Hur många gånger totalt har du använt hasch/marijuana?
# 49. Hur många gånger totalt har du använt annan narkotika än hasch/marijuana?
# 50. Hur många gånger har du använt narkotika (cannabis eller annan narkotika) den senaste 4-veckors perioden?
# 53. Har du någon gång haft möjlighet att pröva narkotika?
# 54. Har du det senaste läsåret varit i kontakt med någon av följande hjälpinstanser på grund av att du använt alkohol eller droger?
# 74. Hur mycket pengar har du spelat för de senaste 30 dagarna?

## Tidig debut:
# 21. Hur gammal var du första gången du snusade?
# 38. Hur gammal var du första gången du kände dig berusad?
# 45. Hur gammal var du första gången du sniffade/boffade?
# 52. Hur gammal var du första gången du använde narkotika?

## Föräldrar
# 18. Får du röka för dina föräldrar?
# 22. Får du snusa för dina föräldrar?
# 23. Använder någon i din familj tobak (röker eller snusar)?
# 41. Får du dricka alkohol för dina föräldrar?
# 76. Får du satsa pengar på spel för dina föräldrar?










#----- OLD VERSION Ordinal to interval transformation-----

# import item information
itemlabels <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/allitems.csv")
itemlabels.final <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/FINALallitems.csv")

df.all <- read_parquet("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2022-08-23 sthlmsenkat recoded responses.parquet")

df<-df.all

# code to create a vector to select only items from a specific domain/index
items.if.all <- itemlabels %>% 
  filter(Index == "Individfaktorer") %>% 
  pull(itemnr)
items.skola.all <- itemlabels %>% 
  filter(Index == "Skola") %>% 
  pull(itemnr)
items.psf.all <- itemlabels %>% 
  filter(Index == "Psykiska/psykosomatiska besvär") %>% 
  pull(itemnr)
items.parents.all <- itemlabels %>% 
  filter(Index == "Föräldraskap") %>% 
  pull(itemnr)
items.nsam.all <- itemlabels %>% 
  filter(Index == "Närsamhälle") %>% 
  pull(itemnr)

# code to create a vector to select only final items from each domain/index
items.if.final <- itemlabels.final %>% 
  filter(Index == "Individfaktorer") %>% 
  pull(itemnr)
items.skola.final <- itemlabels.final %>% 
  filter(Index == "Skola") %>% 
  pull(itemnr)
items.psf.final <- itemlabels.final %>% 
  filter(Index == "Psykiska/psykosomatiska besvär") %>% 
  pull(itemnr)
items.parents.final <- itemlabels.final %>% 
  filter(Index == "Föräldraskap") %>% 
  pull(itemnr)
items.nsam.final <- itemlabels.final %>% 
  filter(Index == "Närsamhälle") %>% 
  pull(itemnr)

## sample code to subset an index
# df %>% 
#   select(any_of(items.if.final)) %>% 
#   RIlistitems()

# create variable for ID, that can be used to add columns with Rasch converted index scores
df$individ <- seq.int(nrow(df))

# Minimum items responses that a participant should have to be included in the analysis?
min.responses <- 5


#----- 02 Individfaktorer score----

# Select the variables we will work with, and filter out respondents with a lot of missing data
df.if <- df %>% 
  select(any_of(items.if.final),individ) %>%  # variables that start with "WAAQ"
  filter(length(items.if.final)-rowSums(is.na(.[items.if.final])) >= min.responses) %>%  # include only respondents with data for at least min.responses items
  na.omit() # for now, we only include those with complete data for each index

df.if.id<-df.if$individ # put the id variable aside, to rejoin it later
df.if$individ <- NULL

df.erm <- PCM(df.if) # run Rasch PCM model
ple <- person.parameter(df.erm) # estimate person parameters
thetas<-as.data.frame(coef(ple, extrapolated = T)) # extract interval scores to new df
thetas$individ <- df.if.id # insert id variable to new df
names(thetas) <- c("IFscore","individ") 
thetas$IFscore <- round(thetas$IFscore,2) 
thetas$IFscore100<-round(scales::rescale(thetas$IFscore, to = c(0,100)),1) # rescale logits to 0-100 range with one decimal
df <- merge(df,thetas,by = "individ", all = T) # merge interval score variables back to the original df
# clean up
thetas <- NULL
df.erm <- NULL
df.if <- NULL

#----- 03 Skola score----

# Select the variables we will work with, and filter out respondents with a lot of missing data
df.if <- df %>% 
  select(any_of(items.skola.final),individ) %>%  # variables that start with "WAAQ"
  filter(length(items.skola.final)-rowSums(is.na(.[items.skola.final])) >= min.responses) %>%  # include only respondents with data for at least min.responses items
  na.omit() # for now, we only include those with complete data for each index

df.if.id<-df.if$individ # put the id variable aside, to rejoin it later
df.if$individ <- NULL

df.erm <- PCM(df.if) # run Rasch PCM model
ple <- person.parameter(df.erm) # estimate person parameters
thetas<-as.data.frame(coef(ple, extrapolated = T)) # extract interval scores to new df
thetas$individ <- df.if.id # insert id variable to new df
names(thetas) <- c("IFscore","individ") 
thetas$IFscore <- round(thetas$IFscore,2) 
thetas$IFscore100<-round(scales::rescale(thetas$IFscore, to = c(0,100)),1) # rescale logits to 0-100 range with one decimal
names(thetas) <- c("SKOLAscore","individ","SKOLAscore100")
df <- merge(df,thetas,by = "individ", all = T) # merge interval score variables back to the original df
# clean up
thetas <- NULL
df.erm <- NULL
df.if <- NULL

#----- 04 PSF score----

# Select the variables we will work with, and filter out respondents with a lot of missing data
df.if <- df %>% 
  select(any_of(items.psf.final),individ) %>%  # variables that start with "WAAQ"
  filter(length(items.psf.final)-rowSums(is.na(.[items.psf.final])) >= min.responses) %>%  # include only respondents with data for at least min.responses items
  na.omit() # for now, we only include those with complete data for each index

df.if.id<-df.if$individ # put the id variable aside, to rejoin it later
df.if$individ <- NULL

df.erm <- PCM(df.if) # run Rasch PCM model
ple <- person.parameter(df.erm) # estimate person parameters
thetas<-as.data.frame(coef(ple, extrapolated = T)) # extract interval scores to new df
thetas$individ <- df.if.id # insert id variable to new df
names(thetas) <- c("IFscore","individ") 
thetas$IFscore <- round(thetas$IFscore,2) 
thetas$IFscore100<-round(scales::rescale(thetas$IFscore, to = c(0,100)),1) # rescale logits to 0-100 range with one decimal
names(thetas) <- c("PSFscore","individ","PSFscore100")
df <- merge(df,thetas,by = "individ", all = T) # merge interval score variables back to the original df
# clean up
thetas <- NULL
df.erm <- NULL
df.if <- NULL


#----- 05 Parents score----

# Select the variables we will work with, and filter out respondents with a lot of missing data
df.if <- df %>% 
  select(any_of(items.parents.final),individ) %>%  # variables that start with "WAAQ"
  filter(length(items.parents.final)-rowSums(is.na(.[items.parents.final])) >= min.responses) %>%  # include only respondents with data for at least min.responses items
  na.omit() # for now, we only include those with complete data for each index

df.if.id<-df.if$individ # put the id variable aside, to rejoin it later
df.if$individ <- NULL

df.erm <- PCM(df.if) # run Rasch PCM model
ple <- person.parameter(df.erm) # estimate person parameters
thetas<-as.data.frame(coef(ple, extrapolated = T)) # extract interval scores to new df
thetas$individ <- df.if.id # insert id variable to new df
names(thetas) <- c("IFscore","individ") 
thetas$IFscore <- round(thetas$IFscore,2) 
thetas$IFscore100<-round(scales::rescale(thetas$IFscore, to = c(0,100)),1) # rescale logits to 0-100 range with one decimal
names(thetas) <- c("PARENTSscore","individ","PARENTSscore100")
df <- merge(df,thetas,by = "individ", all = T) # merge interval score variables back to the original df
# clean up
thetas <- NULL
df.erm <- NULL
df.if <- NULL

#----- 07 Närsamhälle score----

# Select the variables we will work with, and filter out respondents with a lot of missing data
df.if <- df %>% 
  select(any_of(items.nsam.final),individ) %>%  # variables that start with "WAAQ"
  filter(length(items.nsam.final)-rowSums(is.na(.[items.nsam.final])) >= min.responses) %>%  # include only respondents with data for at least min.responses items
  na.omit() # for now, we only include those with complete data for each index

df.if.id<-df.if$individ # put the id variable aside, to rejoin it later
df.if$individ <- NULL

df.erm <- PCM(df.if) # run Rasch PCM model
ple <- person.parameter(df.erm) # estimate person parameters
thetas<-as.data.frame(coef(ple, extrapolated = T)) # extract interval scores to new df
thetas$individ <- df.if.id # insert id variable to new df
names(thetas) <- c("IFscore","individ") 
thetas$IFscore <- round(thetas$IFscore,2) 
thetas$IFscore100<-round(scales::rescale(thetas$IFscore, to = c(0,100)),1) # rescale logits to 0-100 range with one decimal
names(thetas) <- c("NSAMscore","individ","NSAMscore100")
df <- merge(df,thetas,by = "individ", all = T) # merge interval score variables back to the original df
# clean up
thetas <- NULL
df.erm <- NULL
df.if <- NULL

#----- Write datafile----
write_parquet(df, sink = "2022-08-26 sthlmsenkat scored2.parquet") # 8.3mb

df.scored<-read_parquet("2022-08-26 sthlmsenkat scored2.parquet")
df.scored %>% 
  select(ends_with("score100")) %>% 
  write.csv(.,file = "sthlmsenkatScored.csv")


#---- NEW ordinal to interval transformation----

# moved to file "2022-09-22 item-params o thetas.R"


# LRT dif
LRtest(df.erm, se = TRUE)

#---- data Vallentuna flyttat till annan fil----
library(foreign)
df.vtuna1618 <- as.data.frame(read.spss("C:/Users/magnuspjo/RISE/SHIC - Data i Dialog - Data i Dialog/data/Vallentuna/Sthlmsenk/Stockholmsenkäten 2018 Vallentuna 2016-2018.sav"))
df.vtuna20 <- as.data.frame(read.spss("C:/Users/magnuspjo/RISE/SHIC - Data i Dialog - Data i Dialog/data/Vallentuna/Sthlmsenk/Stockholmsenkäten 2020 Vallentuna.sav"))

df.vtuna18 <- as.data.frame(read.spss("C:/Users/magnuspjo/RISE/SHIC - Data i Dialog - Data i Dialog/data/Vallentuna/Sthlmsenk/Stockholmsenkäten 2018 Vallentuna.sav"))



itemlabels.final <- read.csv("C:/Users/magnuspjo/OneDrive - RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/FINALallitems.csv")
demografi <- c("ar","Kön","Arskurs","SkolID","SkolSDO","ARSKURS","Skolnamn")

Skolnamn20 <- df.vtuna20 %>% distinct(Skolnamn)
SkolID1618 <- df.vtuna1618 %>% distinct(SkolID)

df.vtuna1618 <- df.vtuna1618 %>% 
  rename(ARSKURS = Arskurs)

df.vtuna20 <- df.vtuna20 %>% 
  rename(Kön = F2)

df.vtuna20 %>% count(F2)
df.vtuna1618 %>% count(Kön)

#replace all 99* codes with missing (NA)
for (i in 1:ncol(df)) {
  df[,i]<-car::recode(df[,i],"990:999=NA;'<NA>'=NA")
}

names.1618<-df.vtuna1618 %>% 
  select(any_of(c(demografi,itemlabels.final$itemnr))) %>% 
  names()

names.20<-df.vtuna20 %>% 
  select(any_of(c(demografi,itemlabels.final$itemnr))) %>% 
  names()



################

# nedan är ej klart

#################
# Brott, begått: f75a-s, 278:287

# brott, utsatt för: f80a-e, names(df[244:263])
#[1] "f78aa" "F78ab" "f78ac" "f78ad" "f78ba" "F78bb" "f78bc" "f78bd" "f78ca" "F78cb" "f78cc" "f78cd" "f78da"
#[14] "F78db" "f78dc" "f78dd" "f78ea" "F78eb" "f78ec" "f78ed"
# svarsalternativ
#f78aa = 'Nej', 'Ja, antal gånger'
#f78ab = siffra för antal gånger (0 recodas till NA)
#f78ac = 'Nej', 'Ja, antal gånger'
#f78ad = siffra för antal gånger (0 recodas till NA)

#pdf("Plots/brott-histogram.pdf")
#for (i in c(244:263)) {
#  barplot(table(df[i]), col="lightblue",xlab=names(df[i]),ylab="Antal svar")
#}
#dev.off()

# utsatt för brott ja/nej, c(244,248,252,256,260)
for (i in c(244,248,252,256,260)) {
  df[,i]<-recode(df[,i],"'Nej'=0;'Ja, antal gånger'=1",as.factor=FALSE)
}

# antal gånger-frågorna, *b och *d, 0 blir NA. c(245,247,249,251,253,255,257,259,261,263)
for (i in c(245,247,249,251,253,255,257,259,261,263)) {
  df[,i]<-recode(df[,i],"0=NA",as.factor=FALSE)
}

# anmält till polis, c(246,250,254,258,262)
for (i in c(246,250,254,258,262)) {
  df[,i]<-recode(df[,i],"'Nej'=0;'Ja, antal gånger'=1",as.factor=FALSE)
}




###############

# framtidsfrågor, 68-69, heter f67 och f68 i data, variabler 212:213

df$F67<-recode(df$F67,"'Är mycket viktigt'=0;'Är ganska viktigt'=1;'Är varken viktigt eller oviktigt'=2;'Spelar nästan ingen roll'=3;'Spelar ingen roll alls'=4",as.factor=FALSE)
df$F68<-recode(df$F68,"'Mycket bättre'=0;'Lite bättre'=1;'Lika bra'=2;'Lite sämre'=3;'Mycket sämre'=4",as.factor=FALSE)



### DIF code draft

removed.items <- c("F90","F96","F94","F89","F91","F98")
df.dif <- df %>% 
  select(itemlabels$itemnr,Kön) %>% 
  select(!any_of(removed.items)) %>% 
  na.omit()

df.dif.gender <- df.dif$Kön
df.dif$Kön <- NULL

rcat1 <- c("F89","F91")
rcat2 <- c("F94","F95","F97")
rcat3 <- c("F92","F93","F98","F99")

for (i in rcat1) {
  df.dif[[i]]<-recode(df.dif[[i]],"2=1;3=2;4=2",as.factor=FALSE)
}
for (i in rcat2) {
  df.dif[[i]]<-recode(df.dif[[i]],"1=0;2=1;3=2;4=3",as.factor=FALSE)
}
for (i in rcat3) {
  df.dif[[i]]<-recode(df.dif[[i]],"4=3",as.factor=FALSE)
}
