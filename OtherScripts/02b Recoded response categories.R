
# Re-recoding based on psychometric evaluations  ----------------------------------
# fixa svarskategorier, item-eliminering och slutgiltiga item-uppsättningar


## 02 Individfaktorer-----

#df$f66p<-recode(df$f66p,"2=1;3=2", as.factor = F)

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


## 09 ANDTS ----------------------------------------------------------------
df$F14 <- recode(df$F14,"3=2;4=3")
df$FNY12020 <- recode(df$FNY12020,"3:4=2")
df$F18 <- recode(df$F18,"2=1;3:4=2")
df$F34 <- recode(df$F34,"2=1;3=2;4=3;5=4")

#df$F41 <- NULL
df$F47 <- recode(df$F47,"4=3;5:6=4")
df$F47 <- recode(df$F47,"4=3") # i praktiken 4:6=3 

df$F73 <- recode(df$F73,"2:5=1;6=2")
df$F48 <- recode(df$F48,"1=0;2:3=1;4:6=2")

df <- df %>%
  mutate(across(itemsANDTSdebut, ~ recode(.x, "0:8=NA;9:11=0;12=1;13=2;13.5=2;14=3;15=4;16=5;16.5=6;17=6;18:20=7")))


## 10 Brott ----------------------------------------------------------------

for (i in items.brott){
  df[[i]] <- recode(df[[i]],"3:4=2")
}
for (i in items.brott){
  df[[i]] <- recode(df[[i]],"2:4=1")
}

# Write new datafile ------------------------------------------------------

write_parquet(df, sink = glue("../data/{Sys.Date()}_recodedData2b.parquet"))
