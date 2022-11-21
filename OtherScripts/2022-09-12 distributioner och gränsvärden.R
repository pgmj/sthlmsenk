
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

### set up color palette based on RISE guidelines
RISEprimGreen <- "#009ca6"
RISEprimRed <- "#e83c63"
RISEprimYellow <- "#ffe500"
RISEprimGreenMid <- "#8dc8c7"
RISEprimRedMid <- "#f5a9ab"
RISEprimYellowMid <- "#ffee8d"
RISEprimGreenLight <- "#ebf5f0"
RISEprimRedLight <- "#fde8df"
RISEprimYellowLight <- "#fff7dd"
RISEcompPurple <- "#482d55"
RISEcompGreenDark <- "#0e4e65"
RISEgrey1 <- "#f0f0f0"
RISEgrey2 <- "#c8c8c8"
RISEgrey3 <- "#828282"
RISEgrey4 <- "#555555"

#---- code to wrangle df commented out----

# df.sthlm <- read_parquet("/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2022-09-25_SthlmStadSCORED.parquet")
# df.pilot <- read_parquet("/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2022-09-25VtunaVholmSCORED.parquet")

itemlabels.final <- read.csv("/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/FINALallitemsSCORING.csv")
allitems <- read.csv("/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/allitems.csv")

# create vector with all index names
sthlm.index <- itemlabels.final %>% 
  distinct(Index) %>% 
  pull()

demogr.vars<-read.csv("/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/demographicVariables.csv")
demogr.vars <- demogr.vars$demogr.vars

# df.sthlm <- df.sthlm %>%
#   add_column(Kommun = 'Stockholm Stad')
# 
# df.pilot <- df.pilot %>%
#   add_column(Kommun = NA)
# 
# df.pilot[1:1423,146] <- c("Vallentuna")
# df.pilot[1424:2746,146] <- c("Vaxholm")
# 
# df.p <- df.pilot %>%
#   select(all_of(c(demogr.vars,sthlm.index,"Kommun")))
# df.s <- df.sthlm %>%
#   select(all_of(c(demogr.vars,sthlm.index,"Kommun")))
# 
# df <- rbind(df.p,df.s)
# 
# write_parquet(df, sink = "/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-09-25_vizdata.parquet")

#---- import data----

df <- read_parquet("/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-09-25_vizdata.parquet")

sthlmenk <- itemlabels.final %>% 
  distinct(Index) %>% 
  as.data.frame()

df.vtuna22 <- read_parquet("/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/data/2022-10-08Vtuna2022recoded.parquet")
# add 2022 data for Vallentuna
df.vtuna22 <- df.vtuna22 |> 
  select(any_of(c(demogr.vars,sthlm.index))) |> 
  add_column(Kommun = 'Vallentuna')
# merge dataframes
df<-rbind(df,df.vtuna22)

# df %>% 
#   #filter(ar %in% c(2016,2018,2020)) %>% 
#   group_by(ar,Kommun) %>% 
#   summarise(Medel = mean(PsykSomBesv, na.rm = T),
#             StDev = sd(PsykSomBesv,  na.rm = T),
#             n = n(),
#             StErr = StDev/sqrt(n),
#             ci.lo = Medel-StDev,
#             ci.hi = Medel+StDev) %>% 
#   rename(År = ar) %>% 
#   ggplot(aes(x=År,y=Medel, group = Kommun, color = Kommun)) +
#   geom_line(linetype = 1) +
#   #geom_ribbon(aes(ymin = ci.lo, ymax = ci.hi), alpha = 0.1) +
#   scale_y_continuous(limits = c(-1,1)) +
#   scale_x_continuous(breaks = c(df %>% distinct(ar) %>% pull())) +
#   theme_bw() +
#   labs(title="Psykiska/psykosomatiska besvär")
# 
# PsykSomBesv<-df %>% 
#   group_by(ar,Kommun) %>% 
#   summarise(Medel = mean(PsykSomBesv, na.rm = T),
#             StDev = sd(PsykSomBesv,  na.rm = T),
#             n = n(),
#             StErr = StDev/sqrt(n),
#             sd.lo = Medel-StDev,
#             sd.hi = Medel+StDev) %>% 
#   rename(År = ar) %>% 
#   mutate(across(where(is.numeric), round, 3)) %>% 
#   add_column(Faktor = 'PsykSomBesv')
#   as.data.frame()


#---- get cutoff values based on percentiles----


# underlag för att ta fram möjliga gränsvärden
# https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
p <- c(0.70,0.90,0.95)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
rslimits.70 <- df %>% 
  filter(!ar == 2022) |> 
  select(ar,sthlm.index) %>% 
  group_by(ar) %>% 
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>% 
  select(ar,ends_with("70%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  t() %>%
  as.data.frame() %>% 
  pull()
rslimits.90 <- df %>% 
  filter(!ar == 2022) |> 
  select(ar,sthlm.index) %>% 
  group_by(ar) %>% 
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>% 
  select(ar,ends_with("90%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  t() %>%
  as.data.frame() %>% 
  pull()
rslimits.95 <- df %>%
  filter(!ar == 2022) |> 
  select(ar,sthlm.index) %>%
  group_by(ar) %>%
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>%
  select(ar,ends_with("95%")) %>%
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>%
  t() %>%
  as.data.frame() %>%
  pull()

rslimits <- na.omit(as.data.frame(cbind(rslimits.70,rslimits.90,rslimits.95)))
rslimits <- rslimits %>% 
  rownames_to_column(var = "Index")
rslimits$Index <- sthlm.index

# now we need to reverse the positive indices and their limit values
# rslimits[2,2:4] <- rslimits[2,2:4]*-1
# rslimits[7,2:4] <- rslimits[7,2:4]*-1

rslimits <- rslimits %>% 
  t() %>% 
  as.data.frame() %>% 
  janitor::row_to_names(1) %>% 
  mutate_if(is.character, as.numeric)

#rslimits

write_csv(rslimits, file = "/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-10-08_rslimitsNoRev.csv")

rslimits <- read.csv("/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-10-08_rslimitsNoRev.csv")

# another way to find specific percentiles
df %>%  # Initial data
  tidyr::crossing(pctile = c(0.85,0.9,0.95)) %>%  # Specify quantiles; crossing() is like expand.grid()
  dplyr::group_by(ar, pctile) %>%  # Indicate your grouping var, plus your quantile var
  dplyr::summarise(quantile_value = quantile(Normbrytande, unique(pctile), na.rm = T)) %>%  # unique() is needed
  dplyr::mutate(pctile = sprintf("%1.0f%%", pctile*100)) %>% # Optional prettification
  filter(pctile == '85%')
  

# reverse score SkolaPositiv, Wellbeing
df$SkolaPositiv <- df$SkolaPositiv*-1
df$Wellbeing <- df$Wellbeing*-1

# funktion för att ta fram nyckelstatistik för varje  index
# RSsmf <- function(df, i) {
#   df %>% 
#     group_by(ar,Kommun) %>% 
#     summarise(Medel = mean({{i}}, na.rm = T),
#               StDev = sd({{i}},  na.rm = T),
#               n = n(),
#               StErr = StDev/sqrt(n),
#               sd.lo = Medel-StDev,
#               sd.hi = Medel+StDev
#               ) %>% 
#     rename(År = ar) %>% 
#     mutate(across(where(is.numeric), round, 3)) %>% 
#     as.data.frame()
# }  

#---- get key metrics for each year and municipality----

RSsmf <- function(df, i, j) { # input df, index, and index number
  #j <- match(qc({{i}}),sthlm.index)
  df %>% 
    group_by(ar,Kommun) %>% 
    summarise(Medel = mean({{i}}, na.rm = T),
              StDev = sd({{i}},  na.rm = T),
              n = n(),
              StErr = StDev/sqrt(n),
              sd.lo = Medel-StDev,
              sd.hi = Medel+StDev,
              n.90 = length(which({{i}} > rslimits[2,j]))/n*100) %>% 
    rename(År = ar) %>% 
    mutate(across(where(is.numeric), round, 3)) %>% 
    as.data.frame()
}  

# for positive scored index
RSsmfpos <- function(df, i, j) { # input df, index, and index number
  #j <- match(qc({{i}}),sthlm.index)
  df %>% 
    group_by(ar,Kommun) %>% 
    summarise(Medel = mean({{i}}, na.rm = T),
              StDev = sd({{i}},  na.rm = T),
              n = n(),
              StErr = StDev/sqrt(n),
              sd.lo = Medel-StDev,
              sd.hi = Medel+StDev,
              n.90 = length(which({{i}} < rslimits[2,j]))/n*100) %>% 
    rename(År = ar) %>% 
    mutate(across(where(is.numeric), round, 3)) %>% 
    as.data.frame()
}  


sums.Normbrytande <- RSsmf(df, Normbrytande, 1) %>% 
  add_column(Faktor = 'Normbrytande')
sums.SkolaPositiv <- RSsmf(df, SkolaPositiv, 2) %>% 
  add_column(Faktor = 'SkolaPositiv')
sums.SkolaNegativ <- RSsmf(df, SkolaNegativ, 3) %>% 
  add_column(Faktor = 'SkolaNegativ')
sums.PsykSomBesv <- RSsmf(df, PsykSomBesv, 4) %>% 
  add_column(Faktor = 'PsykSomBesv')
sums.Parenting <- RSsmf(df, Parenting, 5) %>% 
  add_column(Faktor = 'Parenting')
sums.Community <- RSsmf(df, Community, 6) %>% 
  add_column(Faktor = 'Community')
sums.Wellbeing <- RSsmf(df, Wellbeing, 7) %>% 
  add_column(Faktor = 'Wellbeing')

sums.index <- rbind(sums.Normbrytande,
                    sums.SkolaPositiv,
                    sums.SkolaNegativ,
                    sums.PsykSomBesv,
                    sums.Parenting,
                    sums.Community,
                    sums.Wellbeing)

write_csv(sums.index, file = "/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-09-26_indexsums90noRev.csv")

sums.index<-read_csv("/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-09-26_indexsums90noRev.csv")

#---- get key values divided by gender----

RSsmfGender <- function(df, i, j) { # input df, index, and index number
  #j <- match(qc({{i}}),sthlm.index)
  df %>% 
    group_by(ar,Kommun,Kön) %>% 
    summarise(Medel = mean({{i}}, na.rm = T),
              StDev = sd({{i}},  na.rm = T),
              n = n(),
              StErr = StDev/sqrt(n),
              sd.lo = Medel-StDev,
              sd.hi = Medel+StDev,
              n.90 = length(which({{i}} > rslimits[2,j]))/n*100) %>% 
    rename(År = ar) %>% 
    mutate(across(where(is.numeric), round, 3)) %>% 
    as.data.frame()
}  

sums.Normbrytande <- RSsmfGender(df, Normbrytande, 1) %>% 
  add_column(Faktor = 'Normbrytande') %>% 
  filter(!Kön == '<NA>')
sums.SkolaPositiv <- RSsmfGender(df, SkolaPositiv, 2) %>% 
  add_column(Faktor = 'SkolaPositiv') %>% 
  filter(!Kön == '<NA>')
sums.SkolaNegativ <- RSsmfGender(df, SkolaNegativ, 3) %>% 
  add_column(Faktor = 'SkolaNegativ') %>% 
  filter(!Kön == '<NA>')
sums.PsykSomBesv <- RSsmfGender(df, PsykSomBesv, 4) %>% 
  add_column(Faktor = 'PsykSomBesv') %>% 
  filter(!Kön == '<NA>')
sums.Parenting <- RSsmfGender(df, Parenting, 5) %>% 
  add_column(Faktor = 'Parenting') %>% 
  filter(!Kön == '<NA>')
sums.Community <- RSsmfGender(df, Community, 6) %>% 
  add_column(Faktor = 'Community') %>% 
  filter(!Kön == '<NA>')
sums.Wellbeing <- RSsmfGender(df, Wellbeing, 7) %>% 
  add_column(Faktor = 'Wellbeing') %>% 
  filter(!Kön == '<NA>')

sums.indexG <- rbind(sums.Normbrytande,
                    sums.SkolaPositiv,
                    sums.SkolaNegativ,
                    sums.PsykSomBesv,
                    sums.Parenting,
                    sums.Community,
                    sums.Wellbeing)

write_csv(sums.indexG, file = "/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-09-26_indexsums90noRevGender.csv")

#---- merge sums.index files----

sums.index <- sums.index %>% 
  add_column(Kön = 'Flickor och pojkar')

sums.indexG <- sums.indexG %>% 
  relocate(Kön, .after = 'Faktor')

allsums.index<-rbind(sums.index,sums.indexG)

write_csv(allsums.index, file = "/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-10-03_indexsums90noRevGender.csv")

#---- get count of each category----
RSsmfGcount <- function(df, i, j) { # input df, index, and index number
  #j <- match(qc({{i}}),sthlm.index)
  df %>% 
    group_by(ar,Kommun,Kön) %>% 
    summarise(Medel = mean({{i}}, na.rm = T),
              StDev = sd({{i}},  na.rm = T),
              n = n(),
              StErr = StDev/sqrt(n),
              sd.lo = Medel-StDev,
              sd.hi = Medel+StDev,
              n.90 = length(which({{i}} > rslimits[2,j])),
              n.10 = length(which({{i}} < rslimits[2,j]))) %>% 
    rename(År = ar) %>% 
    mutate(across(where(is.numeric), round, 3)) %>% 
    as.data.frame()
}  

sums.Normbrytande <- RSsmfGcount(df, Normbrytande, 1) %>% 
  add_column(Faktor = 'Normbrytande') %>% 
  filter(!Kön == '<NA>')
sums.SkolaPositiv <- RSsmfGcount(df, SkolaPositiv, 2) %>% 
  add_column(Faktor = 'SkolaPositiv') %>% 
  filter(!Kön == '<NA>')
sums.SkolaNegativ <- RSsmfGcount(df, SkolaNegativ, 3) %>% 
  add_column(Faktor = 'SkolaNegativ') %>% 
  filter(!Kön == '<NA>')
sums.PsykSomBesv <- RSsmfGcount(df, PsykSomBesv, 4) %>% 
  add_column(Faktor = 'PsykSomBesv') %>% 
  filter(!Kön == '<NA>')
sums.Parenting <- RSsmfGcount(df, Parenting, 5) %>% 
  add_column(Faktor = 'Parenting') %>% 
  filter(!Kön == '<NA>')
sums.Community <- RSsmfGcount(df, Community, 6) %>% 
  add_column(Faktor = 'Community') %>% 
  filter(!Kön == '<NA>')
sums.Wellbeing <- RSsmfGcount(df, Wellbeing, 7) %>% 
  add_column(Faktor = 'Wellbeing') %>% 
  filter(!Kön == '<NA>')

sums.indexGc <- rbind(sums.Normbrytande,
                     sums.SkolaPositiv,
                     sums.SkolaNegativ,
                     sums.PsykSomBesv,
                     sums.Parenting,
                     sums.Community,
                     sums.Wellbeing)

write_csv(sums.indexG, file = "/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-10-03_indexsumsNoRevGender.csv")



# multi-panel of % over 90th
#library(severance)
ggplot(sums.index, aes(x=factor(År), y=n.90, group = Kommun, color = Kommun)) +
  geom_line() +
  geom_point(size = 2) +
  #scale_y_continuous(limits = c(-2,2)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_color_manual(values = c(RISEprimGreen,RISEprimRed,RISEprimYellow)) +
  scale_fill_manual(values = c(RISEprimGreen,RISEprimRed,RISEprimYellow)) +
  theme_bw()  + 
  facet_wrap(~ Faktor) +
  ggtitle("Risk- och skyddsfaktorer över tid") +
  xlab("") + ylab("")


# testing code


# how many are above 85% rslimit for Normbrytande, for each Kommun and year?
df %>%
  #filter(!Kommun == 'Stockholm Stad') %>% 
  group_by(ar,Kommun) %>% 
  summarise(Medel = mean(Normbrytande, na.rm = T),
            StDev = sd(Normbrytande,  na.rm = T),
            n = n(),
            StErr = StDev/sqrt(n),
            sd.lo = Medel-StDev,
            sd.hi = Medel+StDev,
            n.90 = length(which(Normbrytande > rslimits[2,1]))/n*100
  ) %>% 
  mutate(n.90 = sprintf("%1.1f%%", n.90)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  formattable(.,list(
    formattable::area(col = 9) ~ color_tile(RISEprimYellowLight, RISEprimYellow))
    ,table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato; width: 80%"')

df %>%
  #filter(!Kommun == 'Stockholm Stad') %>% 
  group_by(ar,Kommun) %>% 
  summarise(Medel = mean(Normbrytande, na.rm = T),
            StDev = sd(Normbrytande,  na.rm = T),
            n = n(),
            StErr = StDev/sqrt(n),
            sd.lo = Medel-StDev,
            sd.hi = Medel+StDev,
            n.90 = length(which(Normbrytande > rslimits[2,1]))/n*100
  ) %>% 
  mutate(n.90 = sprintf("%1.1f%%", n.90)) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  ggplot(aes(x=factor(ar), y=Medel, group = Kommun, color = Kommun)) +
  geom_line(size = 1.8) +
  geom_point(size = 3.2) +
  geom_ribbon(aes(ymin = sd.lo, ymax = sd.hi, 
                  group = Kommun, fill = Kommun), 
              alpha = 0.1, linetype = 0) +
  scale_y_continuous(limits = c(-2,2)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_color_manual(values = c(RISEprimGreen,RISEprimRed,RISEprimYellow)) +
  scale_fill_manual(values = c(RISEprimGreen,RISEprimRed,RISEprimYellow)) +
  theme_bw() +
  xlab("År") +
  ylab("Normbrytande")

       
### ovanstående behöver också ta fram % individer över/under ett gränsvärde
### lite osäker på när reversering ska ske av SkolPos och WB, eller om de ska presenteras separat, för enkelhetens skull (och enbart reverseras inför skapande av figurer)

sums.Normbrytande <- RSsmf(df, Normbrytande) %>% 
  add_column(Faktor = 'Normbrytande')
sums.SkolaPositiv <- RSsmf(df, SkolaPositiv) %>% 
  add_column(Faktor = 'SkolaPositiv')
sums.SkolaNegativ <- RSsmf(df, SkolaNegativ) %>% 
  add_column(Faktor = 'SkolaNegativ')
sums.PsykSomBesv <- RSsmf(df, PsykSomBesv) %>% 
  add_column(Faktor = 'PsykSomBesv')
sums.Parenting <- RSsmf(df, Parenting) %>% 
  add_column(Faktor = 'Parenting')
sums.Community <- RSsmf(df, Community) %>% 
  add_column(Faktor = 'Community')
sums.Wellbeing <- RSsmf(df, Wellbeing) %>% 
  add_column(Faktor = 'Wellbeing')

sums.index <- rbind(sums.Normbrytande,
                    sums.SkolaPositiv,
                    sums.SkolaNegativ,
                    sums.PsykSomBesv,
                    sums.Parenting,
                    sums.Community,
                    sums.Wellbeing)

write_csv(sums.index, file = "/Users/magnuspjo/Library/CloudStorage/OneDrive-RISE/Dokument/Länsstyrelsen/Stockholmsenkäten2022/Rapporter/Visualisering/2022-09-26_indexsums.csv")


#- create groups based on 90th percentile----

df.grp <- df %>% 
  mutate(NBgrp = case_when(
    Normbrytande <= rslimits[2,1] ~ "0",
    Normbrytande >= rslimits[2,1] ~ "1"
  ),
  PSYgrp = case_when(
    PsykSomBesv <= rslimits[2,4] ~ "0",
    PsykSomBesv >= rslimits[2,4] ~ "1"
  ),
  SKOLpGrp = case_when(
    SkolaPositiv <= rslimits[2,2] ~ "0",
    SkolaPositiv >= rslimits[2,2] ~ "1"
  ),
  SKOLnGrp = case_when(
    SkolaNegativ <= rslimits[2,3] ~ "0",
    SkolaNegativ >= rslimits[2,3] ~ "1"
  ),
  FLDRgrp = case_when(
    Parenting <= rslimits[2,5] ~ "0",
    Parenting >= rslimits[2,5] ~ "1"
  ),
  NSAMgrp = case_when(
    SkolaNegativ <= rslimits[2,6] ~ "0",
    SkolaNegativ >= rslimits[2,6] ~ "1"
  ),
  NSAMgrp = case_when(
    SkolaNegativ <= rslimits[2,6] ~ "0",
    SkolaNegativ >= rslimits[2,6] ~ "1"
  ))

groups <- df.grp %>% 
  select(names(df.grp) %>% tail())

write_csv(groups, file = "2022-10-04_riskgroups.csv")


# Relative Risk calculations ----------------------------------------------

library(epitools)
psyk <- c()
SErrCalc <- function(predictor, outcome, output) {
  rr11 <- riskratio(predictor, outcome)
  rr22 <- as.data.frame(rr11$measure)
  output <- cbind(output,rr22[2,1])
}
SErrCalc(groups$PSYgrp, groups$FLDRgrp, psyk)

rr1 <- riskratio(groups$PSYgrp, groups$NBgrp)
rr2 <- as.data.frame(rr1$measure)
psyk <- rr2[2,1]

rr1 <- riskratio(groups$PSYgrp, groups$SKOLnGrp)
rr2 <- as.data.frame(rr1$measure)
psyk <- c(psyk,rr2[2,1])

rr1 <- riskratio(groups$PSYgrp, groups$FLDRgrp)
rr2 <- as.data.frame(rr1$measure)
psyk <- c(psyk,rr2[2,1])

rr1 <- riskratio(groups$PSYgrp, groups$NSAMgrp)
rr2 <- as.data.frame(rr1$measure)
psyk <- c(psyk,rr2[2,1])

as.data.frame(psyk)

groupsF <- df.grp %>% 
  filter(Kön == 'Flicka') %>% 
  select(names(df.grp) %>% tail())
groupsM <- df.grp %>% 
  filter(Kön == 'Pojke') %>% 
  select(names(df.grp) %>% tail())

# females only
psykF <- c()
rr1 <- riskratio(groupsF$PSYgrp, groupsF$NBgrp)
rr2 <- as.data.frame(rr1$measure)
psykF <- rr2[2,1]

rr1 <- riskratio(groupsF$PSYgrp, groupsF$SKOLnGrp)
rr2 <- as.data.frame(rr1$measure)
psykF <- c(psykF,rr2[2,1])

rr1 <- riskratio(groupsF$PSYgrp, groupsF$FLDRgrp)
rr2 <- as.data.frame(rr1$measure)
psykF <- c(psykF,rr2[2,1])

rr1 <- riskratio(groupsF$PSYgrp, groupsF$NSAMgrp)
rr2 <- as.data.frame(rr1$measure)
psykF <- c(psykF,rr2[2,1])

# males only
psykM <- c()
rr1 <- riskratio(groupsM$PSYgrp, groupsM$NBgrp)
rr2 <- as.data.frame(rr1$measure)
psykM <- rr2[2,1]

rr1 <- riskratio(groupsM$PSYgrp, groupsM$SKOLnGrp)
rr2 <- as.data.frame(rr1$measure)
psykM <- c(psykM,rr2[2,1])

rr1 <- riskratio(groupsM$PSYgrp, groupsM$FLDRgrp)
rr2 <- as.data.frame(rr1$measure)
psykM <- c(psykM,rr2[2,1])

rr1 <- riskratio(groupsM$PSYgrp, groupsM$NSAMgrp)
rr2 <- as.data.frame(rr1$measure)
psykM <- c(psykM,rr2[2,1])

psykALL <- as.data.frame(cbind(psyk,psykF,psykM))

rownames(psykALL) <- c('Normbrytande','Vantrivsel i skolan','Föräldraskap','Närsamhälle')
write_csv(psykALL, file = "2022-10-04_RRpsyk.csv")
psykALL %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  formattable(., table.attr = 
                'class=\"table table-striped\" style="font-size: 15px; 
                  font-family: Lato; width: 80%"')

# RR for other variables----

#TBD if desired

# multi-panel linegraph that shows each index over time, for the three groups
ggplot(sums.index, aes(x=factor(År), y=Medel, group = Kommun, color = Kommun)) +
  geom_line() +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = sd.lo, ymax = sd.hi, 
                  group = Kommun, fill = Kommun), 
              alpha = 0.15, linetype = 0) +
  scale_y_continuous(limits = c(-2,2)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_color_manual(values = c(RISEprimGreen,RISEprimRed,RISEprimYellow)) +
  scale_fill_manual(values = c(RISEprimGreen,RISEprimRed,RISEprimYellow)) +
  theme_bw()  + 
  facet_wrap(~ Faktor) +
  ggtitle("Risk- och skyddsfaktorer över tid")


  
linegraphs <- function(df, index) {
  df %>% 
    #filter(ar %in% c(2016,2018,2020)) %>% 
    group_by(ar,Kommun) %>% 
    summarise(Medel = mean({{index}}, na.rm = T),
              StDev = sd({{index}},  na.rm = T),
              n = n(),
              StErr = StDev/sqrt(n),
              sd.lo = Medel-StDev,
              sd.hi = Medel+StDev) %>% 
    ggplot(aes(x=ar,y=Medel, group = Kommun, color = Kommun)) +
    geom_line(linetype = 1) +
    geom_ribbon(aes(y = Medel, ymin = sd.lo, ymax = sd.hi, group = Kommun, fill = Kommun), alpha = 0.1) +
    scale_y_continuous(limits = c(-2,2)) +
    theme_bw()
}

linegraphs(df, Wellbeing)

sums.text <- sums.index %>% filter(Faktor == 'Normbrytande')
ggplot(data = sums.text, aes(x = factor(År), y = Medel, colour = Kommun, group = Kommun)) +
  geom_point(size = 3) + xlab('Årtal') + ylab('Indexvärde') + geom_line() +
  scale_color_manual("",labels = c(expression(paste("Priogrupp: n", phantom() %~~% phantom(), "19")),expression(paste("Övriga i interventionsgruppen: n", phantom() %~~% phantom(), "101"))), values =  c(rgb(255,115,69, max = 255), rgb(82,146,235, max = 255), RISEprimGreen)) +
  geom_ribbon(data = sums.text, aes(ymin= sd.lo, ymax = sd.hi, fill = Kommun), alpha = 0.2, linetype = 0) +
  scale_fill_manual("",labels = c(expression(paste("Priogrupp: n", phantom() %~~% phantom(), "19")),expression(paste("Övriga i interventionsgruppen: n", phantom() %~~% phantom(), "101"))), values =  c(rgb(255,115,69, max = 255), rgb(82,146,235, max = 255), RISEprimGreen)) +
  theme_set(theme_minimal() + theme(legend.position = 'bottom')) + ggtitle("Reaktionstid - priogrupp 1 (Urval PedsQL)") #


### not working:
# sums.index <- list()
# for (j in sthlm.index){
#   sums.index[[length(sums.index) + 1]] <- list(RSsmf(df, j))
# }



###### kan vi använda facet_wrap för olika årtal/områden för att skapa parallella figurer samtidigt?





#---- ta fram gränsvärden 85%----

library(lme4)
library(ggeffects)
library(ggplot2)
library(tidyverse)
library(shiny)
library(arrow)

library(ggdist) # for shadeable density slabs
library(gghalves) # for half-half geoms
library(ggpp) # for position_dodge2nudge %>% 
library(colorspace) # for lightening color palettes
library(extrafont) # for Lato, RISE font
library(stringr)


# https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
p <- c(0.2, 0.5, 0.85)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)
df %>% 
  select(ar,sthlm.index) %>% 
  group_by(ar) %>% 
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>% 
  select(ar,ends_with("85%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  formattable(., list(
    formattable::area(row = 9) ~ color_tile(RISEprimYellowLight, RISEprimYellow)),
    table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato; width: 80%"')



df.shiny %>%
  filter(year == 2020) %>% # filter the year selected
  filter(gender == 'Pojke') %>% # 
  summarise(factor.mean = mean(Normbrytande)) %>% 
  summary(factor.mean)

df.shiny %>% 
  group_by(year, SkolSDO) %>% 
  summarise()


#p_funs
library(formattable)
df.shiny %>% 
  select(year,rsfaktorer) %>% 
  group_by(year) %>% 
  summarize_at(vars(rsfaktorer), funs(!!!p_funs)) %>% 
  select(year,ends_with("85%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  formattable(., list(
    formattable::area(row = 9) ~ color_tile(RISEprimYellowLight, RISEprimYellow)),
    table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato; width: 80%"')


rslimits<-df.shiny %>% 
  select(year,rsfaktorer) %>% 
  group_by(year) %>% 
  summarize_at(vars(rsfaktorer), funs(!!!p_funs)) %>% 
  select(year,ends_with("85%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>%
  as.data.frame()

rslimits <- rbind(rslimits[9,2:ncol(rslimits)])
names(rslimits) <- rsfaktorer

varmean <- mean(df.shiny$Normbrytande, na.rm = T)

df.shiny <- df

df.shiny<-df.shiny %>% 
  rename(year = ar)

#---- plot yearly distributions per factor----
ggplot(df.shiny, aes(Normbrytande, as.factor(year), fill = as.factor(year))) + # make plot, with area color
  stat_slab(side = "right", show.legend = F,
            scale = 0.6, # defines the height that a slab can reach
            position = position_dodge(width=.6), # distance between elements for dodging
            aes(fill_ramp = stat(level), fill=as.factor(year)), 
            .width = c(.50,.90,1)) +  # set shading
  stat_summary(fun.data = "mean_cl_normal",show.legend = F, size = .4,
               position = position_dodge2nudge(x=.05,width = .8)) +
  scale_fill_ramp_discrete(from='black', aesthetics = "fill_ramp") +
  geom_vline(xintercept = rslimits[9,2], color = RISEprimGreen, linetype = 2, size = 0.8) +
  geom_vline(xintercept = varmean, color = RISEprimYellowLight, linetype = 2, size = 0.8) +
    #geom_text() +
  scale_x_continuous(limits = c(0,100)) +
  # styling
  theme(axis.text.x = element_text(size=16, family = "sans"), 
        axis.text.y = element_text(size=16, family = "sans"),
        title = element_text(size=18),
  )+
  xlab("Normbrytande")+
  ylab("Årtal")

rslimitsN <- c()
rslim1 <- c()
for (i in rsfaktorer) {
  df.shiny %>% 
    count(i > rslimits[[i]]) %>% 
    .[2,2] -> rslim1
  
  rslimitsN <- c(rslimitsN,rslim1)
}

nb.count <- df.shiny %>% 
  group_by(year) %>% 
  count(Normbrytande > rslimits$Normbrytande) %>% 
  rename(nb = `Normbrytande > rslimits$Normbrytande`) %>% 
  filter(nb == TRUE) %>% 
  select(year,n) %>% 
  as.data.frame()
#  add_column(andel = n/nrow(df.shiny %>% filter(year =)))
#  formattable(table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato; width: 80%"')
# ta fram TRUE för varje år, räkna mot totalt antal det året för att få andelar

nb.count$totalN <- df.shiny %>% 
  group_by(year) %>% 
  count() %>% 
  pull()

nb.count$andel <- round(nb.count$n/nb.count$totalN*100,2)

formattable(nb.count, list(
            formattable::area(col = 4) ~ color_tile(RISEprimYellowLight, RISEprimYellow)),
            table.attr = 'class=\"table table-striped\" style="font-size: 15px; font-family: Lato; width: 80%"')

  .[2,2] -> rslimitNORM2 


ggplot(df.plot, aes_string(input$factor, "SkolSDO", fill = "SkolSDO")) + # make plot, with area color
  stat_slab(side = "right", show.legend = F,
            scale = 0.6, # defines the height that a slab can reach
            position = position_dodge(width=.6), # distance between elements for dodging
            aes(fill_ramp = stat(level), fill=SkolSDO), 
            .width = c(.50,.90,1)) +  # set shading
  stat_summary(fun.data = "mean_cl_normal",show.legend = F, size = .4,
               position = position_dodge2nudge(x=.05,width = .8)) +
  scale_fill_ramp_discrete(from='black', aesthetics = "fill_ramp") +
  geom_vline(xintercept = varmean, color = RISEprimGreen, linetype = 2, size = 0.8) +
  #geom_text() +
  scale_x_continuous(limits = c(0,100)) +
  # styling
  theme(axis.text.x = element_text(size=16, family = "sans"), 
        axis.text.y = element_text(size=16, family = "sans"),
        title = element_text(size=18),
  )+
  xlab("")+
  ylab("")


hist(df.shiny$Normbrytande, col = RISEprimGreen, main = "Normbrytande, samtliga år", xlab = "")




