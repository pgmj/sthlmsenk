####
library(arrow)
library(xlsx)
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

#----- read item/variable information-----
allitems <- read.csv("../data/allitems.csv")
itemlabels <- read.csv("../data/allitems.csv")
#itemlabels.final <- read.csv("../data/FINALallitems.csv")
itemlabels.final <- read.csv("../data/FINALallitemsSCORING.csv")


#---- read item data----
# recoded items (except for F61) based on analyses 02-08
df <- read_parquet("../../data/2022-09-24 sthlmsenkat recoded responses.parquet")


# create vector with all index names
sthlm.index <- itemlabels.final %>% 
  distinct(Index) %>% 
  pull()

#---- creating excel file with item parameters----
#---- skip if no changes have been made since last update----
# subset each index and retrive Rasch model item parameters
RISEitemps <- function(dfin, se.index) {
  df.omit.na <- dfin %>% 
    select(all_of(itemlabels.final %>% 
                    filter(Index == se.index) %>% 
                    select(itemnr) %>% 
                    pull())) %>% 
    na.omit()
  df.erm <- PCM(df.omit.na)
  item.estimates <- eRm::thresholds(df.erm)
  item_difficulty <- as.data.frame(item.estimates[["threshtable"]][["1"]])
  item_difficulty$Location <- NULL
  se.itemps <- item_difficulty %>% 
    mutate_if(is.character, as.numeric) %>% 
    as.matrix()
  return(se.itemps)
}

#----loop through all indices using the function----
#storing each in a separate list element
allitemps <- list()
for (i in sthlm.index){
  allitemps[[length(allitemps) + 1]] <- list(RISEitemps(df,i))
}

#---- write item parameters to a single excel file----
#setwd("../../data/")
for (i in 1:length(sthlm.index)) {
  write.xlsx(allitemps[[i]],
             "../../data/2022-09-24_itemparams.xls", 
             sheet = sthlm.index[i], 
             append = T, 
             showNA = F)
}

#####################################################


#---- read item params----
# (no need to re-calculate or access raw data)
allitemps <- list()
#setwd("../../data/")
for (i in 1:length(sthlm.index)) {
  read.xlsx("../data/2022-09-23_itemparams.xls", 
             sheetName = sthlm.index[i]) %>% 
    select(!NA.) %>% # remove item names
    as.matrix() -> allitemps[[length(allitemps) + 1]]
}

library(catR) # for thetaEst
# create vector with participant ID's for use to merge in thetas when calculated
df$individ <- seq.int(nrow(df))
individ.id <- seq.int(nrow(df))

# Minimum items responses that a participant should have to be included in the analysis?
min.responses <- 5

#----- 02 Individfaktorer score----
# Select the variables we will work with, and filter out respondents with a lot of missing data
df.if <- df %>% 
  head(100) %>% 
    select(all_of(itemlabels.final %>% 
                  filter(Index == 'PsykSomBesv') %>% 
                  select(itemnr) %>% 
                  pull()
                ),individ) %>%
  filter(length(itemlabels.final %>% 
                  filter(Index == 'PsykSomBesv') %>% 
                  select(itemnr) %>% 
                  pull()
                )-rowSums(is.na(.[itemlabels.final %>% 
                                    filter(Index == 'PsykSomBesv') %>% 
                                    select(itemnr) %>% 
                                    pull()])) >= min.responses) # include only respondents with data for at least min.responses items

# put the id variable aside, for later merge
df.if.id<-df.if$individ 
df.if$individ <- NULL

# create vector for theta scores
thetaEstScores <- c()
for (i in 1:nrow(df.if)){
  p1 <- as.numeric(as.vector((df.if[i,])))
  ptheta <- thetaEst(allitemps[[1]], p1, model = "PCM", method = "ML") # "WL" or "ML"
  thetaEstScores <- c(thetaEstScores,ptheta)
}

thetas<-as.data.frame(thetaEstScores) # extract interval scores to new df
thetas$individ <- df.if.id # insert id variable to new df
names(thetas) <- c("PsykSomBesv","individ") 
thetas[1] <- round(thetas[1],3) 
#thetas$IFscore100<-round(scales::rescale(thetas$IFscore, to = c(0,100)),1) # rescale logits to 0-100 range with one decimal
df <- merge(df,thetas,by = "individ", all = T) # merge interval score variables back to the original df

#---- can we loop the section above through all indices?----

for (i in sthlm.index) {
  df.if <- df %>% 
    #head(100) %>% # for testing purposes subset small group
    select(all_of(itemlabels.final %>% 
                    filter(Index == i) %>% 
                    select(itemnr) %>% 
                    pull()
    ),individ) %>%
    filter(length(itemlabels.final %>% 
                    filter(Index == i) %>% 
                    select(itemnr) %>% 
                    pull()
            )-rowSums(is.na(.[itemlabels.final %>% 
                        filter(Index == i) %>% 
                        select(itemnr) %>% 
                        pull()])) >= min.responses) # include only respondents with data for at least min.responses items
  
  # put the id variable aside, for later merge
  df.if.id<-df.if$individ 
  df.if$individ <- NULL
  
  # create vector for theta scores
  thetaEstScores <- c()
  x <- itemlabels.final %>% 
    distinct(Index) %>% 
    select(Index) %>% 
    rownames_to_column(var = "order") %>% 
    filter(Index == i) %>% 
    pull(order) %>% 
    as.numeric()
  # this loop should probably be rewritten using sapply instead
  # https://bioinformatics.stackexchange.com/questions/4580/how-do-i-create-a-for-loop-to-filter-through-different-fdr-values
  for (j in 1:nrow(df.if)){
    p1 <- as.numeric(as.vector((df.if[j,])))
    ptheta <- thetaEst(allitemps[[x]], p1, model = "PCM", method = "ML") # "WL" or "ML"
    thetaEstScores <- c(thetaEstScores,ptheta)
  }
  
  thetas<-as.data.frame(thetaEstScores) # insert interval scores to new df
  thetas$individ <- df.if.id # insert id variable to new df
  names(thetas) <- c(i,"individ") 
  thetas[1] <- round(thetas[1],3) 
  #thetas$IFscore100<-round(scales::rescale(thetas$IFscore, to = c(0,100)),1) # rescale logits to 0-100 range with one decimal
  df <- merge(df,thetas,by = "individ", all = T) # merge interval score variables back to the original df 
  # clean up between loops
  df.if <- NULL
  thetas <- NULL
  thetaEstScores <- c()
}


#---- cleaned code for estimating thetas----

df <- read_parquet("../../data/2022-09-24 sthlmsenkat recoded responses.parquet")

library(catR)
library(xlsx)

itemlabels.final <- read.csv("../../data/FINALallitemsSCORING.csv")
allitems <- read.csv("../../data/allitems.csv")


# create vector with all index names
sthlm.index <- itemlabels.final %>% 
  distinct(Index) %>% 
  pull()

#---- read item params----
# (no need to re-calculate or access raw data)
allitemps <- list()
#setwd("../../data/")
for (i in 1:length(sthlm.index)) {
  read.xlsx("../data/2022-09-24_itemparams.xls", 
            sheetName = sthlm.index[i]) %>% 
    select(!NA.) %>% # remove item names
    as.matrix() -> allitemps[[length(allitemps) + 1]]
}

# create vector with participant ID's for use to merge in thetas when calculated
df$individ <- seq.int(nrow(df))
individ.id <- seq.int(nrow(df))

# Minimum items responses that a participant should have to be included in the analysis?
min.responses <- 5

for (i in sthlm.index) {
  df.if <- df %>% 
    #head(100) %>% # for testing purposes subset small group
    select(all_of(itemlabels.final %>% 
                    filter(Index == i) %>% 
                    pull(itemnr)
    ),individ) %>%
    filter(length(itemlabels.final %>% 
                    filter(Index == i) %>% 
                    pull(itemnr)
    )-rowSums(is.na(.[itemlabels.final %>% 
                        filter(Index == i) %>% 
                        pull(itemnr)])) >= min.responses) # include only respondents with data for at least min.responses items
  
  # put the id variable aside, for later merge
  df.if.id<-df.if$individ 
  df.if$individ <- NULL
  
  # create vector for theta scores
  thetaEstScores <- c()
  x <- itemlabels.final %>% distinct(Index) %>% select(Index) %>% rownames_to_column(var = "order") %>% filter(Index == i) %>% pull(order) %>% as.numeric()
  # this loop should probably be rewritten using sapply instead
  # https://bioinformatics.stackexchange.com/questions/4580/how-do-i-create-a-for-loop-to-filter-through-different-fdr-values
  for (j in 1:nrow(df.if)){
    p1 <- as.numeric(as.vector((df.if[j,])))
    ptheta <- thetaEst(allitemps[[x]], p1, model = "PCM", method = "ML") # "WL" or "ML"
    thetaEstScores <- c(thetaEstScores,ptheta)
  }
  
  thetas<-as.data.frame(thetaEstScores) # insert interval scores to new df
  thetas$individ <- df.if.id # insert id variable to new df
  names(thetas) <- c(i,"individ") 
  thetas[1] <- round(thetas[1],3) 
  #thetas$IFscore100<-round(scales::rescale(thetas$IFscore, to = c(0,100)),1) # rescale logits to 0-100 range with one decimal
  df <- merge(df,thetas,by = "individ", all = T) # merge interval score variables back to the original df 
  # clean up between loops
  df.if <- NULL
  thetas <- NULL
  thetaEstScores <- c()
}

write_parquet(df, sink = "../../data/2022-09-25_SthlmStadSCORED.parquet")

