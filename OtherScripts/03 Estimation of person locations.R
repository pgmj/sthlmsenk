library(catR) # for thetaEst()
library(arrow)
library(tidyverse)
library(catR)
library(furrr)

### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename


# Read recoded data -------------------------------------------------------

#df <- read_parquet("../data/2022-12-05_recodedData.parquet")

# all analyzed items
allAnalyzedItems <- read.csv("../data/allitems.csv")

# create vector with participant ID's for use to merge in thetas when calculated
df$individ <- seq.int(nrow(df))
individ.id <- seq.int(nrow(df))

# Minimum items responses that a participant should have to be included in the analysis?
min.responses <- 5

### use objects created in script 01:
sthlm.index <- allItems %>% 
  distinct(Index) %>% 
  pull()


# Nested loop function for estimating thetas for all indices --------------------

# Select the variables we will work with, and filter out respondents with
# more missing data than allowed according to `min.responses`
for (i in sthlm.index) {
  df.if <- df %>%
    # head(100) %>% # for testing purposes subset small group
    select(all_of(allItems %>%
      filter(Index == i) %>%
      select(itemnr) %>%
      pull()), individ) %>%
    filter(length(allItems %>%
      filter(Index == i) %>%
      select(itemnr) %>%
      pull()) - rowSums(is.na(.[allItems %>%
      filter(Index == i) %>%
      select(itemnr) %>%
      pull()])) >= min.responses) # include only respondents with data for at least min.responses items

  # put the id variable aside, for later merge
  df.if.id <- df.if$individ
  df.if$individ <- NULL

  # create vector for theta scores
  thetaEstScores <- c()
  x <- allItems %>%
    distinct(Index) %>%
    select(Index) %>%
    rownames_to_column(var = "order") %>%
    filter(Index == i) %>%
    pull(order) %>%
    as.numeric()
  # this loop should probably be rewritten using sapply instead, or furrr::map2_dbl
  # https://bioinformatics.stackexchange.com/questions/4580/how-do-i-create-a-for-loop-to-filter-through-different-fdr-values
  # for (j in 1:nrow(df.if)) {
  #   p1 <- as.numeric(as.vector((df.if[j, ])))
  #   ptheta <- thetaEst(itemParams[[x]], p1, model = "PCM", method = "WL") # WL has less bias (see Warm, 1989)
  #   thetaEstScores <- c(thetaEstScores, ptheta)
  # }

  #thetas <- as.data.frame(thetaEstScores) # insert interval scores to new df
  thetas <- RIestThetas2(df.if, itemParams = x, cpu = 8)
  thetas$individ <- df.if.id # insert id variable to new df
  names(thetas) <- c(i, "individ")
  thetas[1] <- round(thetas[1], 3)
  # thetas$IFscore100<-round(scales::rescale(thetas$IFscore, to = c(0,100)),1) # rescale logits to 0-100 range with one decimal
  df <- merge(df, thetas, by = "individ", all = T) # merge interval score variables back to the original df
  # clean up between loops
  df.if <- NULL
  thetas <- NULL
  thetaEstScores <- c()
}

# Reverse score positive scales -------------------------------------------

df$SkolaPositiv <- df$SkolaPositiv*-1
df$Wellbeing <- df$Wellbeing*-1


# Save to file ------------------------------------------------------------

# code below for adding Vallentuna 2022 data, messily
# df.old <- read_parquet("../DIDapp/data/2023-01-10_ScoredRev.parquet")
# #df <- df %>% 
# #  select(any_of(c(demogr.vars,allAnalyzedItems$itemnr,"SkolID_gammal","SkolSDO")))
# 
# df.new <- df %>% 
#   select(!any_of(c("Skolenhetskod","Skolnamn","F3_Omkodad","SkolID_gammal")))
#   
# df.old$individ <- NULL
# df.new$individ <- NULL
# 
# setdiff(names(df.old),names(df.new))
# setdiff(names(df.new),names(df.old))
# 
# df.new <- df.new %>%
#   rename(`Hur länge har du bott i Sverige?` = F5,
#          `Vilken högsta utbildning har din mamma?` = f6a,
#          `Vilken högsta utbildning har din pappa?` = f6b,
#          `Vad bor du i för typ av bostad?` = F7)
# 
# df.new <- df.new %>% 
#   add_column(DIDkommun = "Vallentuna",
#              riskPSF = NA)
# 
# df.new <- df.new %>% 
#   relocate(DIDkommun, .after = "SkolSDO")
# 
# df.newest <- rbind(df.old,df.new)
# 
# write_parquet(df.newest, sink = glue("../DIDapp/data/{Sys.Date()}_ScoredRev.parquet"))

# Single index calculation ------------------------------------------------

for (i in "Wellbeing") {
  df.if <- df %>% 
    #head(100) %>% # for testing purposes subset small group
    select(all_of(allItems %>% 
                    filter(Index == i) %>% 
                    select(itemnr) %>% 
                    pull()
    ),individ) %>%
    filter(length(allItems %>% 
                    filter(Index == i) %>% 
                    select(itemnr) %>% 
                    pull()
    )-rowSums(is.na(.[allItems %>% 
                        filter(Index == i) %>% 
                        select(itemnr) %>% 
                        pull()])) >= min.responses) # include only respondents with data for at least min.responses items
  
  # put the id variable aside, for later merge
  df.if.id<-df.if$individ 
  df.if$individ <- NULL
  
  # create vector for theta scores
  thetaEstScores <- c()
  x <- allItems %>% 
    distinct(Index) %>% 
    select(Index) %>% 
    rownames_to_column(var = "order") %>% 
    filter(Index == i) %>% 
    pull(order) %>% 
    as.numeric()
  # this loop should probably be rewritten using sapply instead, or furrr::map2_dbl
  # https://bioinformatics.stackexchange.com/questions/4580/how-do-i-create-a-for-loop-to-filter-through-different-fdr-values
  for (j in 1:nrow(df.if)){
    p1 <- as.numeric(as.vector((df.if[j,])))
    ptheta <- thetaEst(itemParams[[x]], p1, model = "PCM", method = "WL") # "WL" or "ML"
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



# Dividing the function to test its components ----------------------------



df.if <- df %>% 
  #head(100) %>% # for testing purposes subset small group
  select(all_of(allItems %>% 
                  filter(Index == "Wellbeing") %>% 
                  select(itemnr) %>% 
                  pull()
  ),individ) %>%
  filter(length(allItems %>% 
                  filter(Index == "Wellbeing") %>% 
                  select(itemnr) %>% 
                  pull()
  )-rowSums(is.na(.[allItems %>% 
                      filter(Index == "Wellbeing") %>% 
                      select(itemnr) %>% 
                      pull()])) >= min.responses) # include only respondents with data for at least min.responses items

# put the id variable aside, for later merge
df.if.id<-df.if$individ 
df.if$individ <- NULL

# create vector for theta scores
thetaEstScores <- c()
x <- allItems %>% 
  distinct(Index) %>% 
  select(Index) %>% 
  rownames_to_column(var = "order") %>% 
  filter(Index == "Wellbeing") %>% 
  pull(order) %>% 
  as.numeric()

for (j in 1:nrow(df.if)){
  p1 <- as.numeric(as.vector((df.if[j,])))
  ptheta <- thetaEst(itemParams[[x]], p1, model = "PCM", method = "WL") # "WL" or "ML"
  thetaEstScores <- c(thetaEstScores,ptheta)
}