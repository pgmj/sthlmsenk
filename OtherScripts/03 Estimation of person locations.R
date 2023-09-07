library(catR) # for thetaEst()
library(arrow)
library(tidyverse)
library(furrr)
library(RISEkbmRasch)

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
    select(all_of(allItems %>% # select index items
      filter(Index == i) %>%
      select(itemnr) %>%
      pull()), individ) %>% # and id variable
    filter(length(allItems %>% # include only respondents with data for at least min.responses items
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
  # estimate person scores using RIestThetas2, which uses parallel processing
  thetas <- as.data.frame(RIestThetas2(df.if, itemParams = itemParams[[x]], cpu = 8))
  thetas$individ <- df.if.id # insert id variable to new df
  names(thetas) <- c(i, "individ")
  thetas[1] <- round(thetas[1], 3)
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

write_parquet(df, sink = glue("../DIDapp/data/{Sys.Date()}_ScoredRev.parquet"))
