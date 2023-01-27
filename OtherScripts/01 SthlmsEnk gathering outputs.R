### This is a script to collect outputs from all psychometric analyses, which includes two types of files
# 1. CSV file with itemnr and item description
# 2. CSV file with item parameters from Rasch analysis
# both are needed for the estimation of person locations for each subscale
library(arrow)
library(tidyverse)
library(xlsx)
# read all item data into dataframes
IFitemParams <- read_csv_arrow("02_IF/IFnegativaItems.csv")
IFitems <- read_csv_arrow("02_IF/IFnegItemnr.csv")

SkolaNegParams <- read_csv_arrow("03_Skola/SkolaNegativaItems.csv")
SkolaNegItems <- read_csv_arrow("03_Skola/SkolaNegItemnr.csv")
SkolaPosParams <- read_csv_arrow("03_Skola/SkolaPositivaItems.csv")
SkolaPosItems <- read_csv_arrow("03_Skola/SkolaPosItemnr.csv")

PSFitemParams <- read_csv_arrow("04_PSF/itemParameters.csv")
PSFitems <- read_csv_arrow("04_PSF/PSFitemnr.csv")

ParentingParams <- read_csv_arrow("05_Parenting/itemParameters.csv")
ParentingItems <- read_csv_arrow("05_Parenting/ParentingItemnr.csv")

CommunityParams <- read_csv_arrow("07_Community/itemParameters.csv")
CommunityItems <- read_csv_arrow("07_Community/CommunityItemnr.csv")

WellbeingParams <- read_csv_arrow("Wellbeing/itemParameters.csv")
WellbeingItems <- read_csv_arrow("Wellbeing/WellbeingItemnr.csv")

# collect all item parameters as matrix objects (needed for catR::thetaEst()) within a list object, so that we can loop/map it later
# itemParams[[1]] will be IFitemParams, etc
itemParams <- list()
itemParams$Utagerande <- as.matrix(IFitemParams)
itemParams$SkolaNegativ <- as.matrix(SkolaNegParams)
itemParams$SkolaPositiv <- as.matrix(SkolaPosParams)
itemParams$PsykSomBesv <- as.matrix(PSFitemParams)
itemParams$Parenting <- as.matrix(ParentingParams)
itemParams$Community <- as.matrix(CommunityParams)
itemParams$Wellbeing <- as.matrix(WellbeingParams)

# collect all itemnr and descriptions in a list object (as tibble)
# itemNumber[[1]]$itemnr will access a vector of IF items, that can be use for item selection
# we also add an index variable to each dataframe
itemNumber <- list()
itemNumber$Utagerande <- IFitems %>% 
  add_column(Index = "Utagerande")
itemNumber$SkolaNegativ <- SkolaNegItems %>% 
  add_column(Index = "SkolaNegativ")
itemNumber$SkolaPositiv <- SkolaPosItems %>% 
  add_column(Index = "SkolaPositiv")
itemNumber$PsykSomBesv <- PSFitems %>% 
  add_column(Index = "PsykSomBesv")
itemNumber$Parenting <- ParentingItems %>% 
  add_column(Index = "Parenting")
itemNumber$Community <- CommunityItems %>% 
  add_column(Index = "Community")
itemNumber$Wellbeing <- WellbeingItems %>% 
  add_column(Index = "Wellbeing")

# create a df with all items
allItems <- data.frame(matrix(ncol = 3, nrow = 0))
for (i in 1:length(itemNumber)){
  allItems <- rbind(allItems,itemNumber[[i]])
}

### when/if needed, remove duplicates (created by Wellbeing index) by using the distinct() command 
# allItems %>%
#   distinct(.keep_all = TRUE)

# create a df with all item parameters and itemnr
allItemParams <- data.frame(matrix(ncol = 5, nrow = 0))
allItemParams <- data.frame(itemParams[[1]])
#names(allItemParams) <- c("Threshold 1","Threshold 2","Threshold 3")
for (i in 2:length(itemParams)){
  allItemParams <- bind_rows(allItemParams,data.frame(itemParams[[i]]))
}

# join params and descriptions
allItemInfo <- cbind(allItems,allItemParams)
#write.xlsx(allItemInfo, "../data/2022-12-06 allItemInfo.xls", row.names = F)

# add itemnr identifyer to params df
allItemParams <- allItemParams %>% 
  add_column(itemnr = allItems$itemnr) %>% 
  relocate(itemnr, .before = "Threshold.1")

# get all iteminfo in the same frame, but without itemnr duplicates (wellbeing item duplicates removed)
allItemInfoNonDup <- left_join(allItems %>% 
                           distinct(itemnr, .keep_all = TRUE),
                         allItemParams %>% 
                           distinct(itemnr, .keep_all = TRUE),
                         by = "itemnr")


