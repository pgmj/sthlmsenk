### Not used, but archived here in case it is needed

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