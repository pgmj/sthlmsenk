library(furrr)
plan(multisession, workers = 8)
library(tictoc)
tic()
itemfit.skolpos <- df %>% 
  select(any_of(items.skolpos.final),ar) %>% 
  filter(ar %in% c(2008,2010,2012,2014)) %>% 
  na.omit() %>% 
  split(.$ar) %>% 
  future_map(~ RIitemfitPCM(subset(., select = -ar), table = FALSE), 
             .options = furrr_options(seed = TRUE))
toc()

itemfit.skolpos[["2010"]] 


tic()
itemfit.skolpos2 <- df %>% 
  select(any_of(items.skolpos.final),ar) %>% 
  na.omit() %>% 
  filter(ar %in% c(2008,2010)) %>% 
  group_by(ar) %>% 
  group_map(~ RIitemfitPCM2(.,400,8, table = FALSE))
toc()



df %>% 
  filter(ar %in% c(2008,2014)) %>% 
  select(any_of(items.skolpos.final)) %>% 
  na.omit() %>% 
  RIitemfitPCM(samplesize = 300, nsamples = 2, table = TRUE)

