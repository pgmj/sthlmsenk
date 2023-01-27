library(arrow)
library(tidyverse)
library(car)
### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

df <- read_parquet("../data/2022-12-06_SthlmsEnkThetas.parquet")

#---- get cutoff values based on percentiles----

#temporary removal of wellbeing index
sthlm.index <- allItems %>% 
  filter(!Index %in% c("Wellbeing","SkolaPositiv")) %>% 
  distinct(Index) %>% 
  pull()

# https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
p <- c(0.75,0.90,0.95)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

rslimits.75 <- df %>% 
  filter(!ar == 2022) |> 
  filter(DIDkommun == 'Stockholm') %>% 
  select(ar,all_of(sthlm.index)) %>% 
  group_by(ar) %>% 
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>% 
  select(ar,ends_with("75%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  t() %>%
  as.data.frame() %>% 
  pull()
rslimits.90 <- df %>% 
  filter(!ar == 2022) |> 
  filter(DIDkommun == 'Stockholm') %>% 
  select(ar,all_of(sthlm.index)) %>% 
  group_by(ar) %>% 
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>% 
  select(ar,ends_with("90%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  t() %>%
  as.data.frame() %>% 
  pull()
rslimits.95 <- df %>%
  filter(!ar == 2022) |> 
  filter(DIDkommun == 'Stockholm') %>% 
  select(ar,all_of(sthlm.index)) %>% 
  group_by(ar) %>%
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>%
  select(ar,ends_with("95%")) %>%
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>%
  t() %>%
  as.data.frame() %>%
  pull()

rslimits <- na.omit(as.data.frame(cbind(rslimits.75,rslimits.90,rslimits.95)))
rslimits <- rslimits %>% 
  rownames_to_column(var = "Index")
rslimits$Index <- sthlm.index

rslimits <- rslimits %>% 
  t() %>% 
  as.data.frame() %>% 
  janitor::row_to_names(1) %>% 
  mutate_if(is.character, as.numeric)

#rslimits

write_csv(rslimits, file = "../data/2022-12-06_rslimitsNoRev.csv")


# cutoff values for protective factors -------------------------------------

# reverse scores, making higher score = positive

df$SkolaPositiv <- df$SkolaPositiv*-1
df$Wellbeing <- df$Wellbeing*-1

# https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
p <- c(0.15,0.85)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

sthlm.index <- c("SkolaPositiv","Wellbeing")

rslimits.15 <- df %>% 
  filter(!ar == 2022) |> 
  filter(DIDkommun == 'Stockholm') %>% 
  select(ar,all_of(sthlm.index)) %>% 
  group_by(ar) %>% 
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>% 
  select(ar,ends_with("15%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  t() %>%
  as.data.frame() %>% 
  pull()
rslimits.85 <- df %>% 
  filter(!ar == 2022) |> 
  filter(DIDkommun == 'Stockholm') %>% 
  select(ar,all_of(sthlm.index)) %>% 
  group_by(ar) %>% 
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>% 
  select(ar,ends_with("85%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  t() %>%
  as.data.frame() %>% 
  pull()


rslimitsProt <- na.omit(as.data.frame(cbind(rslimits.15,rslimits.85)))
rslimitsProt <- rslimitsProt %>% 
  rownames_to_column(var = "Index")
rslimitsProt$Index <- sthlm.index

rslimitsProt <- rslimitsProt %>% 
  t() %>% 
  as.data.frame() %>% 
  janitor::row_to_names(1) %>% 
  mutate_if(is.character, as.numeric)

write_csv(rslimitsProt, file = "../data/2022-12-16_protective.csv")



## Skolinspektionen --------------------------------------------------------

# reverse scores, making higher score = positive

df <- read_parquet("../rapporter/Skolinspektionen/2022-12-20_SkolinspÅk5Scored.parquet") %>% 
  rename(DIDkommun = Kommun,
         ar = År)

#df$Indexvärde <- df$Indexvärde*-1

# https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
p <- c(0.20,0.80)
p_names <- map_chr(p, ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

sthlm.index <- c("Indexvärde")

rslimits.20 <- df %>% 
  #filter(!ar == 2022) |> 
  filter(DIDkommun == 'Stockholm') %>% 
  select(ar,all_of(sthlm.index)) %>% 
  group_by(ar) %>% 
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>% 
  select(ar,ends_with("20%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  t() %>%
  as.data.frame() %>% 
  pull()
rslimits.80 <- df %>% 
  #filter(!ar == 2022) |> 
  filter(DIDkommun == 'Stockholm') %>% 
  select(ar,all_of(sthlm.index)) %>% 
  group_by(ar) %>% 
  summarize_at(vars(sthlm.index), funs(!!!p_funs)) %>% 
  select(ar,ends_with("80%")) %>% 
  add_row(medel = !!! colMeans(.[-1], na.rm = T)) %>% 
  t() %>%
  as.data.frame() %>% 
  pull()


rslimitsProt <- na.omit(as.data.frame(cbind(rslimits.20,rslimits.80)))
rslimitsProt <- rslimitsProt %>% 
  rownames_to_column(var = "Index")
rslimitsProt$Index <- c("Positiv skolanknytning åk 5")

rslimitsProt <- rslimitsProt %>% 
  t() %>% 
  as.data.frame() %>% 
  janitor::row_to_names(1) %>% 
  mutate_if(is.character, as.numeric)

write_csv(rslimitsProt, file = "../data/2022-12-20_SkolinspLimits.csv")



# Calculate key values for each municipality and year ------------------------------

# this is done in the setup.R part of the DID Shiny app

# create function
# RSsmf <- function(df, i, j) { # input df, index, and index number
#   #j <- match(qc({{i}}),sthlm.index)
#   df %>% 
#     group_by(ar,Kommun) %>% 
#     summarise(Medel = mean({{i}}, na.rm = T),
#               StDev = sd({{i}},  na.rm = T),
#               n = n(),
#               StErr = StDev/sqrt(n),
#               sd.lo = Medel-StDev,
#               sd.hi = Medel+StDev,
#               n.90 = length(which({{i}} > rslimits[2,j]))/n*100) %>% 
#     rename(År = ar) %>% 
#     mutate(across(where(is.numeric), round, 3)) %>% 
#     as.data.frame()
# }  
