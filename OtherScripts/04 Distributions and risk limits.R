library(arrow)
library(tidyverse)
library(car)
library(glue)
library(janitor)
### some commands exist in multiple packages, here we define preferred ones that are frequently used
select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

df <- read_parquet("../DIDapp/data/2023-03-14_ScoredRev.parquet") %>% 
  filter(ar > 2004 & ar < 2022)

årtal <- c(2006,2008,2010,2012,2014,2016,2018,2020,2022)

#---- get cutoff values based on percentiles----

# create vector of risk index names
sthlm.index <- allItems %>% 
  filter(!Index %in% c("Wellbeing","SkolaPositiv")) %>% 
  distinct(Index) %>% 
  pull()

cutOffValues <- function(i,p) {
  p_names <- map_chr(p, ~paste0(.x*100, "%"))
  
}


cutOffValues("Utagerande", c(0.75,0.90))

p <- c(0.75,0.90)
p_names <- map_chr(p, ~paste0(.x*100, "%"))

quantiles <- df %>% 
  group_by(ar) %>% 
  reframe(enframe(quantile(Utagerande, probs = c(0.75,0.9), na.rm = T))) %>% 
  pivot_wider() %>% 
  set_names(nm = p_names)
  
mean(quantiles$`75%`)
mean(quantiles$`90%`)

# https://tbradley1013.github.io/2018/10/01/calculating-quantiles-for-groups-with-dplyr-summarize-and-purrr-partial/
p <- c(0.75,0.90)
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

write_csv(rslimits, file = glue("../DIDapp/data/{Sys.Date()}_rslimitsNoRev.csv"))


# cutoff values for protective factors -------------------------------------

# reverse scores, making higher score = positive
### Already done in script 03.
# df$SkolaPositiv <- df$SkolaPositiv*-1
# df$Wellbeing <- df$Wellbeing*-1

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

write_csv(rslimitsProt, file = glue("../DIDapp/data/{Sys.Date()}_protective.csv"))

