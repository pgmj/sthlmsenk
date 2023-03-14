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

