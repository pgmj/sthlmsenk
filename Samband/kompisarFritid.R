df.all %>%
  select(all_of(items.removed)) %>%
  mutate(F70 = car::recode(F70,"1=0;2=1;3=1")) %>% 
  select(!F70) %>% 
  na.omit() %>% 
  PCM() %>% 
  item_restscore()

# utan F70, pga har med mig själv att göra, inte mina kompisar
out2 <- df.all %>%
  select(all_of(items.removed)) %>%
  mutate(F70 = car::recode(F70,"1=0;2=1;3=1")) %>% 
  select(!F70) %>% 
  na.omit() %>% 
  RIestThetas()

out %>% 
  mutate(risk = ifelse(WLE > 0.5, "Förhöjd risk", "Låg risk")) %>% 
  count(risk) %>% 
  mutate(Procent = 100 * n / sum(n))

hist(out$WLE,breaks = 25)

count(out,WLE)

out %>% 
  mutate(risk = ifelse(WLE < -1, "Skydd", "Mindre skydd")) %>% 
  count(risk) %>% 
  mutate(Procent = 100 * n / sum(n))

item_restscore(PCM(df.pos %>% select(!F70)))
RIitemfit(df.pos %>% select(!F70))  

