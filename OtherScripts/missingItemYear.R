
missingItemYear <- function(data, item) {
  data %>% 
    mutate(År = factor(ar)) %>% 
    group_by(År) %>% 
    count({{ item }}) %>% 
    rename(Svarskategorier = {{ item }}) %>% 
    ggplot(aes(x = Svarskategorier, y = n, fill = År, group = År)) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d() +
    theme_minimal(base_size = 14, base_family = "Lato") +
    ylab("Antal svar") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
}

df.södertälje %>% 
  filter(ar %in% c(2018,2020,2022)) %>% 
  missingItemYear(f101b)

missingItemYearP <- function(data, item) {
  data %>% 
    mutate(År = factor(ar)) %>% 
    group_by(År) %>% 
    count({{ item }}) %>% 
    mutate(Andel = n * 100/ sum(n)) %>% 
    rename(Svarskategorier = {{ item }}) %>% 
    ggplot(aes(x = Svarskategorier, y = Andel, fill = År, group = År)) +
    geom_col(position = "dodge") +
    scale_fill_viridis_d() +
    theme_minimal(base_size = 14, base_family = "Lato") +
    ylab("Andel svar i procent") +
    scale_x_discrete(guide = guide_axis(n.dodge = 2))
}

df.södertälje %>% 
  filter(ar %in% c(2018,2020,2022)) %>% 
  missingItemYearP(f101b)