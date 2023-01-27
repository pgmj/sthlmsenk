library(tidymodels)
library(arrow)
library(tidyverse)
library(dotwhisker)
library(vip)         # for variable importance plots
library(ranger)

select <- dplyr::select
count <- dplyr::count
recode <- car::recode
rename <- dplyr::rename

df <- read_parquet("../data/2023-01-10_ScoredRev.parquet") %>% 
  mutate(År = factor(ar))

rslimits <- read_csv("../data/2023-01-17_rslimitsRisk.csv")

# Fix demographics
df <- df %>% 
  rename(F6mamma = `Vilken högsta utbildning har din mamma?`,
         F6pappa = `Vilken högsta utbildning har din pappa?`)

df <- df %>% mutate(ParentEdu = case_when(F6mamma == "Universitet och högskola" | F6pappa == "Universitet och högskola" ~ "Högutbildad",
                                          TRUE ~ "Lågutbildad")
                    )

# Create risk groups for externalizing behavior
df <- df %>% 
  mutate(riskUtagerande = case_when(
    `Utagerande` < rslimits |> select(`Utagerande`) |> slice(1) |> pull() ~ "Låg risk",
    `Utagerande` >= rslimits |> select(`Utagerande`) |> slice(1) |> pull() & 
      `Utagerande` < rslimits |> select(`Utagerande`) |> slice(2) |> pull() ~ "Något förhöjd risk",
    `Utagerande` >= rslimits |> select(`Utagerande`) |> slice(2) |> pull() ~ "Förhöjd risk")
  )

# create cohort and year variables that work with lmer and ggplot, both numerical starting at 0 and as a factor
df <- df %>% 
  mutate(ÅrNum = recode(df$ar,"2006=0;2008=2;2010=4;2012=6;2014=8;2016=10;2018=12;2020=14;2022=16", as.factor = F),
         ÅrFakt = factor(ar),
         Kohort = case_when(ARSKURS == "Åk 9" &
                              ar == 2006 ~ "1",
                            ARSKURS == "Gy 2" &
                              ar == 2006 ~ "0",
                            ARSKURS == "Åk 9" &
                              ar == 2008 ~ "2",
                            ARSKURS == "Gy 2" &
                              ar == 2008 ~ "1",
                            ARSKURS == "Åk 9" &
                              ar == 2010 ~ "3",
                            ARSKURS == "Gy 2" &
                              ar == 2010 ~ "2",
                            ARSKURS == "Åk 9" &
                              ar == 2012 ~ "4",
                            ARSKURS == "Gy 2" &
                              ar == 2012 ~ "3",
                            ARSKURS == "Åk 9" &
                              ar == 2014 ~ "5",
                            ARSKURS == "Gy 2" &
                              ar == 2014 ~ "4",
                            ARSKURS == "Åk 9" &
                              ar == 2016 ~ "6",
                            ARSKURS == "Gy 2" &
                              ar == 2016 ~ "5",
                            ARSKURS == "Åk 9" &
                              ar == 2018 ~ "7",
                            ARSKURS == "Gy 2" &
                              ar == 2018 ~ "6",
                            ARSKURS == "Åk 9" &
                              ar == 2020 ~ "8",
                            ARSKURS == "Gy 2" &
                              ar == 2020 ~ "7",
                            ARSKURS == "Åk 9" &
                              ar == 2022 ~ "9",
                            ARSKURS == "Gy 2" &
                              ar == 2022 ~ "8")
  )

df$riskUtagerande <- as.factor(df$riskUtagerande)

df.model <- df %>% 
  filter(ar == 2020) %>% 
  select(Utagerande,Parenting,riskUtagerande,Kön,ARSKURS,ParentEdu,Community,SkolaNegativ) %>% 
  na.omit()

ggplot(df.model, aes(x = Utagerande, y = Parenting)) +
  geom_point(aes(color = riskUtagerande)) +
  geom_smooth(method = lm, se = F) +
  scale_color_viridis_d(option = "plasma", end = .7)

df.model %>% 
#  filter(!Utagerande == -4,
#         !Parenting == -4) %>% 
  ggplot(aes(x = Utagerande, y = Parenting)) +
  geom_point(aes(color = Kön)) +
  geom_smooth(method = lm, se = F) +
  scale_color_viridis_d(option = "plasma", end = .7)

ggplot(df.model, aes(x = Utagerande, fill = Kön)) +
  geom_histogram(alpha = 0.8)

ggplot(df.model, aes(x = Utagerande, fill = ARSKURS)) +
  geom_histogram(alpha = 0.8)

ggplot(df.model, aes(x = Utagerande, fill = ParentEdu)) +
  geom_histogram(alpha = 0.8)

## start to build model
# https://www.tidymodels.org/start/models/


# Linear model ------------------------------------------------------------

lm_mod <- linear_reg()

lm_fit <- 
  lm_mod %>% 
  fit(Utagerande ~ Parenting + Kön + ARSKURS + ParentEdu + Community + SkolaNegativ, data = df.model)

tidy(lm_fit)

tidy(lm_fit) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))


# Risk level outcome ------------------------------------------------------

## using risk level as outcome (factor with 3 levels)

## split data

splits      <- initial_split(df.model, strata = Kön)

df_other <- training(splits)
df_test  <- testing(splits)

val_set <- validation_split(df_other, 
                            strata = Kön, 
                            prop = 0.80)

# Multinomial model -------------------------------------------------------

lr_mod <- 
  multinom_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lr_fit <- 
  lr_mod %>% 
  fit(riskUtagerande ~ Parenting + Kön + ARSKURS + ParentEdu + Community + SkolaNegativ, data = df.model)

lr_recipe <- 
  recipe(riskUtagerande ~ Parenting + Kön + ARSKURS + ParentEdu + Community + SkolaNegativ, data = df.model) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())

lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)


lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 
top_models

lr_best <- lr_res %>% 
  select_best()

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(riskUtagerande, `.pred_Förhöjd risk`, `.pred_Låg risk`, `.pred_Något förhöjd risk`) %>% 
  mutate(model = "Multinomial Regression")

autoplot(lr_auc)

last_lr_mod <- 
  multinom_reg(penalty = lr_best$penalty, mixture = 1) %>% 
  set_engine("glmnet")

last_lr_workflow <- 
  lr_workflow %>% 
  update_model(last_lr_mod)

last_lr_fit <- 
  last_lr_workflow %>% 
  last_fit(splits)

last_lr_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

## testing a tree based model

cores <- 8

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

rf_recipe <- 
  recipe(riskUtagerande ~ Parenting + Kön + ARSKURS + ParentEdu + Community + SkolaNegativ, data = df.model)

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>% 
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")

rf_res %>% 
  collect_predictions()

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(riskUtagerande, `.pred_Förhöjd risk`, `.pred_Låg risk`, `.pred_Något förhöjd risk`) %>% 
  mutate(model = "Random Forest")

bind_rows(rf_auc, lr_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(linewidth = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6)

# The mtry hyperparameter sets the number of predictor variables that each node in the decision tree “sees” and can learn about, 
# so it can range from 1 to the total number of features present; when mtry = all possible features, 
# the model is the same as bagging decision trees. The min_n hyperparameter sets the minimum n to split at any node.

last_rf_mod <- 
  rand_forest(mtry = 6, min_n = 7, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)

last_rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(riskUtagerande, `.pred_Förhöjd risk`, `.pred_Låg risk`, `.pred_Något förhöjd risk`) %>% 
  autoplot()


# Risk with 2 levels ------------------------------------------------------



df.model <- df.model %>% 
  mutate(riskUtagerande2 = recode(riskUtagerande,"'Något förhöjd risk'='Låg risk'"))

splits      <- initial_split(df.model, strata = Kön)

df_other <- training(splits)
df_test  <- testing(splits)

val_set <- validation_split(df_other, 
                            strata = Kön, 
                            prop = 0.80)

lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

lr_fit <- 
  lr_mod %>% 
  fit(riskUtagerande2 ~ Parenting + Kön + ARSKURS + ParentEdu + Community + SkolaNegativ, data = df.model)

lr_recipe <- 
  recipe(riskUtagerande2 ~ Parenting + Kön + ARSKURS + ParentEdu + Community + SkolaNegativ, data = df.model) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors())

lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)


lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 
top_models

lr_best <- lr_res %>% 
  select_best()

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(riskUtagerande2, `.pred_Förhöjd risk`) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

last_lr_mod <- 
  logistic_reg(penalty = lr_best$penalty, mixture = 1) %>% 
  set_engine("glmnet")

last_lr_workflow <- 
  lr_workflow %>% 
  update_model(last_lr_mod)

last_lr_fit <- 
  last_lr_workflow %>% 
  last_fit(splits)

last_lr_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)


