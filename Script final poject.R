rm(list = ls())

setwd("C:/Users/mlara/Desktop/BD&ML/Proyecto Final/Data")

if (!require(pacman))install.packages("pacman");library(pacman)

p_load(tidyverse,rio,skimr,viridis, haven, caret, readxl)

# Data y estadísticas descriptivas--------------------------------------------------------------------

df_2014 <- filter(df, df$ano == 2014)
df_2015 <- filter(df, df$ano == 2015)
df_2016 <- filter(df, df$ano == 2016)
df_2017 <- filter(df, df$ano == 2017)
df_2018 <- filter(df, df$ano == 2018)
df_2019 <- filter(df, df$ano == 2019)
df_2020 <- filter(df, df$ano == 2020)

estadisticas_descriptivas <- df_2014 %>%
  select(`pobl_tot`, `discapital`, `disbogota`, `DF_desemp_fisc`, `y_transf`, `presidencia_votos_ganador`, voto_presidente_gano, gano_alcaldia_igual_presidente, gano_camara_igual_presidente) %>%
  psych::describe(quant = c(.25,.75)) %>%
  as_tibble(rownames = "rowname") %>%
  print()


estadisticas_descriptivas <- df_2016 %>%
  select(`pobl_tot`, `discapital`, `disbogota`, `DF_desemp_fisc`, `y_transf`, `presidencia_votos_ganador`, voto_presidente_gano, gano_alcaldia_igual_presidente, gano_camara_igual_presidente) %>%
  psych::describe(quant = c(.25,.75)) %>%
  as_tibble(rownames = "rowname") %>%
  print()

write_xlsx(estadisticas_descriptivas, "estadisticas_2014.xlsx")
write_xlsx(estadisticas_descriptivas, "estadisticas_2016.xlsx")

estadisticas_descriptivas <- df_2020 %>%
  select(`pobl_tot`, `discapital`, `disbogota`, `DF_desemp_fisc`, `y_transf`, `presidencia_votos_ganador`, voto_presidente_gano, gano_alcaldia_igual_presidente, gano_camara_igual_presidente) %>%
  psych::describe(quant = c(.25,.75)) %>%
  as_tibble(rownames = "rowname") %>%
  print()

write_xlsx(estadisticas_descriptivas, "estadisticas_2020.xlsx")

# Modelos -----------------------------------------------------------------

dt <- read_xlsx("base_caracteristicas (1).xlsx", sheet = "Hoja13", range = "A1:R7853")

na_fix <- function(x){ifelse(is.na(x)==T, 0, x)}

dt <- as.data.frame(apply(dt, 2, na_fix))

types <- apply(dt, 2, class)

dt <- as.data.frame(apply(dt, 2, as.integer))

##División de base
smp_size <- floor(0.7 * nrow(dt))

set.seed(123)
train_ind <- sample(seq_len(nrow(dt)), size = smp_size)

train <- dt[train_ind, ]
test <- dt[-train_ind, ]
  
model_base <- as.formula("y_transf_nal ~ ano + pobl_tot + discapital + disbogota + indesarrollo_mun + IGA_total + DF_desemp_fisc + presidencia_votos_ganador + voto_presidente_gano + gano_alcaldia_igual_presidente + votos_alcaldia + gano_camara_igual_presidente + votos_camara")

types <- apply(dt, 2, class)


fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
control <- trainControl(method = "cv", number = 5,
                        summaryFunction = fiveStats, 
                        classProbs = TRUE,
                        verbose=FALSE,
                        savePredictions = T)

regressControl  <- trainControl(method="repeatedcv",
                                number = 4,
                                repeats = 5) 


# Modelos de Regresión ----------------------------------------------------


regress <- train(model_base,
                 data = train,
                 method  = "lm",
                 trControl = regressControl, 
                 tuneGrid  = expand.grid(intercept = FALSE))
regress

ridge<-train(model_base,
             data = train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 0, lambda = 1))
ridge

lasso<-train(model_base,
             data = train,
             method = 'glmnet', 
             tuneGrid = expand.grid(alpha = 1, lambda = 1)) 
lasso

elasticnet <- train(model_base,
                    data = train,
                    method = 'glmnet') 
elasticnet

model_rf <- as.formula("y_transf_nal ~ ano + pobl_tot + voto_presidente_gano + gano_alcaldia_igual_presidente + gano_camara_igual_presidente")

r.forest <- train(model_base, 
                  data = train, 
                  method = "ranger")
r.forest

xgboost <- train(model_base, 
                  data = train, 
                  method = "xgbTree")
xgboost

r.tree <- train(model_base, 
                data = train, 
                method = "rpart")
r.tree

# Médidas de accuracy -----------------------------------------------------

prediccion <- predict(r.forest, test, type = "raw")

RMSE(as.numeric(prediccion$x), as.numeric(test$y_transf_nal))

MSE(as.numeric(prediccion$x), as.numeric(test$y_transf_nal))

