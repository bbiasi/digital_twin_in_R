

# Brenner Biasi S. Silva

# referencia
# https://medium.com/data-science/how-to-build-a-digital-twin-b31058fd5d3e


# Script ----

## Pacotes ----
library(pacman)
pacman::p_load(readr, tidyverse, GGally, ggh4x,
               tidymodels, xgboost, keras, brulee, ranger, torch, kknn)

## Dados e analises 1 ----
discharge <- read_csv("discharge.csv")

glimpse(discharge)
table(discharge$Battery)
table(discharge$id_cycle)

# filtrando para B0005
df <- discharge |> 
  dplyr::filter(Battery == "B0005")

# 
# df |> 
#   dplyr::filter(id_cycle < 8) |> 
#   ggpairs(columns = c(1:5, 7),
#           aes(colour = as.character(id_cycle), alpha = 0.4))
# 
# df |> 
#   dplyr::filter(id_cycle < 160) |> 
#   ggpairs(columns = c(1:5, 7),
#           aes(colour = as.character(id_cycle), alpha = 0.4))

df |> 
  ggplot(aes(x = Time, y = Temperature_measured)) +
  geom_line(aes(color = as.character(id_cycle)), show.legend = F) +
  theme_minimal()


# maximo por ciclo
# Usa os valores máximos/mínimos que representam o estado da bateria em cada 
# ciclo completo
dfb <- df |> 
  dplyr::group_by(id_cycle) |> 
  dplyr::summarise(across(everything(), max, na.rm = TRUE))

# Calcular tempo cumulativo
dfb <- dfb |> 
  dplyr::mutate(Cumulated_T = cumsum(Time))

p0 <- dfb |>
  ggplot(aes(x = Cumulated_T/3600)) +
  geom_point(aes(y = Capacity, color = "NASA dataset"), size = 2, shape = 4) +
  labs(
    title = "Degradação de bateria Lithium-Ion",
    x = "Tempo trabalhado [horas]",
    y = "Capacidade Observada [Ahr]",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("NASA dataset" = "grey50"),
    breaks = c("NASA dataset"), 
    guide = guide_legend(override.aes = list(
      linetype = 0,  
      shape = 4,     
      size = 2       
    ))
  ) +
  theme_minimal()
p0

## 1 Modelo fisico ----
# Capacidade no instante zero
initial_capacity <- dfb$Capacity[1]

# Parâmetro K admensional
K <- 0.13

# Cálculo de L_1
L_1 <- 1 - exp(-K * as.numeric(dfb$id_cycle) * dfb$Temperature_measured / dfb$Time)

# Cálculo da Capacidade Corrigida (C. Capacity)
dfb <- dfb |> 
  dplyr::mutate(`Capacidade Corrigida` = -(L_1 * initial_capacity) + initial_capacity) |> 
  dplyr::rename("Capacidade" = "Capacity")

p1 <- dfb |>
  ggplot(aes(x = id_cycle)) +
  geom_line(aes(y = `Capacidade Corrigida`, color = "Modelo Físico"), 
            linewidth = 0.8) +
  geom_point(aes(y = Capacidade, color = "Capacidade Observada"), 
             size = 2, shape = 8) +
  annotate("text", x = 22, y = 1.3, label = "Brenner B. S. Silva",
           color = "orange", size = 3, fontface = "bold") +
  scale_color_manual(
    name = "Modelo",
    values = c(
      "Modelo Físico" = "blue",
      "Capacidade Observada" = "black"
    ),
    breaks = c("Modelo Físico",
               "Capacidade Observada"),
    guide = guide_legend(
      override.aes = list(
        shape = c(NA, 8)
      ),
      order = 1  
    )
  ) +
  labs(
    title = "Modelo de degradação de bateria",
    x = "Ciclos",
    y = "Capacidade (Ah)"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    legend.position = c(0.78, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.spacing.y = unit(0.2, "cm"),  
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
p1
ggsave("plot2.jpeg",
       plot = p1,
       width = 22,
       height = 14,
       units = c("cm"),
       dpi = 400)


dfb |>
  ggplot(aes(x = Cumulated_T/3600)) +
  geom_line(aes(y = `Capacidade Corrigida`, color = "Modelo Físico"), linewidth = 0.8) +
  geom_point(aes(y = Capacidade, color = "NASA dataset"), size = 2, shape = 4) +
  labs(
    title = "Degradação de bateria Lithium-Ion",
    x = "Ciclos",
    y = "Capacidade Corrigida [Ahr]",
    color = "Legenda"
  ) +
  scale_color_manual(
    values = c("Modelo Físico" = "navy", "NASA dataset" = "grey50"),
    breaks = c("Modelo Físico", "NASA dataset"),  # ordem da legenda
    guide = guide_legend(override.aes = list(
      linetype = c(1, 0),
      shape = c(NA, 4),
      size = c(0.8, 2)
    ))
  ) +
  theme_minimal()





## 2 Modelo Híbrido ----
### 2.1 Preparação dos dados ----
# Criando o dataframe com as variáveis necessárias
df_hybrido <- dfb |> 
  dplyr::select(`Capacidade Corrigida`, Capacidade) |> 
  dplyr::mutate(residual = Capacidade - `Capacidade Corrigida`) |> 
  drop_na()

# Definindo split 75% treino / 25% teste
{
  set.seed(1)
  split <- df_hybrido |> rsample::initial_split(prop = 0.75)
  train_data <- split |> rsample::training()
  test_data <- split |> rsample::testing()
  }

### 2.2 Pré-processamento ----
recipe <- recipes::recipe(residual ~ `Capacidade Corrigida`, 
                          data = train_data)

### 2.3 Definindo os modelos ----
#### RF ----
# Random Forest
rf_spec <- parsnip::rand_forest(
  mtry = tune(),
  trees = 1500,
  min_n = tune()) |> 
  set_engine("ranger") |> 
  set_mode("regression")

#### XGB ----
# B) XGBoost
xgb_spec <- parsnip::boost_tree(
  mtry = tune(),
  trees = 1500,
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune()) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

#### RNA ----
# C) Redes Neurais # 01 camada oculta
mlp_spec <- parsnip::mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = 100) |> 
  set_engine("nnet") |> 
  set_mode("regression")

#### KNN ----
# D) KNN
knn_spec <- parsnip::nearest_neighbor(
  neighbors = tune(),
  weight_func = tune()) |> 
  set_engine("kknn")  |> 
  set_mode("regression")

## 2.4 Workflow e Tuning ----
# Criando workflow
wf <- workflows::workflow() |> 
  workflows::add_recipe(recipe)

# Configuração para validação cruzada
folds <- train_data |> rsample::vfold_cv(v = 10)

# Função para tunar e avaliar modelos
train_model <- function(spec, model_name) {
  cat("\nTreinando", model_name, "...\n")
  
  # Adiciona modelo ao workflow
  wf_model <- wf |>  
    add_model(spec)
  
  # Tuning de hiperparâmetros
  tuned <- tune_grid(
    wf_model,
    resamples = folds,
    grid = 10,  # 10 combinações de hiperparâmetros
    control = control_grid(verbose = TRUE)
  )
  
  # Seleciona melhores parâmetros
  best_params <- select_best(tuned, metric = "rmse")
  
  # Finaliza o modelo com os melhores parâmetros
  final_model <- finalize_workflow(wf_model, best_params) %>%
    fit(data = train_data)
  
  # Retorna modelo treinado
  return(final_model)
}

## 2.5 Treinando todos os modelos ----
models <- list(
  "Random Forest" = rf_spec,
  "XGBoost" = xgb_spec,
  "Rede Neural" = mlp_spec,
  "KNN" = knn_spec
)

trained_models <- map2(models, names(models), ~train_model(.x, .y))

## 2.6 Avaliação no conjunto de teste ----
results <- map_dfr(trained_models, function(model) {
  preds <- predict(model, test_data) %>% 
    bind_cols(test_data)
  
  metrics <- metric_set(rmse, rsq, mae)
  eval <- metrics(preds, truth = residual, estimate = .pred)
  
  return(eval)
}, .id = "Model")

# Exibe resultados
print(results)

## 2.7 Previsões com o melhor modelo ----
# "Melhor modelo" baseado no RMSE
best_model_name <- results |> 
  dplyr::filter(`.metric` == "rmse") |> 
  dplyr::arrange(`.estimate`) |> 
  dplyr::slice(1) |> 
  dplyr::pull(Model)

best_model <- trained_models[[best_model_name]]

# Predições finais
final_predictions <- predict(best_model, df_hybrido) |> 
  dplyr::bind_cols(df_hybrido) |> 
  dplyr::mutate(hybrid_pred = `Capacidade Corrigida` + .pred)  # Modelo híbrido = físico + correção ML

# Visualização dos resultados
p2 <- final_predictions |> 
  ggplot(aes(x = `Capacidade Corrigida`, y = residual)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = .pred), color = "red", size = 1) +
  labs(title = paste("Resíduos vs Predição:", best_model_name),
       x = "Capacidade Prevista pelo Modelo Físico",
       y = "Resíduo (Real - Previsto)") +
  theme_minimal()

p2

## 2.8 Compilado para digital twin hibrido ----
# Criar o dataframe com os dados do modelo híbrido
model_blueprint <- best_model$pre$mold$blueprint

# 2.8.2 Criar um novo dataframe formatado exatamente como o modelo espera
new_data <- dfb |> 
  dplyr::select(`Capacidade Corrigida`) |> 
  as.data.frame() |> 
  setNames(names(best_model$pre$mold$predictors))

# 2.8.3 Fazer as previsões corretamente
residual_predictions <- predict(best_model, new_data = new_data)

# 2.8.4 Construir o dataframe final do twin híbrido
df_hybrido <- dfb |> 
  dplyr::mutate(
    X_in = `Capacidade Corrigida`,
    X_out = Capacidade - `Capacidade Corrigida`,
    residual_pred = residual_predictions$.pred,
    X_twin = `Capacidade Corrigida` + residual_pred,
    Cycle = row_number()
  )

### Comparação do Twin Híbrido ----
df_hybrido |> 
  ggplot(aes(x = Cycle)) +
  geom_line(aes(y = X_twin, color = "Híbrido Digital Twin"), linewidth = 1.2) +
  geom_line(aes(y = `Capacidade Corrigida`, color = "Modelo Físico"), 
            linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(y = Capacidade, color = "Capacidade Observada"), 
             shape = 4, size = 2.5, alpha = 0.8) +
  scale_color_manual(
    name = "Modelo",
    values = c(
      "Híbrido Digital Twin" = "firebrick",
      "Modelo Físico" = "navy",
      "Capacidade Observada" = "grey40"
    ),
    breaks = c("Híbrido Digital Twin", "Modelo Físico", "Capacidade Observada"),
    guide = guide_legend(
      override.aes = list(
        linetype = c("solid", "dashed", "blank"),
        shape = c(NA, NA, 4)
      ),
      order = 1  
    )
  ) +
  labs(
    title = "Comparação de híbrido twin com modelo físico e dados observados",
    subtitle = "Capacidade de degradação da bateria",
    x = "Ciclos",
    y = "Capacidade (Ah)"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#f2f8fd"),
    panel.background = element_rect(fill = "white"),
    legend.position = c(0.78, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.spacing.y = unit(0.2, "cm"),  
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )


## 3 Modelo Empírico ----
glimpse(dfb)

### 3.1 Pré-processamento dos dados ----
df_model <- dfb |> 
  dplyr::select(-c(Battery, id_cycle, type)) 

### 3.2 Divisão dos dados ----
{
  set.seed(1)
  split <- rsample::initial_split(df_model, prop = 0.75)
  train_data <- rsample::training(split)
  test_data <- rsample::testing(split)
  }

### 3.3 Pré-processamento ----
recipe <- recipes::recipe(`Capacidade Corrigida` ~ ., data = train_data) |> 
  recipes::step_zv(all_predictors()) %>%  # Remove variáveis com variância zero
  recipes::step_dummy(all_nominal_predictors()) |>  # Convertendo fatores em dummy variables
  recipes::step_normalize(all_numeric_predictors())  # Normalizando variáveis numéricas

### 3.4 Definindo os modelos ----
#### KNN ----
knn_model <- parsnip::nearest_neighbor(
  mode = "regression",
  neighbors = tune()) |> 
  set_engine("kknn")

#### RF ----
rf_model <- parsnip::rand_forest(
  mode = "regression",
  mtry = tune(),
  trees = 1500,
  min_n = tune()) |> 
  set_engine("ranger")

### 3.5 Workflows ----
knn_workflow <- workflow() |> 
  add_recipe(recipe) |> 
  add_model(knn_model)

rf_workflow <- workflow() |> 
  add_recipe(recipe) |> 
  add_model(rf_model)

### 3.6 Tuning de hiperparâmetros ----
# KNN
knn_grid <- grid_regular(
  neighbors(range = c(3, 30)),
  levels = 10
)

# Random Forest
rf_grid <- grid_regular(
  mtry(range = c(2, 8)),  # Ajustado para o número máximo de preditores
  min_n(range = c(2, 20)),
  levels = 5
)

### 3.7 Validação cruzada ----
folds <- rsample::vfold_cv(train_data, v = 10)

# Tuning KNN
knn_tuned <- knn_workflow |> 
  tune_grid(
    resamples = folds,
    grid = knn_grid,
    metrics = metric_set(rmse, rsq, mae, mape)
  )

# Tuning Random Forest
rf_tuned <- rf_workflow |> 
  tune_grid(
    resamples = folds,
    grid = rf_grid,
    metrics = metric_set(rmse, rsq, mae, mape)
  )



### 3.8 Selecionando os melhores parâmetros ----
best_knn <- select_best(knn_tuned, metric = "rmse")
best_rf <- select_best(rf_tuned, metric = "rmse")

### 3.9 Finalizando os modelos ----
final_knn <- finalize_workflow(knn_workflow, best_knn)
final_rf <- finalize_workflow(rf_workflow, best_rf)

### 3.10 Treinando os modelos finais ----
knn_fit <- fit(final_knn, data = train_data)
rf_fit <- fit(final_rf, data = train_data)

# 11. Avaliação nos dados de teste
test_results <- test_data |> 
  dplyr::bind_cols(
    predict(knn_fit, new_data = test_data) |>  rename(knn_pred = .pred),
    predict(rf_fit, new_data = test_data) |> rename(rf_pred = .pred)
  )

# Métricas de avaliação
metrics <- yardstick::metric_set(rmse, rsq, mae)

knn_metrics <- test_results |> 
  metrics(truth = `Capacidade Corrigida`, estimate = knn_pred)

rf_metrics <- test_results |> 
  metrics(truth = `Capacidade Corrigida`, estimate = rf_pred)

# 12. Visualização dos resultados
list(
  KNN = knn_metrics,
  Random_Forest = rf_metrics,
  Best_KNN_Params = best_knn,
  Best_RF_Params = best_rf
)


## Comparativo ----
# Gerar previsões do Random Forest para todos os dados
rf_predictions <- predict(rf_fit, new_data = dfb) |> 
  dplyr::rename(rf_pred = .pred)

# Atualizar o dataframe com as previsões do RF
df_hybrido <- df_hybrido |>  
  dplyr::bind_cols(rf_predictions)

# Gráfico atualizado com as 4 séries
px <- df_hybrido |> 
  ggplot(aes(x = Cycle)) +
  geom_point(aes(y = Capacidade, color = "Capacidade Observada"), 
            shape = 8) +
  geom_line(aes(y = X_twin, color = "Híbrido Digital Twin"),
            linewidth = 0.8) +
  geom_line(aes(y = rf_pred, color = "Random Forest"), 
            linewidth = 1.6, alpha = 0.6) +
  geom_line(aes(y = `Capacidade Corrigida`, color = "Modelo Físico"), 
            linewidth = 1.6) +
  annotate("text", x = 22, y = 1.3, label = "Brenner B. S. Silva",
           color = "orange", size = 3, fontface = "bold") +
  scale_color_manual(
    name = "Modelo",
    values = c(
      "Híbrido Digital Twin" = "#E1BD6D",#23775E
      "Modelo Físico" = "blue",
      "Random Forest" = "#CB2314",
      "Capacidade Observada" = "black"
    ),
    breaks = c("Híbrido Digital Twin", "Modelo Físico",
               "Random Forest", "Capacidade Observada"),
    guide = guide_legend(
      override.aes = list(
        linetype = c("solid", "solid", "solid", "blank"),
        shape = c(NA, NA, NA, 8)
      ),
      order = 1  
    )
  ) +
  labs(
    title = "Comparação de modelos de degradação de bateria",
    subtitle = "Híbrido Twin vs Modelo Físico vs Random Forest vs Observado",
    x = "Ciclos",
    y = "Capacidade (Ah)"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    legend.position = c(0.78, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.spacing.y = unit(0.2, "cm"),  
    legend.text = element_text(size = 9),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  )
px

ggsave("plot1.jpeg",
       plot = px,
       width = 22,
       height = 14,
       units = c("cm"),
       dpi = 400)



# Fim -----
