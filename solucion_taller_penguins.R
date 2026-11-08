# =====================================================================
# Taller Práctico: Construcción de Modelos de Machine Learning con tidymodels
# Dataset: penguins (palmerpenguins)
# Objetivo: Predecir el peso de los pingüinos (body_mass_g) en 
# función de las demás variables
# =====================================================================

# 1. Cargar paquetes necesarios
library(tidymodels)
library(palmerpenguins)

# Exploración y preparación de datos
data(penguins)
penguins <- drop_na(penguins) # Eliminar filas con valores faltantes

# 2. Partición de los datos (rsample)
set.seed(2026)
data_split <- initial_split(penguins, prop = 0.80, strata = body_mass_g)
train_data <- training(data_split)
test_data  <- testing(data_split)

# 3. Creación de la receta (recipes)
penguin_recipe <- recipe(body_mass_g ~ ., data = train_data) |>
  step_normalize(all_numeric_predictors(), -all_outcomes()) |> 
  step_dummy(all_nominal_predictors())
  
# 4. Definición de modelos (parsnip)
# 4.1 Modelo de Regresión Lineal Múltiple
lm_spec <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

# 4.2 Modelo de Árbol de Regresión (con hiperparámetros a afinar)
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
  ) |>
  set_engine("rpart") |>
  set_mode("regression")

# 5. Creación de workflows (workflows)
lm_wf <- workflow() |>
  add_recipe(penguin_recipe) |>
  add_model(lm_spec)

tree_wf <- workflow() |>
  add_recipe(penguin_recipe) |>
  add_model(tree_spec)

# 6. Validación cruzada para remuestreo
set.seed(123)
cv_folds <- vfold_cv(train_data, v = 5, strata = body_mass_g)

# Sintonización de hiperparámetros para el árbol de regresión (tune)
grid_vals <- grid_regular(
  cost_complexity(range = c(-3, -1)),
  tree_depth(range = c(2, 6)),
  levels = 5
)

tune_results <- tune_grid(
  tree_wf,
  resamples = cv_folds,
  grid = grid_vals,
  metrics = metric_set(mae, ccc)
)

# Mostrar mejores hiperparámetros según MAE
best_tree_params <- select_best(tune_results, metric = "mae")
print(best_tree_params)

# Finalizar el flujo de trabajo del árbol con los mejores parámetros
final_tree_wf <- finalize_workflow(tree_wf, best_tree_params)

# 7. Ajuste final y evaluación en conjunto de prueba

# Entrenando
lm_fit <- fit(lm_wf, data = train_data)
tree_fit <- fit(final_tree_wf, data = train_data)

# Calculando predicciones
lm_preds <- predict(lm_fit, new_data = test_data) |>
  bind_cols(test_data |> select(body_mass_g))

tree_preds <- predict(tree_fit, new_data = test_data) |>
  bind_cols(test_data |> select(body_mass_g))

# Evaluación de métricas con yardstick (MAE y CCC)
custom_metrics <- metric_set(mae, ccc)

cat("--- Métricas Regresión Lineal Múltiple ---\n")
print(custom_metrics(lm_preds, truth = body_mass_g, estimate = .pred))

cat("\n--- Métricas Árbol de Regresión Tuned ---\n")
print(custom_metrics(tree_preds, truth = body_mass_g, estimate = .pred))
