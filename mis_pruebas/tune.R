source("mis_pruebas/gen_dat.R")

set.seed(123)
datos <- gen_dat(n=1000)
datos

library(tidymodels)

split_inf <- initial_split(datos, prop = 0.70)
train_data <- training(split_inf)
test_data <- testing(split_inf)

rec1 <- recipe(salario ~ ., data = datos) |>
  step_range(all_numeric_predictors()) |> 
  step_dummy(all_factor_predictors())

# resample training data
set.seed(100) # Important!
folds <- vfold_cv(train_data, v = 10, 
                  strata = salario, breaks = 4)
folds

rf_tuner <- 
  rand_forest(mtry = tune(),
              min_n = tune(),
              trees = 50
              ) |> 
  set_engine("ranger") |> 
  set_mode("regression")

rf_wflow <- workflow() |> 
  add_recipe(rec1) |> 
  add_model(rf_tuner)

rf_tune_results <-
  rf_wflow |>
  tune_grid(resamples = folds,
            metrics = metric_set(rsq, mse))

rf_tune_results |> 
  collect_metrics(summarize = TRUE)

collect_metrics(rf_tune_results) |> head(7)

show_best(rf_tune_results, metric = "mse")

show_best(rf_tune_results, metric = "rsq")

best_parameters <- select_best(rf_tune_results, metric = "mse") |>
  select(-.config)
best_parameters

best_workflow <- rf_wflow |>
  finalize_workflow(best_parameters) |>
  fit(train_data)

best_workflow

df <- tibble(
  actual = test_data$salario,
  predicted = predict(best_workflow, new_data = test_data)$.pred
)

ggplot(df, aes(x = actual, y = predicted)) +
  geom_point() +
  geom_abline()


  
  
