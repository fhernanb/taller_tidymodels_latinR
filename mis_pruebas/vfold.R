source("mis_pruebas/gen_dat.R")

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
folds <- vfold_cv(data = train_data, v = 10)
folds

set.seed(100) # Important!
folds <- vfold_cv(data = train_data, v = 10, 
                  strata = salario, breaks = 4)
folds

# mod 1 --------------------------------------------------
lm_spec <- linear_reg() |>  
  set_engine("lm") |> 
  set_mode("regression")

lm_wflow <- workflow() |> 
  add_recipe(rec1) |> 
  add_model(lm_spec)

lm_results <-
  lm_wflow |> 
  fit_resamples(resamples = folds,
                metrics = metric_set(rsq, mse))

lm_results |> 
  collect_metrics(summarize = FALSE)

lm_results |> 
  collect_metrics(summarize = TRUE)


# mod 2 --------------------------------------------------
glm_spec <- linear_reg() |>  
  set_engine("glm", family = stats::Gamma(link = "log")) |> 
  set_mode("regression")

glm_wflow <- workflow() |> 
  add_recipe(rec1) |> 
  add_model(glm_spec)

glm_results <-
  glm_wflow |> 
  fit_resamples(resamples = folds,
                metrics = metric_set(rsq, mse))

glm_results

glm_results |> 
  collect_metrics(summarize = FALSE)

glm_results |> 
  collect_metrics(summarize = TRUE)

# mod 3 --------------------------------------------------
rf_spec <- 
  rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("regression")

rf_wflow <- workflow() |> 
  add_recipe(rec1) |> 
  add_model(rf_spec)

rf_results <-
  rf_wflow |> 
  fit_resamples(resamples = folds,
                metrics = metric_set(rsq, mse))

rf_results

rf_results |> 
  collect_metrics(summarize = FALSE)

rf_results |> 
  collect_metrics(summarize = TRUE)



