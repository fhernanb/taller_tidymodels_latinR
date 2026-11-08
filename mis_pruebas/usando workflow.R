# Ejemplo en el que se muestra como usando workflows
# se puede entrenar un modelo sin tener que darle los
# datos para train transformados

library(tidymodels)

# 1. Split the data
datos <- mtcars |> select(mpg, wt, hp, qsec)
head(datos)

set.seed(123)
data_split <- initial_split(datos, prop = 0.8)
train_data <- training(data_split)
test_data  <- testing(data_split)

# 2. Define the recipe (Transform PREDICTOR variables here)
# Note: Transform predictors inside the recipe. 
# For the OUTCOME, it's best to transform it in the data or 
# use a target workflow.
lm_recipe <- recipe(mpg ~ wt + hp + qsec, data = train_data) |>
  step_log(wt, base = 10) |>           # Transform a predictor
  step_normalize(all_predictors())     # Center and scale predictors
  
# 3. Define the model spec
lm_spec <- linear_reg() |>  
  set_engine("lm") |> 
  set_mode("regression")

# 4. Create a Workflow
lm_wflow <- workflow() |>
  add_recipe(lm_recipe) |>
  add_model(lm_spec)

# 5. Fit the model
lm_fit <- fit(lm_wflow, data = train_data)

lm_fit

lm_fit |> 
  extract_fit_parsnip() |> 
  tidy()

lm_fit$pre$mold$outcomes
lm_fit$pre$mold$predictors

train_data |> head()

# 6. Predict (Predictors are automatically transformed; 
# outputs are on the original scale)
pred <- predict(lm_fit, new_data = test_data)

bind_cols(test_data, pred)

# 7. Predicting for new observations
# We can create a data frame with the data
new_data <- data.frame(wt=c(2.8, 3.4, 4.0),
                       hp=c(110, 105, 180),
                       qsec=c(17.02, 20.22, 17.40))

predict(lm_fit, new_data = new_data)

