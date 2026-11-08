# Instalar tidymodels si no lo tienes instalado:
# install.packages("tidymodels")

# Creando un toy dataset --------------------------------------------

# 1. Crear el data.frame con 5 variables (3 cuantitativas y 2 cualitativas)
# Variable respuesta (cuanti): salario

gen_dat <- function(n) {
  exp <- rpois(n, lambda=10)         # Cuanti 1 (Predictora)
  edad <- sample(22:65, n, replace = TRUE)   # Cuanti 2 (Predictora)
  educa <- factor(sample(c("Posgrado", "Universitario", "Tecnico"), 
                        n, replace = TRUE),
                 levels=c("Tecnico", "Universitario", "Posgrado")) # Cuali 1 (Predictora)
  depto <- factor(sample(c("Ventas", "IT", "Finanzas"), 
                        n, replace = TRUE),
                  levels=c("Ventas", "IT", "Finanzas"))  # Cuali 2 (Predictora)
  media <- 3 + 5 * edad + 2 * exp + 3 * (depto == "IT") + 4 * (depto == "Finanzas") +
           10 * (educa == "Universitario") + 15 * (educa == "Posgrado")
  salario <- rnorm(n, mean = media, sd = 10)  # (Variable Respuesta de interés)
  datos <- data.frame(exp=exp, edad=edad, educa=educa, depto=depto, salario=salario)
}

datos <- gen_dat(n=10000)
datos

# Preparing the dataset ------------------------------------------------

library(tidymodels)

# Slipt the dataset (70% train, 30% test) using tidymodels
# The initial_split() function belongs to rsample package 
split_inf <- initial_split(datos, prop = 0.70)
split_inf

# Creating the train and test using the split_inf information
train_data <- training(split_inf)
test_data <- testing(split_inf)

# Transforming the data -----------------------------------------------

#----- Initialize the recipe and add steps ----
rec1 <- recipe(salario ~ ., data = datos) |>
  step_range(edad)
rec1
#----- Run the steps using the data ------
pr1 <- prep(rec1, training = datos) # I prefer datos and not train_data
pr1
#--------- Apply estimates to new data --------
train_data_transf <- bake(pr1, new_data = train_data)
test_data_transf  <- bake(pr1, new_data = test_data)

train_data_transf
train_data

test_data_transf
test_data

# Imputacion: manejando datos faltantes ---------------------------------------------

# Vamos a introducir unos NA's
datos_na <- datos
datos_na$edad[c(1, 3)] <- NA
datos_na$educa[c(2, 4)] <- NA
datos_na

rec2 <- recipe(salario ~ ., data = datos_na) |>
  step_impute_bag(edad, educa)

pr2 <- prep(rec2, training = datos_na)

bake(pr2, new_data = datos_na)

datos |> slice(1:4)

# Creando variables dummy ------------------------------------------------------

#----- Initialize the recipe and add steps ----
rec3 <- recipe(salario ~ ., data = datos) |>
  step_dummy(educa, depto)
#----- Run the steps using the data ------
pr3 <- prep(rec3, training = datos) # I prefer datos and not train_data
pr3
#--------- Apply estimates to new data --------
bake(pr3, new_data = train_data)
bake(pr3, new_data = test_data)


# Creando el modelo ----------------------------------------------------------

# Linear regression model

# 1. Specifiying the model
lm_spec <- linear_reg() %>% 
  set_engine("lm") %>% 
  set_mode("regression")

# 2. Creating the workflow
lm_workflow <- workflow() %>% 
  add_model(lm_spec) %>% 
  add_formula(salario ~ .)

# 3. Fitting the model
lm_fit <- lm_workflow %>% 
  fit(data = train_data)

tidy(lm_fit)

# Doing in a single pipe
lm_fit <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression") |>
  fit(salario ~ ., data = train_data)

# View the model coefficients and summary details
tidy(lm_fit)

# 4. Generar las predicciones para el conjunto de prueba
predicciones <- predict(lm_fit, new_data = test_data)

# 5. Combinar las predicciones con los valores reales de 'salario' para comparar
resultados <- test_data %>% 
  select(salario) %>% 
  bind_cols(predicciones)

# 6. Ver las primeras filas del resultado (valor real vs predicho)
head(resultados)

# 7. Crear un conjunto de métricas para regresión
metricas_regresion <- metric_set(rmse, mae, rsq)

# 8. Calcular las métricas usando el objeto 'resultados' 
evaluacion <- metricas_regresion(resultados, truth = salario, estimate = .pred)

# 9. Ver los resultados de la evaluación
print(evaluacion)







# K-nn

# 1. Especificar el modelo KNN (aquí definimos k = 5 vecinos como ejemplo)
knn_spec <- nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

# 2. Crear el workflow
knn_workflow <- workflow() %>% 
  add_model(knn_spec) %>% 
  add_formula(salario ~ .)

# 3. Ajustar el modelo usando el conjunto de datos
knn_fit <- knn_workflow %>% 
  fit(data = train_data)

# Haciendo todo de una vez
knn_fit <- nearest_neighbor(neighbors = 5) |>
  set_engine("kknn") |>
  set_mode("regression") |>
  fit(salario ~ ., data = train_data)

# 4. Generar las predicciones para el conjunto de prueba
predicciones <- predict(knn_fit, new_data = test_data)

# 5. Combinar las predicciones con los valores reales de 'salario' para comparar
resultados <- test_data %>% 
  select(salario) %>% 
  bind_cols(predicciones)

# 6. Ver las primeras filas del resultado (valor real vs predicho)
head(resultados)

# 7. Crear un conjunto de métricas para regresión
metricas_regresion <- metric_set(rmse, mae, rsq)

# 8. Calcular las métricas usando el objeto 'resultados' 
evaluacion <- metricas_regresion(resultados, truth = salario, estimate = .pred)

# 9. Ver los resultados de la evaluación
print(evaluacion)


