library(tidymodels)

edad <- c(15, 19, 13, NA, 20)
deporte <- c(TRUE, TRUE, NA, FALSE, TRUE)
comic_fav <- c(NA, 'Superman', 'Batman', NA, 'Batman')
mimarco <- data.frame(edad, deporte, comic_fav)
mimarco

rec <- recipe(edad ~ ., data = mimarco)

str(mimarco)
summary(rec, original = TRUE)

rec |> step_normalize(all_numeric_predictors())

rec |> step_dummy()
