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
  salario <- rnorm(n, mean = media, sd = 10)  # (Variable Respuesta de interes)
  datos <- data.frame(exp=exp, edad=edad, educa=educa, depto=depto, salario=salario)
}
