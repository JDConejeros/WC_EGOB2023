library(tidyverse)

# Funciones ---------------------------------------------------------------

ejemplo <- function() {
  
  if(!file.exists("Ejemplos")) dir.create("Ejemplos")
  
  for(i in 1:5) {
    if(i == 1 | i == 3) {
      data <- dplyr::tibble(Variable1 = paste0("Observación ", 1:100),
                            Variable2 = as.character(round(rnorm(100))))
    } else {
      data <- dplyr::tibble(Variable1 = paste0("Observación ", 1:100),
                            Variable2 = round(rnorm(100)))
    }
    writexl::write_xlsx(data, path = paste0("Ejemplos/data", i, ".xlsx"))
  }
  
}

analisis <- function(df) {
  
  resumen <- df %>% 
    dplyr::mutate(Variable2 = as.numeric(Variable2)) %>% 
    dplyr::summarise(`Mínimo` = min(Variable2, na.rm = TRUE),
                     Promedio = mean(Variable2, na.rm = TRUE),
                     Mediana = median(Variable2, na.rm = TRUE),
                     `Máximo` = max(Variable2, na.rm = TRUE),
                     `Desviación Estándar` = sd(Variable2, na.rm = TRUE),
                     CV = sd(Variable2, na.rm = TRUE)/abs(mean(Variable2, na.rm = TRUE)))
  return(resumen)
}

Sampling <- function(v, N, n) {
  
  promedio <- c()
  for(i in 1:N) {
    muestra <- sample(x = v, size = n, replace = TRUE)
    promedio[i] <- mean(muestra, na.rm = TRUE)
  }
  
  return(promedio)
  
}

Sampling2 <- function(v, N, n) {
  
  promedio <- c()
  for(i in 1:N) {
    muestra <- sample(x = v, size = n, replace = TRUE)
    promedio[i] <- mean(muestra, na.rm = TRUE)
  }
  
  data <- dplyr::tibble(Muestra = n, 
                        Estimacion = mean(promedio, na.rm = TRUE))
  return(data)
  
}


