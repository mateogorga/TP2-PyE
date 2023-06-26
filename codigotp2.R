intervalo_boot <- function(x, nivel) {
  B <- 1000
  
  mediana_original <- median(x)
  
  medias_bootstrap <- numeric(B)
  
  for (i in 1:B) {
    
    muestra_bootstrap <- sample(x, replace = TRUE)
    
    medias_bootstrap[i] <- median(muestra_bootstrap)
  }
  
  limite_inferior <- quantile(medias_bootstrap, (1 - nivel) / 2)
  limite_superior <- quantile(medias_bootstrap, 1 - (1 - nivel) / 2)
  
  intervalo_confianza <- c(limite_inferior, limite_superior)
  
  return(intervalo_confianza)
}



cubrimiento_empirico <- function(n) {
  B <- 1000  # Número de muestras aleatorias
  prop_contiene_mediana <- 0  # Contador para la proporción de intervalos que contienen la mediana
  suma_longitud_intervalos <- 0  # Suma de las longitudes de los intervalos
  
  for (i in 1:B) {
    # Generar una muestra aleatoria de tamaño n con distribución FΘ
    muestra<-runif(n, min = 0, max = 20)  # Ejemplo: Distribución normal con media 0 y desviación estándar 1
    
    # Calcular los límites del intervalo de confianza para la mediana
    intervalo<-intervalo_boot(muestra, nivel = 0.95)  # Ejemplo: Nivel de confianza del 95%
    
    # Verificar si el intervalo contiene a la verdadera mediana
    if (intervalo[1] <= median(muestra) && intervalo[2] >= median(muestra)) {
      prop_contiene_mediana<-prop_contiene_mediana + 1
    }
    
    # Calcular la longitud del intervalo
    longitud_intervalo<-intervalo[2] - intervalo[1]
    suma_longitud_intervalos<-suma_longitud_intervalos + longitud_intervalo
  }
  
  # Calcular la proporción de intervalos que contienen la mediana
  prop_contiene_mediana<-prop_contiene_mediana / B
  
  # Calcular la longitud promedio de los intervalos
  longitud_promedio<-suma_longitud_intervalos / B
  
  # Devolver los resultados como una lista
  resultados<-list(prop_contiene_mediana = prop_contiene_mediana, longitud_promedio = longitud_promedio)
  return(resultados)
}



# Crear una matriz para almacenar los resultados
resultados <- matrix(NA, nrow = 3, ncol = 2)
colnames(resultados) <- c("Cubrimiento empírico", "Longitud promedio")

# Tamaños de muestra
tamanos_muestra <- c(10, 100, 500)

# Calcular los resultados para cada tamaño de muestra
for (i in 1:length(tamanos_muestra)) {
  n <- tamanos_muestra[i]
  res <- cubrimiento_empirico(n)
  resultados[i, 1] <- res$prop_contiene_mediana
  resultados[i, 2] <- res$longitud_promedio
}

# Crear una tabla con los resultados
tabla_resultados <- data.frame(Tamano_Muestra = tamanos_muestra, resultados)
