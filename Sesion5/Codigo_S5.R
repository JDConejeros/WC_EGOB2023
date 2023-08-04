########################################################################/
# Campamento de invierno ------
# Escuela de Gobierno UC - 02/08/2023
########################################################################/
# Manipulación de tablas y objetos
# José Daniel Conejeros - jdconejeros@uc.cl
########################################################################/

# En este código aplicaremos algunos temas de programación funcional 

########################################################################/
# Tema 1. Librerias y datos -------------
########################################################################/

# Desactivar notación científica y limpiar la memoria
options(scipen=999)
rm(list=(ls()))

# Vamos a cargar las las librerías 
#install.packages("") # Puedes escribir entre las comillas sino tienes instalada alguna librería
library(rio)      # Importar BBDD
library(dplyr)    # Manipulación de datos
library(purrr)    # Programación funcional
library(datos)    # Para trabajar con tablas de datos de ejemplo
library(ggplot2)  # Aplicamos a nuestros datos 
library(patchwork) # Compactar figuras

# Cargamos datos de ejemplo 
data <- datos::paises
glimpse(data)

########################################################################/
# Tema 2. Estructuras de control -------------
########################################################################/

## 2.1 Operadores logicos ----------------------------------------------------

10 > 4
10 < 4
10 <= 4
10 >= 4
10 == 4
10 != 4

"UDP" == "udp"

# Existen vectores por defecto en R (llamados constantes). 
# Como por ejemplo: letters y LETTERS
letters
LETTERS
month.abb
month.name
pi

# Evaluemos un condicional 
c("z", "B", "c") %in% letters
c("a", "B", "c") == letters ### Cuidado con el "=="

# Complemento
c("a", "B", "c") != letters
!(c("z", "B", "c") %in% letters)

# Con nuestros datos
data$esperanza_baja <- data$esperanza_de_vida <= mean(data$esperanza_de_vida)
table(data$esperanza_baja)

## 2.2 Comando if ------------------------------------------------------------
# Condicional
# Parentesis: Recuerde diferenciar los distintos tipos de parentesis:
ejemplo <- c(4, 5, 8, 9)
ejemplo[2]
mean(ejemplo)

# Ejemplo 1
nota <- 5
if(nota >= 4){
  print("¡Felicitaciones!")
}

if(is.numeric(nota) & (nota > 4 | nota == 4) & nota <= 7) {
  print("¡Felicitaciones!")
  print("Nos vemos el proximo semestre")
}

mensaje <- "Buenos dias"
if(mensaje == "Hola" | mensaje == "Buenos dias") {
  print("El mensaje es un saludo cordial")
}

if(mensaje %in% c("Hola", "Buenos dias", "Que tal")) {
  print("El mensaje es un saludo cordial")
} 

# Con nuestros datos
data$esperanza_de_vida[1] 

if(data$esperanza_de_vida[1] <= mean(data$esperanza_de_vida)) {
  print(paste0("Esperanza de vida bajo el promedio: ", data$pais[1], ", Año:", data$anio[1]))  
}

data$esperanza_de_vida[2] 

if(data$esperanza_de_vida[2] <= mean(data$esperanza_de_vida)) {
  print(paste0("Esperanza de vida bajo el promedio: ", data$pais[2], ", Año:", data$anio[2]))  
}

data$esperanza_de_vida[15] # Albania

if(data$esperanza_de_vida[15] <= mean(data$esperanza_de_vida)) {
  print(paste0("Esperanza de vida bajo el promedio: ", data$pais[15], ", Año:", data$anio[15]))  
}

# ¿Cómo lo podemos hacer para para todas las observaciones? 

## 2.3 Comando else ------------------------------------------------------------
# La manera de trabajar los que no cumplen con la condición 
mensaje <- "Cualquier cosa"
if(mensaje %in% c("Hola", "Buenos dias", "Que tal")) {
  print("El mensaje es un saludo cordial")
} else {
  print("El mensaje no es un saludo cordial")
}

# Ejemplo 2
nota <- 3.999999999
if(is.numeric(nota) & (nota > 4 | nota == 4) & nota <= 7) {
  print("¡Felicitaciones!")
} else {
  print("Reprobaste")
}

# Con nuestros datos
data$esperanza_de_vida[15] 

if(data$esperanza_de_vida[15] <= mean(data$esperanza_de_vida)) {
  print(paste0("Esperanza de vida bajo el promedio: ", data$pais[15], ", Año:", data$anio[15]))
} else {
  print(paste0("Esperanza de vida sobre el promedio: ", data$pais[15], ", Año:", data$anio[15]))  
}

# ¿Qué pasa si lo queremos aplicar a todas las observaciones?
if(data$esperanza_de_vida <= mean(data$esperanza_de_vida)) {
  print(paste0("Esperanza de vida bajo el promedio: ", data$pais, ", Año:", data$anio))
} else {
  print(paste0("Esperanza de vida sobre el promedio: ", data$pais, ", Año:", data$anio))  
}

# Genera un error porque este flujo no está vectorizado. 

## 2.4 Comando ifelse ------------------------------------------------------------
# Condicional binario (vectorizado)
# Las funciones a continuación están hechas para vectores 
ifelse(nota >= 4,
       "¡Felicitaciones!",
       "Reprobaste")

ifelse(nota >= 1:10,
       "¡Felicitaciones!",
       "Reprobaste")

# Con nuestros datos
ifelse(data$esperanza_de_vida <= mean(data$esperanza_de_vida),
       paste0("Esperanza de vida bajo el promedio: ", data$pais, ", Año:", data$anio), 
       paste0("Esperanza de vida sobre el promedio: ", data$pais, ", Año:", data$anio))

# Podemos guardar en un vector 
data$resultado <- ifelse(data$esperanza_de_vida <= mean(data$esperanza_de_vida),
                         paste0("Esperanza de vida bajo el promedio: "), 
                         paste0("Esperanza de vida sobre el promedio: "))
table(data$resultado)

## 2.5 Comando else if -----------------------------------------------------------
# Podemos considerar múltiples condiciones (no está vectorizado)
nota <- " 4"
nota <- 8
class(nota)
nota <- as.numeric(nota)

if(!is.numeric(nota) | (nota < 1) | (nota > 7)) {
  print("Error, ingrese una nota correcta")
} else if (nota >= 4) {
  print("Felicitaciones")
} else {
  print("Reprobaste")
}

ifelse(!is.numeric(nota) | (nota < 1) | (nota > 7),
       "Error, ingrese una nota correcta",
       ifelse(nota >= 4, "Felicitaciones", "Reprobaste")
)

## 2.6 Comando case_when -----------------------------------------------------------
# Multiples condiciones (vectorizado)
library(dplyr) # Cargamos la librería que contiene la función

nota <- seq(5, 7.1, by=0.1)
case_when(!is.numeric(nota) ~ "Error, ingrese un número.",
          nota < 1 ~ "Error, ingrese un número entre 1 y 7.",
          nota > 7 ~ "Error, ingrese un número entre 1 y 7.",
          nota >= 4 ~ "¡Felicitaciones!",
          nota < 4 ~ "Reprobaste")

# Con datos 
data <- data %>% 
  mutate(resultado =
           case_when(
             esperanza_de_vida > mean(esperanza_de_vida) ~ "Mayor al promedio",
             esperanza_de_vida == mean(esperanza_de_vida) ~ "Igual al promedio",
             esperanza_de_vida < mean(esperanza_de_vida) ~ "Menor al promedio",
             TRUE ~ NA_character_))
table(data$resultado, useNA = "ifany")

## 2.7 Iteraciones: comando for -----------------------------------------------------------

# Veamos algunos ejemplos iniciales 
for(i in 1:5){
  print(i^2 + i)
}

x <- 1:5
y <-  c()
for(i in 1:5){
  y[i] = x[i]^2
}
y

# Función sample() permite generar una muestra aleatoria 
set.seed(2023) # Definimos una semilla
nota <- sample(x = seq(1, 7, by = 0.1), # Valores de la muestra
               size = 40,               # Tamaño de la muestra
               replace = TRUE)          # Valores con reemplazo

for(i in 1:length(nota)) {
  if(!is.numeric(nota[i]) | (nota[i] < 1) | (nota[i] > 7)) {
    print("Error, ingrese una nota correcta")
  } else if (nota[i] >= 4) {
    print("Felicitaciones")
  } else {
    print("Reprobaste")
  }
}

# Guardemos el objeto del mensaje
mensaje_alumno <- c()
mensaje_alumno <- character(40)
for(i in seq(2, 40, by = 2)) {
  mensaje_alumno[i] <- case_when(!is.numeric(nota[i]) ~ "Error, ingrese un número.",
                                 nota[i] < 1 ~ "Error, ingrese un número entre 1 y 7.",
                                 nota[i] > 7 ~ "Error, ingrese un número entre 1 y 7.",
                                 nota[i] >= 4 ~ "¡Felicitaciones!",
                                 nota[i] < 4 ~ "Reprobaste")
}

base_alumnos <- data.frame(Nota = nota, Mensaje = mensaje_alumno)
base_alumnos

# Manera alternativa: sin usar "alumno" como el dato "variable".
for(x in nota) {
  if(!is.numeric(x) | (x < 1) | (x > 7)) {
    print("Error, ingrese una nota correcta")
  } else if (x >= 4) {
    print("Felicitaciones")
  } else {
    print("Reprobaste")
  }
}

# Podemos ver alguno de los ejemplos anteriores
for(i in 1:dim(data)[1]){
  if(data$esperanza_de_vida[i] <= mean(data$esperanza_de_vida)) {
    print(paste0("Esperanza de vida bajo el promedio: ", data$pais[i], ", Año:", data$anio[i]))  
  }
}

# Veamos con los datos 
# Por ejemplo quisiera calcular la población promedio para cada país 
unique(data$pais)
tabla <- data.frame(pais=rep(NA_character_, length(unique(data$pais))), 
                    promedio_poblacion=rep(NA_real_, length(unique(data$pais))))

for(i in 1:length(unique(data$pais))) {
  tabla[i,1] <- as.character(unique(data$pais)[[i]])
  tabla[i,2] <- mean(data$poblacion[data$pais==unique(data$pais)[i]], na.rm = TRUE)
}

# Podemos comprobar con la función group_by
tabla2 <- data %>% 
  group_by(pais) %>% 
  summarise(media_poblacion=mean(poblacion, na.rm = TRUE))

# Otro ejemplo: quisieramos sacar muestras aleatorias 
# Vamos a trabajar con la data filtrada para un año
data_2007 <- data %>% filter(anio==2007)
# Veamos el promedio de esperanza de vida para todos los países
mean(data_2007$esperanza_de_vida)

# Nuestro objetivo es obtener 200 promedios de muestras aleatorias (n=50)
promedios <- numeric(200) 
promedios
for(i in 1:200) {
  muestra <- sample(x = data_2007$esperanza_de_vida,
                    size = 50,
                    replace = TRUE)
  promedios[i] <- mean(muestra)
}

promedios # Veamos los 200 promedios que obtuvo

mean(promedios) # ¿Cuál es el promedio de esos promedios?

# Grafiquemos 
ggplot(data = NULL, aes(x = 1:200, y = promedios)) +
  geom_point(color = "darkblue") + geom_line(color = "blue") +
  geom_hline(yintercept = mean(data_2007$esperanza_de_vida), color = "red", lwd = 1) + # Promedio real 
  geom_hline(yintercept = mean(promedios), color = "green", lwd = 1) # Promedio de un muestreo repetido


ggplot(data = NULL, aes(x = promedios)) +
  geom_histogram(bins = 100, fill="darkblue") + 
  geom_vline(xintercept =  mean(data_2007$esperanza_de_vida), color = "red", lwd = 1) +
  geom_vline(xintercept =  mean(promedios), color = "green", lwd = 1)


## 2.8 Comando while -----------------------------------------------------------

count <- 0
while(count < 5){
  count <- count+1
  print(count)
}

resultado <- 1
while(resultado < 50) {
  resultado = resultado*2 + 5
  print(resultado)
}

# Apliquemos lo visto en el caso anterior 

promedios <- numeric(200)
contador <- 1

while(contador < 201) {
  muestra <- sample(x = data_2007$esperanza_de_vida,
                    size = 50,
                    replace = TRUE)
  promedios[contador] <- mean(muestra)
  contador <- contador + 1
}

mean(promedios)


## 2.9 Comando repeat y break -----------------------------------------------------------

promedios <- numeric(200)
indice <- 1

repeat {
  if(indice > 200) break
  muestra <- sample(x = data_2007$esperanza_de_vida,
                    size = 50,
                    replace = TRUE)
  promedios[indice] <- mean(muestra)
  indice <- indice + 1
}

promedios 
mean(promedios)

########################################################################/
# Tema Funciones -------------
########################################################################/

# Programemos acciones sobre objetos
funcion_1 <- function(x) {
  x^2 + x
}

funcion_2 <- function(x, a, b, c) {
  a*x^2 + b*x + c
}

# Uso de las funciones programadas
funcion_1(x = 2)
funcion_1(x = 1:10)
funcion_2(x = 2, a = 1, b = 1, c = 0)
funcion_2(x=1:10, a=10, b=5, c=1)

# Argumentos:
formals(funcion_2)
formals(mean)

# Explique la siguiente función 
funcion_3 <-  function(x){
  a <-  mean(x, na.rm = TRUE)
  b <-  sd(x, na.rm = TRUE)
  x <-  (x-a)/b
  return(head(x))
}

funcion_3(x=data_2007$esperanza_de_vida)

# Veamos la siguiente función: ¿Qué estamos estimando?
funcion_4 <-  function(x, y){
  varx <-  var(x, na.rm = TRUE)
  covxy <-  cov(x, y)
  beta1 <- covxy/varx
  beta0 <- mean(y, na.rm = TRUE) - beta1*mean(x, na.rm = TRUE) 
  return(c(beta0, beta1))
}

funcion_4(x=data_2007$pib_per_capita, y=data_2007$esperanza_de_vida)

# Verificamos nuestro resultado
summary(lm(esperanza_de_vida ~ pib_per_capita, data=data_2007))

########################################################################/
# Tema 4. Generalización de funciones -------------
########################################################################/

# Podemos generar una función similar al anterior 
# Nuestro objetivo es obtener 200 promedios de muestras aleatorias (n=50)
promedios <- c()
promedios
for(i in 1:200) {
  muestra <- sample(x = data_2007$esperanza_de_vida,
                    size = 50,
                    replace = TRUE)
  promedios[i] <- mean(muestra)
}

promedios # Veamos los 200 promedios que obtuvo

mean(promedios) # ¿Cuál es el promedio de esos promedios?

# Grafiquemos 
g1 <- ggplot(data = NULL, aes(x = 1:200, y = promedios)) +
  geom_point(color = "darkblue") + geom_line(color = "blue") +
  geom_hline(yintercept = mean(data_2007$esperanza_de_vida), color = "red", lwd = 1) + # Promedio real 
  geom_hline(yintercept = mean(promedios), color = "green", lwd = 1) # Promedio de un muestreo repetido


g2 <- ggplot(data = NULL, aes(x = promedios)) +
  geom_histogram(bins = 100, fill="darkblue") + 
  geom_vline(xintercept =  mean(data_2007$esperanza_de_vida), color = "red", lwd = 1) +
  geom_vline(xintercept =  mean(promedios), color = "green", lwd = 1)

g1/g2

### ¡Generalicemos todo el proceso en una función!
muestreo <- function(v, m, n, replace = TRUE) {
  vector_promedios <- c()
  for(i in 1:m) {
    muestra <- sample(x = v, 
                      size = n,
                      replace = TRUE)
    vector_promedios[i] <- mean(muestra)
  }
  
  promedio <- mean(vector_promedios) # Cuál es el promedio de esos promedios
  
  g1 <- ggplot(data = NULL, aes(x = 1:m, y = vector_promedios)) +
    geom_point(color = "darkblue") + geom_line(color = "blue") +
    geom_hline(yintercept = mean(v), color = "red", lwd = 1) + # Promedio real 
    geom_hline(yintercept = promedio, color = "green", lwd = 1) # Promedio de un muestreo repetido
  
  
  g2 <- ggplot(data = NULL, aes(x = promedios)) +
    geom_histogram(bins = 100, fill="darkblue") + 
    geom_vline(xintercept =  mean(v), color = "red", lwd = 1) +
    geom_vline(xintercept =  promedio, color = "green", lwd = 1)
  
  grafico <- g1/g2
  
  #return(c(vector_promedios, promedios, grafico))
  print(paste0("Promedios de ", m, " muetras de tamaño ", n, ":"))
  print(vector_promedios)
  
  print(paste0("Gran media: ", promedio))
  print(paste0("Promedio 'verdadero': ", mean(v)))
  print(grafico)
}

# Ahora simplemente aplicamos nuestra función las veces que queramos 
muestreo(v=data_2007$esperanza_de_vida, m=200, n=50, replace = TRUE) 
muestreo(v=data_2007$esperanza_de_vida, m=1000, n=500, replace = TRUE) 
muestreo(v=data_2007$esperanza_de_vida, m=10000, n=5000, replace = TRUE) 

# ¿Cómo podemos ir mejorando nuestra función? 


########################################################################/
# Tema 5. Funciones previas -------------
########################################################################/

# Podemos escribir nuestra funciones previas y cargarlas a nuestra lugar de trabajo
# La función  source nos permite cargar códigos de R previos 
source("input/FuncionesPrevias.R")

# ¿Qué contiene mi archivo de funciones previas? 
file.edit("input/FuncionesPrevias.R")

# Apliquemos una de nuestras funciones previas 
Sampling2(v = data$esperanza_de_vida, N = 200, n = 50)

n_pruebas <- 50:150

estimaciones <- list()

for(i in 1:length(n_pruebas)) {
  estimaciones[[i]] <- Sampling2(v = data$esperanza_de_vida, 
                                 N = 200,
                                 n = n_pruebas[i])
}
estimaciones
estimaciones <- dplyr::bind_rows(estimaciones)
estimaciones


########################################################################/
# Desde este punto en adelante es lo que se conoce como programación funcional
########################################################################/

# Apliquemos una de nuestras funciones previas
Notas <- rbind(c(7, 3.5, 4.8, 6.8, 5.5),
                 c(6.8, 4, 5, 7, 6.2),
                 c(6, 6, 5, 7, 6),
                 c(5.5, 4.5, 7, 6.5, 6))
row.names(Notas) <- paste0("Alumno ", 1:4)
colnames(Notas) <- paste0("Control ", 1:5)

Notas
########################################################################/
# Tema 6. Funciones apply -------------
########################################################################/

## 6.1 Funcion apply ------------------------------------------------
apply(X = Notas, MARGIN = 1, FUN = mean)
apply(X = Notas, MARGIN = 2, FUN = mean)

# ¿Que pasaria si la nota final es ponderada?
apply(X = Notas, MARGIN = 1, FUN = function(x) {
  0.2*x[1] + 0.1*x[2] + 0.2*x[3] + 0.2*x[4] + 0.3*x[5]
})

# Apliquemos a nuestros datos
glimpse(data_2007)
apply(X = data_2007[data_2007$continente=="Américas", 4:6], MARGIN = 2, FUN = mean)
apply(X = data_2007[data_2007$continente=="Américas", 4:6], MARGIN = 1, FUN = mean)

## 6.2 Funcion tapply ---------------------------------------------------------

Notas2 <- rbind(c(7, 3.5, 4.8, 6.8, 5.5),
               c(6.8, 4, 5, 7, 6.2),
               c(6, 6, 5, 7, 6),
               c(5.5, 4.5, 7, 6.5, 6))
colnames(Notas2) <- paste0("Control ", 1:5)
Notas2 <- tibble::as_tibble(Notas2) 
Notas2 <- Notas2 %>% 
  dplyr::mutate(Alumno = paste0("Alumno ", 1:4),
                Sexo = c(rep("F", 2), rep("M", 2)))

Notas2
# Podemos agrupar por un INDEX
tapply(X = Notas2$`Control 2`, INDEX = Notas2$Sexo, FUN = mean)

#Apliquemos a nuestros datos 
tapply(X = data_2007$esperanza_de_vida, INDEX = data_2007$continente, FUN = mean)
tapply(X = data_2007$esperanza_de_vida, INDEX = data_2007$pais, FUN = mean)

## 6.3 Funcion lapply ---------------------------------------------------------

# Para trabajar con objetos que son listas y retorna una lista
# Apliquemos un proceso anterior
muestras <- list()
for(i in 1:length(n_pruebas)) {
  muestras[[i]] <- Sampling(v = data_2007$esperanza_de_vida, 
                            N = 200, 
                            n = n_pruebas[i])
}

class(muestras) # Es una lista
muestras

# Ejemplo: el argumento es una lista
lapply(X = muestras, FUN = mean)
unlist(lapply(X = muestras, FUN = mean, na.rm = TRUE))

# Ejemplo: el argumento es un vector (podemos traer de vuelta un objeto más manipulable):
dplyr::bind_rows(lapply(X = n_pruebas, 
                        Sampling2, 
                        v = data$esperanza_de_vida,
                        N = 200)
)

## 6.4 Funcion sapply ---------------------------------------------------------

# Para trabajar con objetos que son listas y retorna un vector (más útil)
sapply(X = Muestras, FUN = mean)

### Ejemplo extra: apply vs lapply

apply(X = Notas, MARGIN = 1, mean)
lapply(X = Notas, FUN = mean)
lapply(X = Notas, FUN = function(x) sum(x)/length(x))

## 6.5 Apply vs for ---------------------------------------------------------

# Usemos un apply
Sys.time() # Fecha y hora
inicio <- Sys.time()
apply(X=data[, 4:6], MARGIN=2, FUN = mean, na.rm=TRUE)
fin <- Sys.time()

fin-inicio # ¿Cuánto tiempo se demoro en ejecutar?

# Usemos un for 
inicio <- Sys.time()
variables <- colnames(data[,4:6])
for(i in variables){
  print(mean(data[[i]], na.rm=TRUE))
}
fin <- Sys.time()

fin-inicio # ¿Cuánto tiempo se demoro en ejecutar?

########################################################################/
# Tema 7. Purrr: maps -------------
########################################################################/

## 7.1 Funcion map -----------------------------

# Apliquemos un map 
muestras
purrr::map(muestras, mean)

# Map dentro de otro map:
purrr::map(purrr::map(n_pruebas,
                      Sampling,
                      v = data$esperanza_de_vida,
                      N = 200),
           mean)

muestras_version2 <- purrr::map(n_pruebas,
                                Sampling,
                                v = data$esperanza_de_vida,
                                N = 200)
muestras_version2

# Funcion map_dbl: retorna un double
purrr::map_dbl(muestras, mean)

# Funcion map_chr: retorna un character
purrr::map_chr(muestras, mean)

# Funcion map_dfr: genera un dataframe uniendo la información por filas
muestras2 <- map_dfr(n_pruebas,
                     Sampling2,
                     v = data$esperanza_de_vida,
                     N = 200)
muestras2

# Funcion map_dfc
muestras3 <- map_dfc(n_pruebas,
                     Sampling2,
                     v = data$esperanza_de_vida,
                     N = 200)
muestras3

# ¿Qué formato es mejor? ¿Muestra 2 o 3?

## 7.2 Map vs apply vs for ----

# Usemos un apply
Sys.time() # Fecha y hora
inicio <- Sys.time()
apply(X=data[, 4:6], MARGIN=2, FUN = mean, na.rm=TRUE)
fin <- Sys.time()

t1 <- fin-inicio # ¿Cuánto tiempo se demoro en ejecutar?

# Usemos un for 
inicio <- Sys.time()
variables <- colnames(data[,4:6])
for(i in variables){
  print(mean(data[[i]], na.rm=TRUE))
}
fin <- Sys.time()

t2 <- fin-inicio # ¿Cuánto tiempo se demoro en ejecutar?

inicio <- Sys.time()
map(data[, 4:6], mean)
fin <- Sys.time()

t3 <- fin-inicio # ¿Cuánto tiempo se demoro en ejecutar?

t1 # apply
t2 # for
t3 # map


## 7.3 Funcion map2 ----
# Muy similares a las funcione map, pero con la diferencia de que permiten más de dos argumentos
funcion_ejemplo <- function(x, y){
  log(x) + y^2
}

map2(1:3, 11:13, funcion_ejemplo)
map2_dbl(1:3, 11:13, funcion_ejemplo)

ejemplo_complejo <- map2(1:101, # N = 1:101
                         n_pruebas, 
                         Sampling, 
                         v = data$esperanza_de_vida)

ejemplo_complejo

## 7.4 Funcion nest ----

# Estas funciones nos permiten trabajar con datas agrupadas
glimpse(data) # Recordemos nuestros datos
# Vamos a generar un directorio para guardar nuestros resultados
dir.create("output/Bases_paises") # Genera una carpeta en el directorio

data_nest <- data %>% 
  group_nest(pais) %>% 
  mutate(file = paste0("output/Bases_paises/Base_", pais, ".csv"))

data_nest
View(data_nest)

# Vamos a guardar un archivo excel para cada país
data_nest %>% 
  select(file, x=data) %>% 
  pwalk(readr::write_csv)

########################################################################/
# Ejercicio propuesto -------------
########################################################################/

# Generar una función que permite estimar para TODAS las columnas de una base datos
# Si la variable es numérica, estimar e imprimir media, desviación estándar, mediana, mínimo y máximo.
# Si la variable es categórica, estimare imprimir tabla de frecuencias y porcentajes. 

