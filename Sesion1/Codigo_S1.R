########################################################################/
# Campamento de invierno ------
# Escuela de Gobierno UC - 26/07/2023
########################################################################/
# Introducción a R y RStudio
# José Daniel Conejeros - jdconejeros@uc.cl
########################################################################/

# Introducción a R como lenguaje de programación estadístico y RStudio como interfaz gráfica. 

# Para instala R en tu computador: https://cran.r-project.org/
# Para instala RStudio en tu computador: https://posit.co/download/rstudio-desktop/

########################################################################/
## Interactuando con R y RStudio ----
########################################################################/

# Una vez instalado el programa podemos comenzar a trabajar el código.

# : Comentarios que no se ejecutan como comandos
# + : Sigue el comando en la próxima linea
# ; : Para escribir más de una función en la misma línea
# Recomendación general: todo lo que tenga que ver con procesamiento, análisis de bases de datos, entre otros, 
# deben ser sin tilde ni ñ. Te evitarás bastantes problemas. Además se recomienda en trabajar con minúsculas.

# Directorio 
getwd() #Consultar directorio

setwd("/Users/josedanielconejeros/Dropbox") #Fijar directorio

# Primera precaución: cuidado con los códigos
#Windows (\)
setwd("\Users\josedanielconejeros\Dropbox")

#Mac (/)
setwd("/Users/josedanielconejeros/Dropbox")

# Opción mejor: Generar proyectos en R

########################################################################/
## Objetos en R -----
########################################################################/

### 1.1 Vectores y variables ----

a1 <- c(1) #Vector de un elemento =
a2 = c(1)
c(1, 2 , 3 , 4) #Crear un vector de números
a3 <- c(1, "hola" , "chao" , 4) #Crear un vector números y carácteres

#Secuencia de valores
1:4
4:1
?seq
seq(1, 4) 
seq(4, 1)
seq(-1, 2)
seq(from=2, to=8, by=2)
seq(0, 1, by=0.1)
seq(0, 1, length=10)
rep(0:1, 10)
?length

a <- 1
A <- 1

#Objetos (importancia de los nombres)
numeros_palabras <- c(1, "hola" , "chao" , 4)
numeros_palabras

secuencia <- seq(0, 1, length=11)
secuencia
secuencia*numeros_palabras

#Operaciones aritméticas con vectores
c(1, 2, 3, 4)/2
(1:4)/2
(1:4)*(4:1)
c(1,2,3,4)*c(4,3,2,1)

c(1, 2 , 3 , 4) + c(4, 3)
c(1, 2 , 3 , 4) + c(4, 3, 2) #Produce warning cuando no son múltiplos

#Operaciones aritméticas con variables 
secuencia <- seq(0, 1, length=11)
secuencia
secuencia <- secuencia*secuencia
secuencia
secuencia^2
promedio <- sum(secuencia)/11
promedio

promedio2 <- mean(secuencia) 
promedio2


### 1.2 Strings y Factores  ----

#Almacenamiento de variables categóricas 
#¿Cuál es la diferencia con una variable continua?
#Dummy
sexo <- c("Femenino", "Masculino", "Masculino", "Femenino")
sexo 
sexo[1:2] 
sexo <- c(1,0,0,1)
#Nominales
animales <- c("Elefante", "Jirafa", "Mono", "Caballo")
animales
animales[1]

#Ordinal 
acuerdo <- c("Muy en Desacuerdo", "Desacuerdo", "Ni en desacuerdo", "Ni en desacuerdo" , 
             "Deacuerdo", "Muy deacuerdo")
acuerdo[5]

#¿Cuál es el problema con estas variables?
class(sexo)

#Debemos crear factores numéricos con etiquetas
sexo <- factor(c(0,1,1,0))
sexo
#Generamos etiquetas
sexo <- factor(sexo, labels = c("Mujer", "Hombre"))
sexo

########################################################################/
## Tema 2: Matrices, listas y arreglos  ----
########################################################################/

### 2.1 Matrices  ----

#Ejemplo de matriz
matrix(1:9,3,3) #Matriz de 3filasx3columnas con valores del 1 al 9

#Matrices como objetos
x <- matrix(1:9,3,3)
x
dim(x)

matrix(1:8,2,4,byrow = FALSE) 
matrix(1:8,2,4,byrow = TRUE) 

# Podemos definir objetos como matrices
y <- matrix(1:8,2,4,byrow = FALSE) #Genera una matriz con 2 filas y 4 columnas
y
dim(y)

z <- matrix(1:8,2,4,byrow = TRUE) #Genera la matriz completándola por filas
z
dim(z)
?matrix

#Operaciones matemáticas con matrices
y
z
# Resta
diff <- y - z
diff
# Suma
sum <- y + z
sum
# Transponer
t(y)
# Multiplicar/dividir por un escalar
y*2
2*y
y/2
# Multiplicación elemento a elemento
y*z

# Multiplicación matricial 
# Número de columnas de la primera matriz debe ser igual al número de filas de la segunda.
y %*% z

# Determinante de una matriz
det(y) # ¿Cuál es el error que aparece?
y # Matriz original
y_cuadrada <- y[1:2,1:2] # Matriz cuadrada
det(y_cuadrada)

# Inversa de una matriz
y_invertida <- solve(y_cuadrada)
y_invertida
# Comprobamos
y_cuadrada %*% y_invertida

# Rango de una matriz
qr(y_cuadrada)
qr(y_cuadrada)$rank

# Diagonal de una matriz
y_cuadrada
diag(y_cuadrada)

# Matriz identidad
diag(10)

#Podemos construir una matriz con datos
edad <- c(23, 45, 67, 89)
sexo <- c(1, 0, 1, 0)
peso <- c(80, 60, 70, 50)
altura <- c(180, 160, 200, 140)
promedio <- mean(altura)

matriz_a <- cbind(edad, sexo) # Columnas
matriz_a
matriz_b <- cbind(peso, altura)
matriz_b

matriz_c <- rbind(peso, altura) # Filas
matriz_c

#Combinar matrices
matriz <- cbind(matriz_a, matriz_b)
matriz
matriz[1,2] #Podemos ver el elemento 1 de la columna 2

### 2.2 Marcos de datos ----
# Podemos transformar facilmente nuestra matriz en un marco de datos
class(matriz)

data <- as.data.frame(matriz)

class(data)

data$edad
data[1] # Columna
data[1,1] # Fila,columna

### 2.3 Listas ----

#Creamos una lista
#matriz, valor, vector numérico, vetor de caracteres
objeto <- list(matriz, promedio, sexo, animales) 
objeto

#Vemos los elementos cuatro de la lista: 
objeto[[1]]*2
objeto[[2]]

########################################################################/
## Tema 3: Operadores  ----
########################################################################/

# Lógico:
# & Y 
# | O
# ! Negación
# ~ Negación

# Aritmético:
# + Suma
# - Resta
# * Multiplicación 
# / División
# ^ Potencia

# Relacional:
# > Mayor que 
# < Menor que
# >= Mayor o igual que 
# <= Menor o igual que 
# == Igual
# != No igual 
# ~= No igual

# Otros:
# NA Valor perdido

a <- 3
b <- 2.999
c <- 1

c != a
a == b  # ¿Qué pasa si solo uso =?

a > b 
b < c

a > b & b < c
a > b | b < c

########################################################################/
## Tema 4: Explorar funciones y argumentos   ----
########################################################################/

### 4.1 Funciones ----

a <- c(1)
a <- seq(1:100)
b <- c("uno", "dos", "tres")
log("a")
log(b)
log(a) #Por DEFAULT es logaritmo natural, en base a euler 2,718.
log(a, base=10)
log(2.718) #Cercano a 1 porque euler elevado a 1 = euler
log(100, base=10)
log10(a)

### 4.2 Argumentos ----

# Podemos explorar los argumentos de una función 
help("log") #Para saber argumentos (elementos de la función)
?log
args("log")

?sum
args("sum")

a <- c(1,2,NA,4)
sum(a)
sum(a, na.rm=TRUE)

### 4.3  Podemos crear nuestras propias funciones  ----

#Veamos una función simple
fun1 <- function(x){
  result <- round(sqrt(x /10), digits = 1) # paso 1: subpaso a, b, c
  return(result) # paso 2
}

fun2 <- function(x){mean(x)}

#Usamos las funciones
fun1(100)
fun2(100)

fun1(c(0,10,100,1000))

########################################################################/
## Tema 5: Lectura de datos   ----
########################################################################/

# Las tablas de datos en general están en diversos formatos, por lo que
# debemos aplicar funciones para que puedan ser trabajadas en este espacio de trabajo
# Podemos realizar esto de forma manual, ¿Cuál es la desventaja de esto?

#### 5.1 Base en formato .txt  ----
# TXT: Documento de texto plano
data_txt <- read.table(file="input/simce2m2016_extracto.txt", sep="\t")
#data_txt <- read.table(file="input\simce2m2016_extracto.txt", sep="\t")

#### 5.2 Base en formato .csv  ----
# CSV: Valores separados por comas
data_csv <- read.csv("input/simce2m2016_extracto.csv")
#data_csv <- read.csv("input\simce2m2016_extracto.csv")

#### 5.3 Base en formato .xlsx  ----
install.packages("readxl")
library(readxl)

data_xlsx <- read_xlsx(path="input/simce2m2016_extracto.xlsx")
#data_xlsx <- read_xlsx(path="input\simce2m2016_extracto.xlsx")

#### 5.4 Base en formato .sav  ----
install.packages("haven")
library(haven)
data_sav <- read_sav(file="input/simce2m2016_extracto.sav")
#data_sav <- read_sav(file="input\simce2m2016_extracto.sav")

#### 5.5 Base en formato .sas  ----
data_sas <- read_sas(data_file="input/simce2m2016_extracto.sas8bdat")
#data_sas <- read_sas(data_file="input\simce2m2016_extracto.sas8bdat")

#### 5.6 Base en formato .dta  ----
data_dta <- read_dta(file="input/simce2m2016_extracto.dta")
#data_dta <- read_dta(file="input\simce2m2016_extracto.dta")

#### 5.7 Base en formato .RData ----
load(file="input/simce2m2016_extracto.RData")
#load(file="input\simce2m2016_extracto.RData")

########################################################################/
## Tema 6: Exploración de datos   ----
########################################################################/

ls() # Podemos ver los objetos del enviroment
rm(list=ls()) # Borramos todos los objetos

# Abrimos una tabla de datos para explorar
data <- read.csv("input/simce2m2016_extracto.csv")

### 6.1 Exploramos el objeto ---
# Dada la heterogeneidad de nuestro objeto lo vamos a transformar en un marco de datos
typeof(data)
data <- as.data.frame(data)
class(data) # clase

# Realizamos una vista previa de los datos
View(data)

# Podemos ver los componentes del objeto 
dim(data)   # Observaciones y variables
colnames(data) # Nombre de nuestras variables 
str(data)   # Visor de nuestras variables
head(data) # Primeras 5 observaciones
tail(data) # Últimas 5 observaciones

# Lógica fila, columna
data[1]
data[1,]
data[,1]
data[1,1]
data[1:6,1:6] # La separación para un objeto bidimensional es siempre fila, columna
data[,-6] # ¿Qué nos imprime esto?
data[,c(1, 3)] # Veamos la primera y última fila de una BBDD [fila, columna]
solo_mate <- data[,c("idalumno","ptje_mate2m_alu")] # ¿Cuál es la diferencia?

### 6.2 Filtros de información ----
data2 <- data[data$cod_depe2 == "Municipal" & data$ptje_mate2m_alu<100, ] # Usemos condiciones 

data3 <- data[data$ptje_mate2m_alu %in% c(100:120),] # Comparar con varios valores
#data3 <- data[data$gen_alu %in% c("Femenino", "No binario"),]

data3b <- data[data$ptje_mate2m_alu >= 100 & data$ptje_mate2m_alu <= 120,] 

data4 <- data[data$ptje_mate2m_alu==100 | data$ptje_mate2m_alu==120, ] # Equivalente

### 6.3 Missing values ----

data$variable_missing <- NA
data$cpad_p08 <- ifelse(data$cpad_p08==8, NA, data$cpad_p08)

library(naniar) # Cargamos el paquete en nuestra sesión de trabajo 
library(ggplot2)

# % de missing según variable
vis_miss(data, warn_large_data = FALSE)
gg_miss_var(data) + scale_y_continuous(limits = c(0,60)) # Gráfico con el nivel de missing

n_var_miss(data) # Número de variables con casos pérdidos.

n_case_miss(data) # Número de casos con casos pérdidos.

# Upset graph
gg_miss_upset(data)

library(dplyr)
tabla_miss <- data %>%
  miss_var_summary() 

tabla_miss # vemos la tabla 

# Remover los missing
data_sin_miss <- na.omit(data)
dim(data_sin_miss)
head(data_sin_miss, n=20)

### 6.4 Estadísticos descriptivos ----

# Variables numéricas 
summary(data$media_simce)

mean(data$media_simce)
var(data$media_simce, na.rm=TRUE)
sd(data$media_simce, na.rm=TRUE)
range(data$media_simce, na.rm=TRUE)
min(data$media_simce, na.rm=TRUE)
max(data$media_simce, na.rm=TRUE)
quantile(data$media_simce, probs=0.25, na.rm=TRUE)
quantile(data$media_simce, probs=0.50, na.rm=TRUE)
quantile(data$media_simce, probs=0.75, na.rm=TRUE)

# Variables discretas: 
# - Frecuencia 
# - Frecuencia relativa
# - Porcentaje

table(data$gen_alu)
prop.table(table(data$gen_alu))
round(prop.table(table(data$gen_alu)),3)*100

########################################################################/
## Tema 7: Exportación de datos   ----
########################################################################/

#### 7.1 Base en formato .txt  ----
write.table(data, file="output/simce_modificada.txt", sep="\t")

#### 7.2 Base en formato .csv  ----
write.csv(data, file="output/simce_modificada.csv", row.names = FALSE)

#### 7.3 Base en formato .xlsx  ----
install.packages("writexl")
library(writexl)
write.xlsx(data, file="output/simce_modificada.xlsx")

#### 7.4 Base en formato .sav  ----
library(haven)
write_sav(data, "output/simce_modificada.sav")

#### 7.5 Base en formato .sas  ----
write_sas(data, "output/simce_modificada.sas8bdat")

#### 7.6 Base en formato .dta  ----
write_dta(data, "output/simce_modificada.dta")

#### 7.7 Base en formato .RData ----
save(data, file="output/simce_modificada.RData")

########################################################################/
## Ejercicios Propuestos   ----
########################################################################/

# 1. Revise la siguiente función y responda las preguntas.
fun3 <- function(x){ifelse(-10 <= x  & x < -5, x*2-1,
                           ifelse(-5<=x & x < 0, (3*x-2)/(x+1), 
                                  ifelse(1<=x & x < 5, exp(x+1), "Valor no está en el dominio"))) 
}

# a. Evalue la función en los siguientes valores: -11, -7, -2, 0, 2 y 6. Interprete sus resultados.

# b. ¿Qué pasa si aplico la función en el valor 6,5 y si ahora aplica "6,5" (incluyendo comillas). Explique la diferencia.

# c. ¿Qué ocurre con la función en el valor 6? Solucione el problema. 

# d. Genere un objeto con una secuencia entre 1 a 5 y evalue la función corregida en c). Muestre sus valores.


# 2. Explore la BBDD y trate de describir el siguiente código
install.packages("dplyr")
library(dplyr)
sum <- function(x, data){
  data %>%
    dplyr::summarize(count=length(na.omit({{ x }})),
                     media=mean({{ x }}, na.rm=TRUE),
                     sd=sd({{ x }}, na.rm=TRUE),
                     min=min({{ x }}, na.rm=TRUE),
                     q25=quantile({{ x }}, na.rm=TRUE, probs=0.25),
                     q50=quantile({{ x }}, na.rm=TRUE, probs=0.50),
                     q75=quantile({{ x }}, na.rm=TRUE, probs=0.75),
                     max=max({{ x }}, na.rm=TRUE))
}

sum(data=data, x=ptje_mate2m_alu)

# a. ¿Cuál es el input y el output de esta función? 

# b. ¿Para qué necesitaríamos la librería dplyr para aplicar esta función?

# c. ¿Qué hace el operador %>% (pipeline)?


