########################################################################/
# Campamento de invierno ------
# Escuela de Gobierno UC - 28/07/2023
########################################################################/
# Manipulación de tablas y objetos
# José Daniel Conejeros - jdconejeros@uc.cl
########################################################################/

# En esta sesión nos enfocaremos en lo que es manipulación de tablas 

########################################################################/
## Tema 1: Estructura de una tabla de datos  ----
########################################################################/
ls() # Podemos ver los objetos del enviroment
rm(list=ls()) # Borramos todos los objetos

# Abrimos una tabla de datos para explorar
data <- read.csv("input/simce2m2016_extracto.csv")

# Exploramos el objeto
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

# Lógica fila, columna
data[1]
data[1,]
data[,1]
data[1,1]
data[1:6,1:6] # La separación para un objeto bidimensional es siempre fila, columna
data[,-6] # ¿Qué nos imprime esto?
data[,c(1, 3)] # Veamos la primera y última fila de una BBDD [fila, columna]
solo_mate <- data[,c("idalumno","ptje_mate2m_alu")] # ¿Cuál es la diferencia?

########################################################################/
## Tema 2: Explorar una base de datos  ----
########################################################################/

# Hay diversas funciones para explorar una tabla de datos, aquí veremos algunas-

### 2.1 Exploración inicial ----

# Cabeza de los datos
head(data)  # Primeras 6 observaciones
head(data, n=10)  # Primeras 10 observaciones
head(data[1,2])      # por nombres
head(data[[2]])    # por ubicación 

# Cola de los datos
tail(data, n=10)  # Últimas 10 observaciones
tail(data[,4], n=10)
tail(data[4,])
tail(data)
tail(15,15)
tail(1:20, 15)

# Podemos llamar a un vector dentro del marco de datos
data$idalumno
print(data$idalumno)

### 2.2 Variables y levels ----

# Consulta
install.packages("sjPlot")
library(sjPlot)
sjPlot::view_df(data)

# Etiqueta de variable
install.packages("Hmisc")
library(Hmisc)
label(data$gen_alu)
label(data$gen_alu) <- "Género del estudiante"

# Etiqueta de valores
unique(data$gen_alu)
levels(data$gen_alu)
table(data$gen_alu)

data$gen_alu <- as.factor(data$gen_alu)
levels(data$gen_alu)

###  2.3 Filtros de información ----
data2 <- data[data$cod_depe2 == "Municipal" & data$ptje_mate2m_alu<100, ] # Usemos condiciones 

data3 <- data[data$ptje_mate2m_alu %in% c(100:120),] # Comparar con varios valores
#data3 <- data[data$gen_alu %in% c("Femenino", "No binario"),]

data3b <- data[data$ptje_mate2m_alu >= 100 & data$ptje_mate2m_alu <= 120,] 

data4 <- data[data$ptje_mate2m_alu==100 | data$ptje_mate2m_alu==120, ] # Equivalente

########################################################################/
## Tema 3: Manipulación de una tabla  ----
########################################################################/

# Puedes revisar el detalle de cada librería en: https://www.tidyverse.org/

### 3.1 Inicial ----

library(tidyverse) # Ya la instalamos previamente
library(dplyr) # Puedo cargar una librería en particular

# readr: lectura de bases de datos. 
# dplyr: procesamiento de variables y datos. 
# tidyr: trabajo con bases de datos ordenadas.
# ggplot2: visualización de datos. 
# purrr: Herramienta para trabajar funciones, vectores e iteraciones. 
# tibble: gestiñon de marco de datos. 
# stringr: trabajo con variables de tipo caracter (textos).
# forcats: trabajo con variables de tipo factor (variables cualitativas). 

# Uso del operador pipe %>% o |> 
# Exploremos los datos 
data %>% glimpse() # Vista previa 
data %>% head()    # Primera 6 observaciones
data %>% colnames() # Nombres columnas/variables
data %>%  select(idalumno, gen_alu) %>% head(n=20) 
data %>% head(n=3)

# Ajustemos nombre de las variables en minúsculas
library(stringr) # Tiene funciones para operar con vectores de tipo character.
data %>% colnames()
colnames(data) <- str_to_lower(colnames(data))

data <- data %>% 
  mutate(genero = str_to_lower(gen_alu))

# Recomendación: siempre estandarizar el lenguaje. Es más fácil de procesar y homologar.

### 3.2 Select -------------
# Seleccionamos columnas/variables
data1 <- data %>% select(1:3)
data2 <- data %>% select("idalumno", "ptje_lect2m_alu")
data3 <- data %>% select("idalumno", "cpad_p08":"cod_depe2")

data1 %>% colnames()
data2 %>% colnames()
data3 %>% colnames()

### 3.3 Filter -------------
# Filtramos vectores a partir de una condición 
table(data1$gen_alu) # 1: Masculino // 2: Femenino
data_f <- data1 %>% filter(gen_alu=="Femenino") 
data_m <- data1 %>% filter(gen_alu=="Masculino") 

# Generamos dos condicionnes:
data_ptje <- data %>% filter(ptje_mate2m_alu >=300 & ptje_lect2m_alu==200 )
data_ptje2 <- data %>% filter(ptje_mate2m_alu >=300 | ptje_lect2m_alu==200)
data_ptje3 <- data %>% filter(ptje_mate2m_alu >=300)

nrow(data_ptje) # N de la muestra
nrow(data_ptje2) # N de la muestra
nrow(data_ptje3) # N de la muestra

# Diferencias entre objetos
setdiff(data_ptje2, data_ptje3)

### 3.4 Arrange -------------
# Ordenamos la BBDD a partir de uno o más vectores
head(data2, n=15)

data_arrange <- data2 %>% arrange(ptje_lect2m_alu)  # Por defecto el orden es creciente
head(data_arrange, n=15)

data_arrange2 <- data2 %>% arrange(desc(ptje_lect2m_alu)) # Decreciente
head(data_arrange2, n=15)

### 3.5 Rename -------------
# Renombramos vectores
data2 <- data2 %>% rename(id=idalumno)
data3 <- data3 %>% rename(folio=idalumno)

colnames(data2)
colnames(data3)

### 3.6 Slice -------------
# Seleccionamos filas 
data2 %>% filter(ptje_lect2m_alu>350) %>% slice(1:10)

### 3.7 El uso del pipeline para concatenar operaciones -------------
data_adjust <- data %>% 
  select(1:3) %>% 
  filter(ptje_mate2m_alu>=30) %>% 
  arrange(ptje_mate2m_alu) %>% 
  rename(id=idalumno) %>% 
  slice(1:10)

### 3.8 Generar variables nuevas -----

data <- data %>% 
  mutate(media_simce=rowMeans(.[, c("ptje_mate2m_alu", "ptje_lect2m_alu")], na.rm=TRUE))

########################################################################/
## Tema 4: Manipulación de tablas  ----
########################################################################/

# Quedémonos solo con tres tablas de interés
rm(list=ls()[! ls() %in% c("data1", "data2", "data3")])

### 4.1 Append BBDD  -----------------------------------
# Append llamamos a la unión por filas (sandwich)

# Si usamos Dplyr
data_f <- data1 %>% filter(gen_alu=="Femenino")
data_m <- data1 %>% filter(gen_alu=="Masculino")

glimpse(data_m)
glimpse(data_f)

# Trabajo de conjuntos 
union(data_m$idalumno, data_f$idalumno)
intersect(data_m$idalumno, data_f$idalumno)
setdiff(data_m$idalumno, data_f$idalumno)

data_append <- data_m %>% add_row(data_f)
glimpse(data_append)
head(data_append)
tail(data_append)

# Si usamos R Base:
data_append2 <- rbind(data_m, data_f)

# ¿Qué pasa cuando tenemos objetos con distintos vectores?
# Veamos un ejemplo 

data_m <- data_m %>% 
  mutate(variable_ficticia=rnorm(n=nrow(data_m), mean = 0, sd=1))

summary(data_m$variable_ficticia)

glimpse(data_m)
glimpse(data_f)

# Si usamos R Base:
data_append <- rbind(data_m, data_f) # Error
data_append <- data_m %>% add_row(data_f) # Coerción implícita
data_append <- data_m %>% bind_rows(data_f) # Coerción implícita

### 4.2 Join/Antijoin  -----------------------------------
# En este caso queremos unir por columnas, por lo que necesitamos una llave que conecte ambas tablas. 
# Esto es lo que se conoce coloquialmente como cruzar tablas de datos.
# Partamos con un ejemplo sencillo
# Definamos dos matrices x e y. 
x <- data.frame(idx = 1:5, 
                letras = letters[1:5])
y <- data.frame(idy = c(2:6,7), 
                num = c(12:16,3))

x
y

# Apliquemos tradicionalmente un cbind() para unir dos matrices por columnas
cbind(x, y) # ¡Error! # El cbind opera solo en el caso que tengamos el mismo número de filas. 
# Ajustesmoe el problema anterior 
cbind(x, y[-6,]) # No hay error, pero si un problema conceptual. ¿Cuál es?

# Trae los resultados de las tablas que cumplen con la condición de comparación entre columnas.
x %>% inner_join(y, by=c("idx"="idy")) #Utilizan una lleva para realizar el match. Solo los match.

# Trae todos los resultados de las tablas que cumplen con la condición de comparación entre columnas
# y, adicionalmente, trae todos los datos de la tabla de la izquierda.
x %>% left_join(y, by=c("idx"="idy"))

# Trae todos los resultados de las tablas que cumplen con la condición de comparación entre columnas
# y, adicionalmente, trae todos los datos de la tabla de la derecha.
x %>% right_join(y, by=c("idx"="idy"))

# Trae los resultados de las tablas que cumplen con la condición de comparación entre columnas, 
# además de los resultados de las o registros de las tablas de la derecha y la izquierda.
x %>% full_join(y, by=c("idx"="idy"))

# Trae los elementos que no tiene información para la base de destino.
x %>% anti_join(y, by=c("idx"="idy"))

# Veamos un ejemplo con nuestros datos
glimpse(data1) # 3 Columnas
glimpse(data2) # 2 Columnas
glimpse(data3) # 3 Columnas

# Vamos a unir estos tres marcos datos en uno solo para reconstruir la data original
full_data <- data1 %>% 
  left_join(data2, by=c("idalumno"="id")) %>% 
  left_join(data3, by=c("idalumno"="folio")) 

# ¿Por qué full data tiene una columna más que la data original?
setdiff(colnames(full_data), colnames(data1))

glimpse(full_data) # Precaución con los join

########################################################################/
## Tema 5: Pivoteo de tablas y otros ajustes  ----
########################################################################/

# Ahora vamos a generar una variable ficticia que me indique un tiempo
rm(list=ls()[! ls() %in% c("full_data")])

# Aplicamos mutate para generar variables nuevas
long_data <- full_data %>% 
  mutate(agno=ifelse(idalumno<=60000, 2018, 
                     ifelse(idalumno>60000 & idalumno<=180000, 2019, 
                            ifelse(idalumno>180000, 2020, NA_real_))
  ))

long_data <- long_data %>% 
  mutate(idalumno=ifelse(agno==2018, 1:nrow(long_data[long_data$agno==2018,]), 
                         ifelse(agno==2019, 1:nrow(long_data[long_data$agno==2019,]),
                                ifelse(agno==2020, 1:nrow(long_data[long_data$agno==2020,]), NA_real_)))) %>% 
  group_by(idalumno) %>% 
  filter(n() == 3) %>% 
  ungroup() %>% 
  arrange(idalumno)

# Vemos nuestro resultado
glimpse(long_data) # Base de dato en formato largo 
table(long_data$agno, useNA = "ifany")

### 5.1 De largo (long) a ancho (wide) ----
# Formato ancho la información va hacia las columnas
wide_data <- long_data %>% 
  tidyr::pivot_wider(names_from ="agno", 
                     values_from = c("gen_alu":"cod_depe2"),
                     names_glue = "{agno}_{.value}")


### 5.2 De ancho (wide) a largo (long) ----
# Formato largo la información va hacia las filas
data_long <- wide_data %>% 
  tidyr::pivot_longer(cols = !idalumno,
                      names_pattern = "(.+)", 
                      names_to = c(".value"))


# En general es mejor trabajar con los datos en formato long, pero en algunas 
# ocasiones (con datos longitudinales), el formato wide puede ser más óptimo.

## 5.3 Uniones y Separaciones ----
# Podemos unir dos vectores
data_unida <- long_data %>% 
  tidyr::unite(agno, gen_alu, 
               col="agno_sexo", 
               sep = "+")

# Podemos separar el vector unido en dos vectores distintos
data_separada <- data_unida %>% 
  tidyr::separate(agno_sexo, 
                  sep="[+]",
                  into=c("year", "gender"))

# Podemos separar el vector unido en dos filas
data_separada_filas <- data_unida %>% 
  tidyr::separate_rows(agno_sexo, 
                       sep="[+]")

########################################################################/
## Ejercicios Propuestos   ----
########################################################################/

# Busque una tabla de datos que sea de su interés y realice un análisis exploratorio.