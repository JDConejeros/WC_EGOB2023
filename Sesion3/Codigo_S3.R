########################################################################/
# Campamento de invierno ------
# Escuela de Gobierno UC - 31/07/2023
########################################################################/
# Manipulación de tablas y objetos
# José Daniel Conejeros - jdconejeros@uc.cl
########################################################################/

# En esta sesión nos enfocaremos en lo que es manipulación de tablas 

library(readr) # Importar
library(janitor) # clean_names y make_clean_names 
library(rio) # Importar
library(datos) # Datos en español
library(dplyr) # Manipulación
library(tidyr) # Manipulación y limpieza
library(lubridate) # Fechas
library(forcats) # Variables factores
library(stringr) # Variables tipo texto

########################################################################/
## Tema 1: Estructura de una tabla de datos  ----
########################################################################/
ls() # Podemos ver los objetos del enviroment
rm(list=ls()) # Borramos todos los objetos

# Abrimos una tabla de datos para explorar
data <- rio::import("input/simce2m2016_extracto.csv")

########################################################################/
## Tema 2: Manipulación de variables ----
########################################################################/

### 2.1 Generar variables nuevas -----
# Muta nos permite calcular nuevas variables 

data <- data %>% 
  mutate(media_simce=rowMeans(.[, c("ptje_mate2m_alu", "ptje_lect2m_alu")], na.rm=TRUE))

## 2.2 Generemos un factor (discreta o categórica)  ---------------------------
#install.packages("labelled")
library(labelled) # Manejador de etiquetas
var_label(data$gen_alu)
set_variable_labels(data$gen_alu)

data <- data %>% mutate(genero=factor(gen_alu, 
                                      levels=c("Masculino", "Femenino"), 
                                      labels=c("Hombre", "Mujer")))

var_label(data$genero) <- "Género del estudiante"
var_label(data$genero)
set_variable_labels(data$genero)

data %>% select(gen_alu, genero) %>% head()
table(data$gen_alu, data$genero, useNA = "ifany")

## 2.3 Uso de condicionales  ---------------------------
summary(data$media_simce) 
table(data$media_simce, useNA = "ifany")
summary(is.na(data$media_simce)) # Veamos los missing

# Vamos a generar una variable categórica de 3 grupos
# 25% "inferior" (1)
# 50% "normal"   (2) 
# 25% "superior" (3)

### 2.3.1 if_else()  ---------------------------
quantile(data$media_simce, probs = 0.25, na.rm = T)
quantile(data$media_simce, probs = 0.75, na.rm = T)

# if_else es un condicional de una entrada: V -> Realiza una acción o F -> Realiza otra acción 
# Cuando tenemos más de una entrada podemos anidar los if_else()
data <- data %>% 
  mutate(prom_cat = if_else(media_simce<=quantile(media_simce, probs = 0.25, na.rm = T), 1, 
                            if_else(media_simce>quantile(media_simce, probs = 0.25, na.rm = T) &
                                      media_simce<quantile(media_simce, probs = 0.75, na.rm = T), 2,
                                    if_else(media_simce>=quantile(media_simce, probs = 0.75, na.rm = T), 3, NA_real_))))

# Etiquetamos la variable
var_label(data$prom_cat) <- "Grupos de rendimiento académico" 

# Validamos 
table(data$prom_cat, useNA = "ifany")
aggregate(data$media_simce ~ data$prom_cat, FUN=summary)
data %>% select(media_simce, prom_cat) %>% head()

### 2.3.2 case_when()  ---------------------------
# case_when() es la forma optimizada del if_else() anidado. El case_when() se utiliza en este y mucho otros lenguajes
data <- data %>% 
  mutate(prom_cat2 =  case_when(media_simce <= quantile(media_simce, probs = 0.25, na.rm = T)    ~ 1, 
                                media_simce > quantile(media_simce, probs = 0.25, na.rm = T) & 
                                  media_simce < quantile(media_simce, probs = 0.75, na.rm = T) ~ 2, 
                                media_simce >= quantile(media_simce, probs = 0.75, na.rm = T)    ~ 3, 
                                TRUE ~ NA_real_))

# Validamos 
table(data$prom_cat, data$prom_cat2, useNA = "ifany")
data %>% select(media_simce, prom_cat, prom_cat2) %>% head()

### 2.3.3 Indexación  ---------------------------
# La indexación es la forma de recodificar con R base
data$prom_cat3[data$media_simce <= quantile(data$media_simce, probs = 0.25, na.rm = T)] <- 1
data$prom_cat3[data$media_simce >  quantile(data$media_simce, probs = 0.25, na.rm = T) & 
                      data$media_simce < quantile(data$media_simce, probs = 0.75, na.rm = T)] <- 2
data$prom_cat3[data$media_simce >= quantile(data$media_simce, probs = 0.75, na.rm = T)] <- 3

table(data$prom_cat, data$prom_cat3, useNA = "ifany")
data %>% select(media_simce, prom_cat, prom_cat2, prom_cat3) %>% head()

# Eliminamos variables creadas
data$prom_cat2 <- NULL
data$prom_cat3 <- NULL

########################################################################/
## Tema 3: Exploremos una tabla de datos real ----
########################################################################/

# Importemos otra tabla de datos
# Muestra aleatoria de 250000 observaciones del MINEDUC (rendimiento académico)
df <- rio::import("Input/20220302_Rendimiento_2021_20220131_WEB_subsample.csv") 

# ---- PARENTESIS ----
# ¿Cómo construir muestras aleatorias? Aquí un ejemplo: 
set.seed(123) # La semilla nos permite reproducir un proceso aleatorio 
ids <- sample(x=df$MRUN, size=1000, replace = TRUE) # Muestra aleatoria

df_subs <- df %>% filter(MRUN %in% ids) # A pesar de que tenemos un id repetido el filtro solo nos entrega información única.

# Exploramos los duplicados
table(duplicated(ids))
table(duplicated(df$MRUN))

# Generamos una variable con duplicados
df_subs <- df_subs %>% 
  mutate(id_dup=if_else(duplicated(MRUN)==TRUE, 1, 0)) %>% 
  arrange(id_dup, MRUN)

rm(list=ls()[ls() %in% c("df_subs", "ids")])
# ---- FIN PARENTESIS ----

# Realicemos algunos ajustes a nuestra tabla de datos
df <- df %>% 
  mutate(id_dup=if_else(duplicated(MRUN)==TRUE, 1, 0)) %>% 
  filter(PROM_GRAL!=0 & ASISTENCIA!=0 & id_dup==0) # Removemos los casos anómalos

# Corrijamos nuestras variables a un formato estándar
colnames(df)
colnames(df) <- janitor::make_clean_names(colnames(df))

glimpse(df)

########################################################################/
## Tema 4: Variables numéricas ----
########################################################################/

### 4.1 Ajustamos una variable ----
df$prom_gral
df <- df %>% 
  mutate(prom_gral=as.numeric(stringr::str_replace(prom_gral, pattern = ",", replacement = ".")))

### 4.2 Estadísticos descriptivos ----
mean(df$prom_gral) 
# Dispersión 
sd(df$prom_gral)
var(df$prom_gral)
max(df$prom_gral) - min(df$prom_gral)
# Posición 
min(df$prom_gral)
max(df$prom_gral)
median(df$prom_gral)
quantile(df$prom_gral, probs = 0.5)

# Histograma
hist(df$prom_gral, breaks = 50)
table(df$prom_gral, useNA = "ifany")

# Función group_by
df <- df %>% 
  group_by(rbd) %>% 
  mutate(promedio=(prom_gral-mean(prom_gral, na.rm = TRUE))/sd(prom_gral, na.rm = TRUE)) %>% 
  mutate(tasa = prom_gral/asistencia)

# Desactivamos
df <- df %>% ungroup()

# Generamos una tabla de ejemplo
tabla_genero <- df %>% 
  group_by(gen_alu) %>% 
  summarise(media1=mean(prom_gral, na.rm = TRUE),
            sd1=sd(prom_gral, na.rm = TRUE),
            media2=mean(promedio, na.rm = TRUE),
            sd2=sd(promedio, na.rm = TRUE),)

tabla_genero

########################################################################/
## Tema 5: Variables tipo strings ----
########################################################################/

df$nom_rbd # Trabajemos con los nombres de los colegios
string1 <- df$nom_rbd

### 5.1 Largo ----
str_length(string1)

### 5.2 Concatenar cadenas de texto ---- 
str_c("Colegio", string1, sep=": ")
paste0("Colegio:", string1)
paste("Colegio:", string1)
paste("Colegio", string1, sep=": ")

### 5.3 Extraer texto ----
str_sub(string1, start=1, end=2)
str_sub(string1, start=-2, end=-1)

### 5.4 Ordenar un texto ----
str_sort(string1, locale = "es") # locale corresponde a la configuración de idioma de nuestra sesión de R. 

# Ajustes de texto
str_to_lower(string1)
str_to_upper(string1)
str_to_sentence(string1)
str_to_title(string1)

### 5.5 Trabajar con coincidencias ----
df <- df %>% 
  mutate(name_ajust=str_replace_all(nom_rbd, c("COLEGIO" = "ESCUELA")))

str_detect(df$nom_rbd, pattern = "COLEGIO")
table(str_detect(df$nom_rbd, pattern = "COLEGIO"))

str_detect(df$name_ajust, pattern = "COLEGIO")
table(str_detect(df$name_ajust, pattern = "COLEGIO"))

# Contemos el número de vocales para ese vector
df <- df %>% 
  mutate(
    vocales = str_count(nom_rbd, "[aáeéiíoóuúü]"),
    consonantes = str_count(nom_rbd, "[^aáeéiíoóuúü]") # ¿Son todas las consonantes?
  ) %>% 
  arrange(vocales)

# Uso de expresiones regulares
# Expresiones regulares: https://www.diegocalvo.es/expresiones-regulares-en-r/

# \\d	Dígito del 1 al 9	1,2 … 9
# \\D	Distinto de dígito	A, a, $, )
# \\s	Espacio	
# \\S	Distinto de espacio	
# \\w	Palabra	A, B, C, d, e, f, …
# \\W	Distinto de palabra	_, &, #, …
# \\t	Tabulador	
# \\n	Salto de linea	
# ^	Comienzo de cadena	^C -> Casa, Coche, …
# $	Fin de cadena	s$ -> Casas, coches, …
# \	Caracteres especiales.	\\, \+
#   |	O lógico OR	(v|b)aca -> vaca o baca
# •	Cualquier carácter excepto \n	
# [ab]	O lógico OR	a, b
# [^ab]	Distintos de ab	c, d, e, f, …
# [0-9]	Todos los dígitos	0, 1, 2, 3, …
# [A-Z]	Todas las letras mayúsculas	A, B, C, …
# [a-z]	Todas las letras minúsculas	a, b, c, …
# [A-z]	Todas las letras	A, B, C, d, e, f, …
# a+	Letra «a» al menos una vez	a, aa, aaa, …
# a*	Letra «a» cero o más veces	a, , aa, aaa, …
# a?	Letra «a» cero o una vez	a
# a{4}	Buscar 4 «a» seguidas	aaaa
# a{2,4}	Buscar entre 2 y 4 «a» seguidas	aa, aaa, aaaa
# a{2,4}?	Buscar entre 2 y 4 «a» seguidas como mucho una vez	aa, aaaa, …
# a{2,}	Busca a partir de 2 «a» seguidas	aa, aaa, aaaa, aaaa, …
# [:alnum:]	Caracteres alfanuméricos [:alpha:] y [:digit:]	A, B, c, d, 1, 2, …
# [:alpha:]	Caracteres: [:lower:] y [:upper:]	A, B, c, d, …
# [:blank:]	Caracteres blancos	Espacio, Tabulador, …
# [:cntrl:]	Caracteres de control	
# [:digit:]	Dígitos	0, 1, 2, 3, …
# [:graph:]	Caracteres gráficos [:alnum:] y [:punct:]	A, B, c, d, 1, 2, #, %, …
# [:lower:]	Todas las letras minúsculas	a, b, c, …
# [:print:]	Caracteres gráficos [:alnum:] y [:punct:]	A, B, c, d, 1, 2, #, %, …
# [:punct:]	Caracteres de puntuación	! » # $ % & ‘ ( ) * + , – . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
# [:space:]	Caracteres de espaciado	Espacio, tabulador, nueva linea, …
# [:upper:]	Todas las letras mayúsculas	A, B, C, …
# [:xdigit:]	Dígitos hexadecimales	0, 1, 2, 3, A, B, e, f, …

df <- df %>% 
  mutate(numeros = str_extract(nom_rbd, pattern = "[:digit:]")) %>% 
  mutate(numeros2 = str_extract(nom_rbd, pattern = "[:digit:]+")) %>% 
  arrange(desc(numeros))

########################################################################/
## Tema 6: Variables tipo date ----
########################################################################/

# Ocuparemos las funciones de lubridate
## Definiendo fechas y horas
lubridate::today()
lubridate::now()

## Desde cadenas de caracteres
ymd("2022-10-01")

mdy("Octubre 01, 2022")

dmy("31-Oct-2022")

## Tipo de objeto 
hoy <- ymd("2022-10-01")
ahora <- lubridate::now()

class(hoy)
class(ahora)

exacto <- ymd_hms("2023-03-24 19:52:23")
class(exacto)

## Veamos fechas separadas
library(datos)
vuelos <- vuelos

vuelos %>% 
  select(anio, mes, dia, hora, minuto)

# Cremos una fecha 
vuelos %>% 
  select(anio, mes, dia) %>%
  mutate(fecha = make_date(anio, mes, dia))

# Exacta
vuelos %>%
  select(anio, mes, dia, hora, minuto) %>%
  mutate(salida = make_datetime(anio, mes, dia, hora, minuto))

# Podemos extraer los componentes
test <- vuelos %>%
  select(anio, mes, dia) %>%
  mutate(fecha = make_date(anio, mes, dia))

View(test)

year(test$fecha) # año
month(test$fecha) # mes
mday(test$fecha) # día

yday(test$fecha) # día del mes
wday(test$fecha) # día de la semana 

# Uso de locales de idioma 
Sys.setlocale(category = "LC_ALL", "en_US.UTF-8") # Al ingles

month(test$fecha, label = TRUE)
wday(test$fecha, label = TRUE, abbr = FALSE)

Sys.setlocale(category = "LC_ALL", "es_ES.UTF-8") # Al español

month(test$fecha, label = TRUE)
wday(test$fecha, label = TRUE, abbr = FALSE)

# Calculo de rangos de tiempo
test$tiempo <- today() - ymd(test$fecha)

# Intervalos 
years(1) / days(1)

siguiente_anio <- today() + years(1)
(today() %--% siguiente_anio) / ddays(1)

today() %--% ymd(20220804)/ ddays(1)

?lubridate

########################################################################/
# Tema 6: Variables tipo factor -------------
########################################################################/

Sys.setlocale(category = "LC_ALL", "es_ES.UTF-8") # Al español
test <- test %>% 
  mutate(mes2=lubridate::month(fecha, label = TRUE))

glimpse(test)

class(test$mes2)

levels(test$mes2)

factor(test$mes2)

# Creando factores
x1 <- c("Dic", "Abr", "Ene", "Mar")
sort(x1)

niveles_meses <- c(
  "Ene", "Feb", "Mar", "Abr", "May", "Jun",
  "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"
)

y1 <- factor(x1, levels = niveles_meses)
y1

sort(y1)

########################################################################/
## Tema 7: Tablas descriptivas  ----
########################################################################/

rm(list=ls()[! ls() %in% c("data")])

## 7.1 Análisis descriptivos univariados ----

# Podemos aplicar estos mismos análisis con dplyr:
tabla_descriptivos <- data %>%
  dplyr::summarize(count=length(na.omit(media_simce)),
                   media=mean(media_simce, na.rm=TRUE),
                   sd=sd(media_simce, na.rm=TRUE),
                   min=min(media_simce, na.rm=TRUE),
                   q25=quantile(media_simce, na.rm=TRUE, probs=0.25),
                   q50=quantile(media_simce, na.rm=TRUE, probs=0.50),
                   q75=quantile(media_simce, na.rm=TRUE, probs=0.75),
                   max=max(media_simce, na.rm=TRUE))

tabla_descriptivos 

writexl::write_xlsx(tabla_descriptivos, "output/tabla_descriptivos.xlsx")

# Para variables discretas esto se trabaja a través de los group_by()

tabla_frecuencias <- data %>% 
  dplyr::group_by(gen_alu) %>% 
  dplyr::summarise(n=n()) %>% 
  mutate(fr=n/sum(n), 
         porc=fr*100)

tabla_frecuencias

writexl::write_xlsx(tabla_frecuencias, "output/tabla_frecuencias.xlsx")

## 6.2 Análisis descriptivos bivariados ----

## Ejemplo 1: Me interesa saber el promedio simce según género 

tabla_biv1 <- data %>%
  group_by(gen_alu) %>% 
  dplyr::summarize(count=length(na.omit(media_simce)),
                   media=mean(media_simce, na.rm=TRUE),
                   sd=sd(media_simce, na.rm=TRUE),
                   min=min(media_simce, na.rm=TRUE),
                   q25=quantile(media_simce, na.rm=TRUE, probs=0.25),
                   q50=quantile(media_simce, na.rm=TRUE, probs=0.50),
                   q75=quantile(media_simce, na.rm=TRUE, probs=0.75),
                   max=max(media_simce, na.rm=TRUE))

tabla_biv1

# Podemos exportar las tablas 
library(writexl)
write_xlsx(tabla_biv1, "output/Tabla_bivariada.xlsx")

########################################################################/
## Ejercicios Propuestos   ----
########################################################################/

# Realice un análisis descriptivo con la tabla de datos "chile.csv".
# Esta tabla contabliza el cambio de temperatura mensual entre 1961 a 2019.
# No olvide ajustar sus datos. 
