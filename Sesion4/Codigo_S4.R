########################################################################/
# Campamento de invierno ------
# Escuela de Gobierno UC - 02/08/2023
########################################################################/
# Manipulación de tablas y objetos
# José Daniel Conejeros - jdconejeros@uc.cl
########################################################################/

# En este taller nos enfocaremos en generar visualizaciones de datos con ggplot2

########################################################################/
# Tema 1: Librerias y datos -------------
########################################################################/

# Desactivar notación científica y limpiar la memoria
options(scipen=999)
rm(list=(ls()))

# Vamos a cargar las las librerías 
#install.packages("") # Puedes escribir entre las comillas sino tienes instalada alguna librería
library(rio)       # Importar BBDD
library(dplyr)     # Manipulación de datos
library(forecast)  # Series de tiempo
library(ggplot2)   # Visualización
library(viridis)   # Gradiente de colores
library(RColorBrewer) # Paletas de colores
library(ggsci)        # Paletas de journals
library(patchwork) # Ordenamiento de gráficos
library(gridExtra) # Ordenamiento de gráficos y otras funciones
library(ggpubr) # Ordenamiento de gráficos y otras funciones
library(lubridate) # Manejo de fechas  
library(tidyr)     # Manipulación de tablas
library(stringr)   # Manipular caracteres
library(gganimate) # Animaciones


# Referencias de colores
# https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# https://cran.r-project.org/web/packages/ggsci/vignettes/ggsci.html

# Pueden encontrar más gráficos en: https://r-graph-gallery.com/

## 1.1 Abrimos y exploramos nuestras base de datos ----
psu <- rio::import("input/psu_sample.csv")        #Recuerda ajustarlo en tu directorio propio 

## 1.2 Exploramos nuestra base ----
# Muestra de datos PSU - 2016
dim(psu)     
colnames(psu)
head(psu)
str(psu)
summary(psu)


########################################################################/
# Tema 2: Generamos gráficos simples con R-Base  ----
########################################################################/

## 2.1 Histogramas ----
hist(psu$mate, na.rm=TRUE, freq = TRUE)

# Podemos aagregar argumentos para ajustar
hist(psu$mate, main = "Distribución puntaje de matemáticas PSU 2016",  na.rm=TRUE)

hist(psu$mate, main = "Distribución puntaje de matemáticas PSU 2016",  na.rm=T, 
     xlab="Distribución del puntaje", ylab="Frecuencia")

hist(psu$mate, main = "Distribución puntaje de matemáticas",  na.rm=T, 
     xlab="Distribución del puntaje", ylab="Frecuencia", col="blue", xlim=c(400, 700))

hist(psu$mate, main = "Distribución puntaje de matemáticas",  na.rm=T, 
     xlab="Distribución del puntaje", ylab="Frecuencia",  col="white", xlim=c(100, 900), nclass=50, freq=T)

hist(psu$mate, main = "Distribución puntaje de matemáticas",  na.rm=T, 
     xlab="Distribución del puntaje", ylab="Densidad",  col="white", xlim=c(100, 900), breaks=50, freq=F)
lines(density(psu$mate), lwd = 1, col = "blue")

# Podemos acumular especificaciones
# Las curvas de densidad no son una proporción, sino que la función de densidad de probabilidad. 
hist(psu$mate, main = "Distribución puntaje PSU de matemáticas", 
     xlab="Distribución del puntaje", col="white", ylab="Densidad", xlim=c(100, 900), breaks=50, freq=F)
lines(density(psu$mate, adjust=0.5), lwd = 1, col = "blue")
lines(density(psu$mate, adjust=2), lty="dotted", lwd = 2, col = "red") 

## 2.2 Gráficos de Torta ----
pie(table(psu$mate), main="Gráfico de torta puntaje matemáticas")

pie(table(psu$grupo_depend), main="Gráfico de torta dependencia de la escuela")

pie(table(psu$grupo_depend), main="Gráfico de torta dependencia de la escuela", labels=c("Municipal", "Particular Pagado", "Particular subvencionado"))

# Usar una tabla para construir el gráfico
tabla <- as.data.frame(prop.table(table(psu$grupo_depend))*100)
tabla$Freq <- round(tabla$Freq,1)

pie(tabla$Freq, labels = paste(labels = tabla$Var1, paste0(tabla$Freq, "%"), sep=" "))

## 2.3 Gráficos de Barras ----
par(las=1)
barplot(prop.table(table(psu$sexo)), ylim = c(0,1), main="Gráfico de Barras: Género", cex.names=1, horiz = F)

## 2.4 Boxplot ----
par(las=1)
boxplot(psu$leng ~ psu$grupo_depend,
        horizontal=T, 
        ylab = " ",
        xlab = "Puntaje en la PSU de lenguaje", 
        names = c("Municipal", "Part. Pagado", "Part. Subvencionado"), 
        boxwex = 0.5, cex.axis=0.5)
title(main = "Gráfico de caja puntajes PSU de lenguaje", font.main= 3, cex.main = 1)

########################################################################/
# Tema 3. Primera exploración con la librería de ggplot  ----
########################################################################/

install.packages("ggplot2")
library(ggplot2)

ggplot(data=psu)

## 3.1 Argumentos Básicos ----
# Es importante ir agregando información por capas

ggplot(data=psu, mapping=aes(x=leng, y=mate)) #Agrego ejes: capa2

ggplot(data=psu, aes(x=leng, y=mate)) + 
  geom_point()  #Agrego geometría: points. capa3

ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.005) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") 

ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") +
  theme_minimal() +
  theme(plot.title = element_text(size = 32, face="bold"),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=13, face="bold"), 
        axis.title.y=element_text(size=10))

## 3.2 Aplicación de temas ----
# Podría ajustar temas genericos
ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  theme_bw()

ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  theme_classic()

ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  theme_light()


ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(color = "Género") +
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas")

## 3.3 Facetas ----
# Podemos agregar facetas
ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  labs(color = "Género") +
  theme_bw() +
  facet_grid(~factor(psu$grupo_depend)) 


## 3.4 Guardar gráficos ----
# Camino 1: Directo
ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  labs(color = "Género") +
  theme_bw() +
  facet_grid(~factor(psu$grupo_depend)) 
ggsave("Output/mi_primer_ggplot.png")

# Camino 2: Como objetos
g1 <-  ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  labs(color = "Género") +
  theme_minimal()

ggsave("Output/g1.png", plot=g1)

# Camino 3: Múltiples gráficos
g2 <- ggplot(data=psu, aes(x=leng, y=mate, color=factor(sexo))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth() + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Puntaje lenguaje", y="Puntaje matemáticas") + 
  labs(color = "Género") +
  theme_bw() +
  facet_grid(~factor(psu$grupo_depend)) 
ggsave("Output/g2.png", plot=g2)

library(ggpubr)
g3 <- ggarrange(g1, g2, ncol=1, nrow=2)
g3
ggsave("Output/g3.png", plot=g3)


library(patchwork)
g3 <- g1/g2
g3
ggsave("Output/g3.png", plot=g3)

library(gridExtra)
g3 <- grid.arrange(g1, g2)
g3
ggsave("Output/g3.png", plot=g3)


# Camino 4: Ejecutar y guardar
png(file = "Output/g4.png", height = 900, width = 1200)
ggarrange(g1, g2, ncol = 1, nrow = 2,  common.legend = TRUE, legend="right")
dev.off()

########################################################################/
# Tema 4. Gráficos Descriptivos  ----------------------------------------------------------------
########################################################################/

## 4.1 Histogramas ----
# Ejemplo 1: Agregar curva de densidad
ggplot(data=psu, aes(x=leng)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 10,
                 colour="black", fill="white") +
  scale_x_continuous() +
  labs(title="Distribución Puntajes",
       x="PSU - Lenguaje", y = "Densidad") +
  geom_density(col="red") + 
  theme_bw()


# Ejemplo 1: Agregar curva normal
ggplot(data=psu, aes(x=educmadre)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 1,
                 colour="black", fill="white") +
  scale_x_continuous(n.breaks = 14) +
  labs(title="Histograma",
       x="Años de educación de la madre", y = "Densidad") +
  geom_density(col="red") + 
  stat_function(fun = dnorm, n = nrow(psu), args = list(mean = mean(psu$educmadre), sd = 1), colour = "blue") +
  theme_bw()

## 4.2 Gráficos de Barras ----
ggplot(data=psu, aes(x=grupo_depend, fill=as.factor(grupo_depend))) + 
  geom_bar(color="black") +
  xlab("Dependencia del estudiante") + 
  ylab("Cantidades") +
  labs(fill = "Dependencia") +
  ggtitle("Gráfico de Barras") 

# Podríamos hacer algunos ajustes 
ggplot(data=psu, aes(x=grupo_depend, fill=as.factor(grupo_depend))) + 
  geom_bar() +
  scale_fill_grey() +
  xlab("Dependencia del estudiante") + 
  ylab("Cantidades") +
  ggtitle("Gráfico de Barras") + 
  labs(fill = "Dependencia") +
  coord_flip()

# Podríamos utilizar nuestra tabla
ggplot(data=tabla, aes(x=Var1, y=Freq)) + 
  geom_bar(stat = "identity",  fill="steelblue", width=0.5) +
  geom_text(aes(label=paste0(Freq, "%")), hjust=-.1, vjust=0, size=5, fontface="bold")+
  xlab("Dependencia del estudiante") + 
  ylab("Cantidades") +
  ggtitle("Gráfico de Barras") +
  labs(fill = "Dependencia") +
  theme_bw() +
  coord_flip()

## 4.3 Box-Plots ----

ggplot(data=psu, aes(x=as.factor(sexo), y=cien, fill=factor(sexo))) + 
  geom_boxplot(width=0.5)+ 
  xlab("Distribución Puntaje") + 
  ylab(" ") + 
  labs(fill = "Género") +
  ggtitle("Boxplot rendimiento Ciencia") +
  theme_minimal()

## 4.4 Integración con DPLYR ----
library(dplyr)
# Completo
f1 <- psu %>% 
  ggplot(aes(x=as.factor(sexo), y=cien, fill=factor(sexo))) + 
  geom_boxplot(width=0.3) + 
  xlab("Distribución Puntaje") + 
  ylab("Género") + 
  labs(fill = "Género") +
  ggtitle("Boxplot rendimiento")

# Municipal
f2 <- psu %>%  filter(factor(psu$grupo_depend) == "Municipal (Dependencia 1, 2, 5)") %>% 
  ggplot(aes(x=as.factor(sexo), y=cien, fill=factor(sexo))) + 
  geom_boxplot(width=0.3) + 
  xlab("Distribución Puntaje") + 
  ylab("Género") + 
  labs(fill = "Género") +
  ggtitle("Municipal")


# Particular pagado
f3 <- psu %>% filter(factor(psu$grupo_depend) == "Particular Pagado (Dependencia 4)") %>% 
  ggplot(aes(x=as.factor(sexo), y=cien, fill=factor(sexo))) + 
  geom_boxplot(width=0.3) + 
  xlab("Distribución Puntaje") + 
  ylab("Género") + 
  labs(fill = "Género") +
  ggtitle("Particular Pagado")


# Juntamos todo en un gráfico
ggarrange(f1, f2, f3, ncol = 3, nrow = 1,  common.legend = TRUE, legend="right")

# Guardamos el gráfico
png(file = "Output/g5.png", height = 900, width = 900)
ggarrange(f1, f2, f3, ncol = 3, nrow = 1,  common.legend = TRUE, legend="right")
dev.off()

########################################################################/
# Tema 5. Gráficos de asociación  ----
########################################################################/

## 5.1 Asociaciones lineales  ----

# Asociación entre el puntaje obtenido en ciencias y la educación de la madre/padre según tipo de colegio

a <- ggplot(psu, aes(y=cien, x=educmadre)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x) + 
  ylab("Distribución Puntaje PSU Ciencia") + 
  xlab("Años educación de la madre") + 
  theme_bw() + 
  facet_wrap(~grupo_depend)

b <- ggplot(psu, aes(y=cien, x=educpadre)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x) + 
  ylab("Distribución Puntaje PSU Ciencia") + 
  xlab("Años educación del padre") + 
  theme_bw() + 
  facet_wrap(~grupo_depend)

ggarrange(a, b, ncol = 1, nrow = 2)

# Otras formas 
library(patchwork)
a + b
a / b

# Asociación entre el puntaje obtenido en ciencias y la educación de la madre/padre según tipo de colegio y sexo

c <- ggplot(psu, aes(y=cien, x=educmadre)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x) + 
  ylab("Distribución Puntaje PSU Ciencia") + 
  xlab("Años educación de la madre") + 
  theme_bw() + 
  facet_wrap(~sexo + grupo_depend)

d <- ggplot(psu, aes(y=cien, x=educpadre)) + 
  geom_point()+
  stat_smooth(method="lm", se=T, formula=y ~ x) + 
  ylab("Distribución Puntaje PSU Ciencia") + 
  xlab("Años educación del padre") + 
  theme_bw() + 
  facet_wrap(~sexo + grupo_depend)

png(file = "Output/g6.png", height = 900, width = 900)
ggarrange(c, d, ncol = 1, nrow = 2)
dev.off()

########################################################################/
# Tema 6. Gráfico completo (PROPUESTO) ----
########################################################################/

# Cargamos la BBDD: 
# Reporte de casos nuevos COVID. Datos en formato ancho:
data <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto13/CasosNuevosCumulativo.csv")
data %>% glimpse()
View(data)

# Ajustamos como serie de tiempo
data_figura <- data %>% slice(-17) %>% 
  pivot_longer(cols=!Region, names_to="date", values_to = "casos") %>% # Pivoteamos la BBDD  
  mutate(date=str_replace_all(date, c("[X]"="", "[.]"="-")),           # Reemplazamos valores a partir de una expresión regular
         date=lubridate::ymd(date))                                    # Ajustamos la fecha

# Verifiquemos la cantidad de filas
data_figura %>% head(n=30)
unique(data_figura$Region) # Veamos los valores para las regiones

reg <- unique(data_figura$Region)
reg

# Graficamos
# Podemos fijar una paleta de colores:  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
display.brewer.all(colorblindFriendly = TRUE)
colores <- brewer.pal(n=4, name="Set2")

g1 <- data_figura %>% 
  ggplot(aes(x=date, y=casos, color=Region)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "12 week", 
               guide = guide_axis(n.dodge = 2)) +
  labs(x="Semanas del año", y = "Casos de Covid-19",
       color = "Región")  +
  geom_vline(xintercept=ymd("2021-02-01"), linetype="dotdash") +
  facet_wrap(~factor(Region, levels=reg), ncol = 4, scale = "free") +
  geom_label(aes(x = ymd("2021-02-01"), y = 10, label = "Inicio vacunación"),
             color = "white", size = 2, hjust = "middle", label.size = NA, fill = "#d36f6f") + 
  theme_light(base_size = 12) + 
  theme(axis.text=element_text(size=8),
        axis.text.x = element_text(size=6),
        legend.position="none",
        strip.text = element_text(size = 10, face = "bold", color = "gray40"),
        strip.background = element_rect(fill="white", colour="gray", linetype="dashed"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour="grey", linetype="dotted", linewidth=0.3)) 

g1 # Revisamos nuestro resultado

# Podemos guardar la figura con varios ajustes de formato
ggsave(plot = g1,
       filename = "Output/Visualización_covid.png",
       res = 300,
       width = 30,
       height = 20,
       units = 'cm',
       scaling = 0.7,
       device = ragg::agg_png)

########################################################################/
# Tema 7: Animar figuras -------------
########################################################################/

# Podemos crear un gif animado a partir de la librería gganimate
install.packages("gganimate") # más info en: https://gganimate.com/
install.packages("gifski")
library(gganimate)
library(gifski)

# Construímos la animación
ganimado <- g1 + transition_reveal(date) + 
  labs(title = "Casos de Covid-19 para el día: {frame_along}")  + 
  ease_aes('linear')

# Generamos la animación
animate(ganimado, width = 1200, height = 900, fps = 5, duration = 5, 
        rewind = FALSE, renderer = gifski_renderer("Output/gganim_covid.gif")) # Más rápido de ejecutar

animate(ganimado, width = 1200, height = 900, fps = 30, duration = 30, 
        rewind = FALSE, renderer = gifski_renderer("Output/gganim_covid.gif")) # Tarda aprox 20 min.

