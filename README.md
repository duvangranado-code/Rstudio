### Trabajo 1 ###
 
   # Duvan Estiben Granado#
   # Taylor #
   # Yovana Patricia López Marin #
   
   # Cargar base de datos #
   
   library(readxl)
 Datos <- read_excel("C:/Users/Admin/Desktop/UDEA ING. INDUSTRIAL/SEGUNDO SEMESTRE/ESTADISTICA/R STUDIO/base_cadenas_productivas.xlsx",
                    col_types = c("numeric", "text", "text",
                    "text", "text", "numeric", "numeric",
                     "numeric", "numeric", "numeric"))
 
  #clasificar las variables, lo mensiona en al clase del 8 septiembre en el minuto 49:56#


# primera pregunta #

 Datos=Datos[, -1]
 head(Datos) # ver las 6 primeras filas de todas las variables
 tail(Datos) # ver las ultimas filas de todas las variables de la BD
 attach(Datos) # acceder a los nombres de las variables
 str(Datos) #esctructura de las variables que contiene la base de datos
 names(Datos) # nombresde las variables en la base de datos
 
 #resumen numerico de toda la base de datos
 summary(Datos)
 
 library(car)
 library(sandwich)
 library(RcmdrMisc) 
 
 # resumen numerico # variables cuantitativas
 #
 numSummary(Volumen_produccion_ton)
 numSummary(Datos[, 5:9])
 
 # Segunda pregunta #
 
 numSummary(Datos[,5:9], statistics = c("mean", "sd", "IQR", "quantiles", 
                                        "cv", "skewness"))
 
 # resumen numerico y grafico
 
numSummary(Datos[,5:9], statistics = c("mean", "sd", "quantiles", "cv", "skewness"),
           type = "3",quantiles = c(0, 0.25, 0.5, 0.75, 1))

boxplot(Volumen_produccion_ton~Tipo_actor)

 # 3. ESTADÍSTICA DESCRIPTIVA #
 
 # Medidas para variable cuantitativa (ej. Volumen)
 mean(Volumen_produccion_ton)     # Media
 median(Volumen_produccion_ton)   # Mediana
 sd(Volumen_produccion_ton)       # Desviación estándar
 var(Volumen_produccion_ton)      # Varianza
 quantile(Volumen_produccion_ton) # Cuartiles
 
 # 4. TABLAS DE FRECUENCIAS (CUALITATIVAS) --------------------------------------
 tabla_actor <- table(Tipo_actor)
 tabla_actor                      # Frecuencia absoluta
 prop.table(tabla_actor) * 100    # Frecuencia relativa (%)
 
 library(agricolae)
 H_S=graph.freq(Utilidad_COP, plot = FALSE)
 H_S
 (tabla=table.freq(H_S)) #tabla de frecuencia
 
 # 5. GRÁFICOS Y VISUALIZACIÓN --------------------------------------------------
 # Diagrama de barras
 barplot(tabla_actor, 
         main = "Distribución por Tipo de Actor", 
         col = c("navy", "lightblue", "darkgreen", "orange"),
         ylab = "Frecuencia")
 
 # Histograma
 hist(Volumen_produccion_ton, 
      main = "Histograma de Volumen de Producción", 
      xlab = "Toneladas", 
      col = "steelblue", 
      border = "white")
 
 # Diagrama de caja (Boxplot)
 boxplot(Volumen_produccion_ton, 
         main = "Boxplot de Volumen de Producción", 
         col = "salmon", 
         horizontal = TRUE)
 




#Duvan Estibe Granado Alzate # 
# Nuevo código hasta la pregunta 8 del taller #  


# ==============================================================================
# TRABAJO PRÁCTICO 1 - ESTADÍSTICA DESCRIPTIVA EN R
# Autores: Duvan Estiben Granado, Taylor, Yovana Patricia López Marín
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CARGA DE LIBRERÍAS (Sistema Base + Paquetes de la Clase)
# ------------------------------------------------------------------------------
library(readxl)
library(car)
library(sandwich)
library(RcmdrMisc)
library(dplyr)      # Manipulación de datos y resúmenes
library(ggplot2)    # Gráficos avanzados
library(lattice)    # Visualización con lattice


# ==============================================================================
# PUNTO 1: CARGA, LECTURA Y ESTRUCTURA DE LA BASE DE DATOS
# ==============================================================================

library(readxl)
Datos <- read_excel("C:base_cadenas_productivas.xlsx",
                    col_types = c("numeric", "text", "text",
                                  "text", "text", "numeric", "numeric",
                                  "numeric", "numeric", "numeric"))

# Eliminar primera columna de ID o secuencia irrelevante
Datos <- Datos[, -1]

# Exploración inicial de la estructura
head(Datos)
tail(Datos)
str(Datos)
names(Datos)


# ==============================================================================
# PUNTO 2: CLASIFICACIÓN Y CONVERSIÓN DE VARIABLES
# ==============================================================================

# Conversión explícita de variables cualitativas a factores (Categorías)
Datos$Tipo_actor       <- as.factor(Datos$Tipo_actor)
Datos$Tamano_empresa   <- as.factor(Datos$Tamano_empresa)
Datos$Sector_economico <- as.factor(Datos$Sector_economico)
Datos$Mercado_destino  <- as.factor(Datos$Mercado_destino)

# Verificación de la estructura de la variable
str(Datos$Sector_economico)

# Adjuntar base para acceder directamente a las variables
attach(Datos)

# ==============================================================================
# PUNTO 3: ESTADÍSTICA DESCRIPTIVA (VARIABLES CUANTITATIVAS)
# ==============================================================================

# Resumen numérico general
summary(Datos)

# Resumen estadístico detallado con RcmdrMisc
numSummary(Volumen_produccion_ton)
numSummary(Datos[, 5:9])

# Medidas de tendencia central y dispersión explícitas
mean(Volumen_produccion_ton, na.rm = TRUE)     # Media
median(Volumen_produccion_ton, na.rm = TRUE)   # Mediana
sd(Volumen_produccion_ton, na.rm = TRUE)       # Desviación estándar
var(Volumen_produccion_ton, na.rm = TRUE)      # Varianza
quantile(Volumen_produccion_ton, na.rm = TRUE) # Cuartiles


# ==============================================================================
# PUNTO 4: TABLAS DE FRECUENCIAS (VARIABLES CUALITATIVAS)
# ==============================================================================

# --- 1. Tipo de Actor ---
tabla_actor <- table(Tipo_actor)
tabla_actor                                    # Frecuencia absoluta (fi)
prop.table(tabla_actor) * 100                  # Frecuencia relativa (%)

# --- 2. Tamaño de Empresa ---
tabla_tamano <- table(Tamano_empresa)
tabla_tamano                                   # Frecuencia absoluta (fi)
prop.table(tabla_tamano) * 100                 # Frecuencia relativa (%)

# --- 3. Sector Económico ---
tabla_sector <- table(sector_economico)
tabla_sector                                   # Frecuencia absoluta (fi)
prop.table(tabla_sector) * 100                 # Frecuencia relativa (%)


# ==============================================================================
# PUNTO 5: VISUALIZACIÓN EN SISTEMA BASE DE R
# ==============================================================================

# --- A) Diagramas de Barras (Cualitativas) ---
barplot(tabla_actor,
        main = "Distribución por Tipo de Actor",
        col = c("navy", "lightblue", "darkgreen", "orange"),
        ylab = "Frecuencia")

barplot(tabla_tamano,
        main = "Distribución por Tamaño de Empresa",
        col = c("coral", "lightgreen", "skyblue"),
        ylab = "Frecuencia")

barplot(tabla_sector,
        main = "Distribución por Sector Económico",
        col = c("purple", "gold", "turquoise", "pink"),
        ylab = "Frecuencia")

# --- B) Histogramas y Boxplots (Cuantitativas) ---
# 1. Volumen de Producción
hist(Volumen_produccion_ton,
     main = "Histograma de Volumen de Producción",
     xlab = "Toneladas",
     col = "steelblue",
     border = "white")

boxplot(Volumen_produccion_ton,
        main = "Boxplot de Volumen de Producción",
        col = "salmon",
        horizontal = TRUE)

# 2. Costos de Producción
hist(Costos_produccion_COP,
     main = "Histograma de Costos de Producción",
     xlab = "COP",
     col = "darkseagreen",
     border = "white")

boxplot(Costos_produccion_COP,
        main = "Boxplot de Costos de Producción",
        col = "sandybrown",
        horizontal = TRUE)

# 3. Ingresos
hist(Ingresos_COP,
     main = "Histograma de Ingresos",
     xlab = "COP",
     col = "mediumpurple",
     border = "white")

boxplot(Ingresos_COP,
        main = "Boxplot de Ingresos",
        col = "khaki",
        horizontal = TRUE)

# 4. Utilidad
hist(Utilidad_COP,
     main = "Histograma de Utilidad",
     xlab = "COP",
     col = "cadetblue",
     border = "white")

boxplot(Utilidad_COP,
        main = "Boxplot de Utilidad",
        col = "plum",
        horizontal = TRUE)


# ==============================================================================
# PUNTO 6: VISUALIZACIÓN CON LATTICE Y GGPLOT2 (LIBRERÍAS MODERNAS)
# ==============================================================================
# --- A) Gráfico con Lattice ---
barchart(table(Datos$Sector_economico), 
         xlab = "Frecuencias", ylab = "Sector Económico",
         main = "Número de Empresas por Sector Económico",
         col = "steelblue")

# --- B) Gráficos con ggplot2 ---

# 1. Gráfico de barras simple (Cualitativa: Tipo de Actor)
ggplot(Datos, aes(x = Tipo_actor, fill = Tipo_actor)) +
  geom_bar() +
  labs(title = "Distribución de Empresas por Tipo de Actor",
       x = "Tipo de Actor", 
       y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")

# 2. Histograma moderno (Cuantitativa: Volumen de Producción)
ggplot(Datos, aes(x = Volumen_produccion_ton)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribución del Volumen de Producción",
       x = "Volumen de Producción (Toneladas)", 
       y = "Frecuencia") +
  theme_minimal()

# 3. Boxplot comparativo (Producción por Sector Económico)
ggplot(Datos, aes(x = Sector_economico, y = Volumen_produccion_ton, fill = Sector_economico)) +
  geom_boxplot() +
  labs(title = "Volumen de Producción según Sector Económico",
       x = "Sector Económico", 
       y = "Volumen (Toneladas)") +
  theme_minimal() +
  theme(legend.position = "none")


# ==============================================================================
# PUNTO 7: ANÁLISIS BIVARIADO Y TABLAS CRUZADAS
# ==============================================================================

# --- A) Tabla Cruzada entre Variables Cualitativas ---
tabla_cruzada <- table(Datos$Tamano_empresa, Datos$Tipo_actor)
tabla_cruzada

# Porcentajes por fila
prop.table(tabla_cruzada, margin = 1) * 100

# Gráfico de Barras Lado a Lado (position = "dodge")
ggplot(Datos, aes(x = Tamano_empresa, fill = Tipo_actor)) +
  geom_bar(position = "dodge") +
  labs(title = "Tipo de Actor según Tamaño de Empresa", 
       x = "Tamaño de Empresa", y = "Frecuencia") +
  theme_minimal()

# Gráfico de Barras Apiladas corregido (S mayúscula en Sector_economico)
ggplot(Datos, aes(x = Sector_economico, fill = Tamano_empresa)) +
  geom_bar(position = "stack") +
  labs(title = "Distribución de Tamaño de Empresa por Sector Económico",
       x = "Sector Económico", 
       y = "Frecuencia") +
  theme_minimal()

# --- B) Comparativo: Variable Cualitativa vs Cuantitativa ---
# 1. Volumen de producción según Tipo de actor
boxplot(Volumen_produccion_ton ~ Tipo_actor, data = Datos,
        main = "Volumen de Producción según Tipo de Actor",
        xlab = "Tipo de Actor", 
        ylab = "Volumen de Producción (ton)",
        col = c("lightblue", "lightgreen", "lightcoral", "lightyellow"))

# 2. Utilidad según Tamaño de la empresa
boxplot(Utilidad_COP ~ Tamano_empresa, data = Datos,
        main = "Utilidad según Tamaño de la Empresa",
        xlab = "Tamaño de Empresa", 
        ylab = "Utilidad (COP)",
        col = c("skyblue", "pink", "khaki"))


# ==============================================================================
# PUNTO 8: MATRIZ DE CORRELACIÓN Y ANÁLISIS FINAL
# ==============================================================================

# Correlación lineal entre las variables numéricas
cor_matrix <- cor(Datos[, c("Volumen_produccion_ton", "Costos_produccion_COP", 
                            "Ingresos_COP", "Utilidad_COP")], use = "complete.obs")
round(cor_matrix, 2)

# Gráfico de dispersión bivariado (Ingresos vs Utilidad)
ggplot(Datos, aes(x = Ingresos_COP, y = Utilidad_COP, color = Tamano_empresa)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  labs(title = "Relación entre Ingresos y Utilidad por Tamaño de Empresa",
       x = "Ingresos (COP)", y = "Utilidad (COP)") +
  theme_minimal()
