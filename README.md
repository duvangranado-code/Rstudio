# Rstudio
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
 
