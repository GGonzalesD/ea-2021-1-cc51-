library(dplyr)
#LEYENDO LA TABLA PREPROCESADA Y LA ORIGINAL PARA SU COMPARACION
Tabla_preprocesada <- read.csv("hotel_bookings_miss_pre.csv", header = TRUE, stringsAsFactors = FALSE)
Tabla_Original <- read.csv("hotel_bookings_miss.csv", header = TRUE, stringsAsFactors = FALSE)
View(Tabla_preprocesada)





#COMPARACION DE LOS DATOS DESPUES DE MODIFICAR LOS VALORES NA CON EL METODO DE LA MEDIA DE LA POBLACION

# Funcion Verificar las columnas con valores NA
verificar_NA <- function(df, columna){
  print(columna)
  vari <- is.na(df[,c(columna)])
  print(table(vari))
}

verificar_NA(Tabla_Original, "adults")
verificar_NA(Tabla_preprocesada, "adults")


verificar_NA(Tabla_Original, "children")
verificar_NA(Tabla_preprocesada, "children")





#COMPARACION DE LOS DATOS DESPUES DE MODIFICAR LOS VALORES NA CON LOS VALORES RANDOM

verificar_NA(Tabla_Original, "arrival_date_day_of_month")
verificar_NA(Tabla_preprocesada, "arrival_date_day_of_month")







#COMPARACION DE LOS DATOS DESPUES DE ELIMINAR LOS DATOS ATIPICOS DE LA TABLA ADR Y LEAD TIME

verificar_outliers <- function(df, df_names){
  for(variable in df_names) {
    print(variable)
    outliers.values <- boxplot(df[,c(variable)], main = paste(variable,"con Outliers"))$out
    outliers.values 
  }
}
par(mfrow = c(1,2))

verificar_outliers(Tabla_Original, c("adr"))
boxplot((Tabla_preprocesada$adr), main = "adr sin outliers")



verificar_outliers(Tabla_Original, c("lead_time"))
boxplot((Tabla_preprocesada$lead_time), main = "lead_time sin outliers")

