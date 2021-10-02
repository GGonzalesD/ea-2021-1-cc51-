library(dplyr)
Tabla_Datos <- read.csv("hotel_bookings_miss.csv", header = TRUE, stringsAsFactors = FALSE)
Tabla_Original <- read.csv("hotel_bookings_miss.csv", header = TRUE, stringsAsFactors = FALSE)
View(Tabla_Datos)

# Funcion Verificar las columnas con valores NA
verificar_NA <- function(df){
  for(variable in names(df)) {
    print(variable)
    vari <- is.na(df[,c(variable)])
    print(table(vari))
  }
}

# Funcion Arreglar columna con reemplazo de datos NA con la media de la población
Reemplazar_media <- function(columna){
  colum <- Tabla_Datos[,c(columna)]
  Tabla_Datos[,c(columna)][colum == 0] <- NA
  temp.mean <- ifelse(is.na(colum), mean(colum, na.rm = TRUE), colum)
  temp.mean <- round(temp.mean, digits = 0)
  return(temp.mean)
}

# Arreglar las columnas:

# Columna stays_in_weekend_nights
Tabla_Datos$stays_in_weekend_nights.mean <- Reemplazar_media("stays_in_weekend_nights")
Tabla_Datos$stays_in_weekend_nights <- NULL
names(Tabla_Datos)[32] <- "stays_in_weekend_nights"

# Columna lead_time
Tabla_Datos$lead_time.mean <- Reemplazar_media("lead_time")
Tabla_Datos$lead_time <- NULL
names(Tabla_Datos)[32] <- "lead_time"

# Columna arrival_date_year
Tabla_Datos$arrival_date_year.mean <- Reemplazar_media("arrival_date_year")
Tabla_Datos$arrival_date_year <- NULL
names(Tabla_Datos)[32] <- "arrival_date_year"

# Columna arrival_date_week_number
Tabla_Datos$arrival_date_week_number.mean <- Reemplazar_media("arrival_date_week_number")
Tabla_Datos$arrival_date_week_number <- NULL
names(Tabla_Datos)[32] <- "arrival_date_week_number"

# Columna stays_in_week_nights
Tabla_Datos$stays_in_week_nights.mean <- Reemplazar_media("stays_in_week_nights")
Tabla_Datos$stays_in_week_nights <- NULL
names(Tabla_Datos)[32] <- "stays_in_week_nights"

# Columna adults
Tabla_Datos$adults.mean <- Reemplazar_media("adults")
Tabla_Datos$adults <- NULL
names(Tabla_Datos)[32] <- "adults"

# Columna children
Tabla_Datos$children.mean <- Reemplazar_media("children")
Tabla_Datos$children <- NULL
names(Tabla_Datos)[32] <- "children"

# Columna babies
Tabla_Datos$babies.mean <- Reemplazar_media("babies")
Tabla_Datos$babies <- NULL
names(Tabla_Datos)[32] <- "babies"

# Columna babies
Tabla_Datos$days_in_waiting_list.mean <- Reemplazar_media("days_in_waiting_list")
Tabla_Datos$days_in_waiting_list <- NULL
names(Tabla_Datos)[32] <- "days_in_waiting_list"

#//////////////////////////////////////////////////////////////////////////////////

# Reemplazar valores NA con un valor aleatorio simple (variables categoricas)

rand.valor <- function(x){
  faltantes <- is.na(x)
  tot.faltantes <- sum(faltantes)
  x.obs <- x[!faltantes]
  valorado <- x
  valorado[faltantes] <- sample(x.obs, tot.faltantes, replace = TRUE)
  return (valorado)
}

random.df <- function(df, cols){
  nombres <- names(df)
  for (col in cols) {
    nombre <- paste(nombres[col], "valorado", sep = ".")
    df[nombre] <- rand.valor(df[,col])
  }
  df
}

data.limpio <- random.df(Tabla_Datos, c(3))
Tabla_Datos$arrival_date_day_of_month <- data.limpio$arrival_date_month.valorado
rm(data.limpio)

verificar_NA(Tabla_Datos)


#/////////////////////////////////////////////////////////////////////////////////////

# Deteccion de valores atipicos:

# Funcion para verificar los utliers en las columnas
verificar_outliers <- function(df, df_names){
  for(variable in df_names) {
    print(variable)
    outliers.values <- boxplot(df[,c(variable)], main = paste(variable,"con Outliers"))$out
    outliers.values 
  }
}

# Verificando las columnas con valores atipicos

verificar_outliers(Tabla_Datos, c("is_canceled"))
verificar_outliers(Tabla_Datos, c("adr"))
verificar_outliers(Tabla_Datos, c("lead_time"))
verificar_outliers(Tabla_Datos, c("stays_in_weekend_nights"))
verificar_outliers(Tabla_Datos, c("stays_in_week_nights"))
verificar_outliers(Tabla_Datos, c("adults"))
verificar_outliers(Tabla_Datos, c("children"))
verificar_outliers(Tabla_Datos, c("babies"))

# Funcion para corregir los valores atipicos

fix_outliers <- function(x, removeNA = TRUE){
    quantiles <- quantile(x, c(0.05, 0.95), na.rm = removeNA)
    x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
    x[x>quantiles[2]] <- median(x, na.rm = removeNA)
    x
}

# Corregir adr
par(mfrow = c(1,2))
verificar_outliers(Tabla_Datos, c("adr"))
boxplot(fix_outliers(Tabla_Datos$adr), main = "adr sin Outliers")

Tabla_Datos$adr <- fix_outliers(Tabla_Datos$adr)

# Corregir lead_time
par(mfrow = c(1,2))
verificar_outliers(Tabla_Datos, c("lead_time"))
boxplot(fix_outliers(Tabla_Datos$lead_time), main = "lead_time sin Outliers")

Tabla_Datos$lead_time <- fix_outliers(Tabla_Datos$lead_time)

# Corregir stays_in_weekend_nights
par(mfrow = c(1,2))
verificar_outliers(Tabla_Datos, c("stays_in_weekend_nights"))
boxplot(fix_outliers(Tabla_Datos$stays_in_weekend_nights), main = "stays_in_weekend_nights sin Outliers")

Tabla_Datos$stays_in_weekend_nights <- fix_outliers(Tabla_Datos$stays_in_weekend_nights)

# Corregir stays_in_week_nights
par(mfrow = c(1,2))
verificar_outliers(Tabla_Datos, c("stays_in_week_nights"))
boxplot(fix_outliers(Tabla_Datos$stays_in_week_nights), main = "stays_in_week_nights sin Outliers")

Tabla_Datos$stays_in_week_nights <- fix_outliers(Tabla_Datos$stays_in_week_nights)

# Corregir adults
par(mfrow = c(1,2))
verificar_outliers(Tabla_Datos, c("adults"))
boxplot(fix_outliers(Tabla_Datos$adults), main = "adults sin Outliers")

Tabla_Datos$adults <- fix_outliers(Tabla_Datos$adults)

# Corregir children
par(mfrow = c(1,2))
verificar_outliers(Tabla_Datos, c("children"))
boxplot(fix_outliers(Tabla_Datos$children), main = "children sin Outliers")

Tabla_Datos$children <- fix_outliers(Tabla_Datos$children)

# Corregir babies
par(mfrow = c(1,2))
verificar_outliers(Tabla_Datos, c("babies"))
boxplot(fix_outliers(Tabla_Datos$babies), main = "babies sin Outliers")

Tabla_Datos$babies <- fix_outliers(Tabla_Datos$babies)

# GUARDAR DATA PROCESADA:
write.csv(Tabla_Datos,"hotel_bookings_miss_pre.csv")
