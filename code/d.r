# d. ¿Cuándo es menor la demanda de reservas?

library("dplyr")
library("lubridate")
library("purrr")

# Cargar Datos
hotel_data <- read.csv("hotel_bookings_miss_pre.csv")

# Meses de cancelacion de Reservas
m_x <- month(as.POSIXlt(hotel_data$reservation_status_date,
	format="%d/%m/%Y"))
hotel_data$mon <- m_x

hotel_data.grp <- hotel_data %>%
	group_by(mon) %>% 
	summarise(n = n())
hotel_menor_d <- min(hotel_data.grp$n)

colors <- hotel_data.grp$n == hotel_menor_d
colors <- ifelse(colors, "#ff9999", "#99ff99")

barplot(hotel_data.grp$n,
names.arg=month.abb,
col=colors, main="Mes con menor demanda de reserva")