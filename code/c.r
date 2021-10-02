# c. ¿Cuándo se producen las temporadas de
#  reservas: alta, media y baja?

library("dplyr")
library("lubridate")

# Cargar Datos
hotel_data <- read.csv("hotel_bookings_miss_pre.csv")

# Cuando se producen las temporadas de reserva
m_x <- month(as.POSIXlt(hotel_data$reservation_status_date,
	format="%d/%m/%Y"))
hotel_data$mon <- m_x

hotel_data.grp <- hotel_data %>%
  group_by(mon) %>% 
  summarise(n = n())
hotel_menor_d <- min(hotel_data.grp$n)
hotel_mayor_d <- max(hotel_data.grp$n)
d = (hotel_mayor_d - hotel_menor_d)/3

colors_menor <- (hotel_data.grp$n >= hotel_menor_d &
	hotel_data.grp$n < hotel_menor_d + d)
colors_mayor <- (hotel_data.grp$n <= hotel_mayor_d &
	hotel_data.grp$n > hotel_mayor_d - d)
colors_medio <- ( hotel_data.grp$n < hotel_mayor_d - d &
	hotel_data.grp$n > hotel_menor_d + d)
colores <- ifelse(colors_mayor , "green" ,
	ifelse (colors_menor, "red" ,
	ifelse(colors_medio,"yellow", "gray")))

barplot(hotel_data.grp$n, names.arg=month.abb,
	col = colores, main="Temporadas Alta, Medias y Bajas por mes")