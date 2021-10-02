# f. ¿Es importante contar con espacios de estacionamiento?

hotel_data <- read.csv("hotel_bookings_miss_pre.csv")

hotel_data.tr <- hotel_data[hotel_data
	$required_car_parking_spaces==1,]
hotel_data.tr <- hotel_data.tr[
	is.na(hotel_data.tr$required_car_parking_spaces) == 0,]

hotel_data.fl <- hotel_data[
	hotel_data$required_car_parking_spaces==0,]
hotel_data.fl <- hotel_data.fl[
	is.na(hotel_data.fl$required_car_parking_spaces) == 0,]

n_tr <- nrow(hotel_data.tr)
n_fl <- nrow(hotel_data.fl)

colors <- c("green", "gray")
labels <- c("Important", "No Important")
values <- c(n_tr, n_fl)
etiquetas <- paste0(labels, " ", values)

pie(values, labels = etiquetas, col = colors,
main="Reservas con importaciona de espacios de estacionamiento")