# a. ¿Cuántas reservas se realizan por tipo de hotel?
# o ¿Qué tipo de hotel prefiere la gente?

library(dplyr)

hotel_data <- read.csv("hotel_bookings_miss_pre.csv")

hotel_table <- table(hotel_data$hotel);

print(hotel_table)

hotel <- barplot(hotel_table,
	main="Tipos de hotel", col=rainbow(2))
