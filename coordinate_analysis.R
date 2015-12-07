library(dplyr)
library(ggplot2)
library(data.table)
library(readr)

flights <- read_csv("2008.csv")

# Airport data from OpenFlights.org
airports <- read.csv('airport_data/airports.dat', header=FALSE, sep=",")
colnames = c("Airport_ID","Name","City","Country","IATA","ICAO","Latitude","Longitude","Altitude","Timezone","DST","Tz_database_time_zone")

# Labels the columns where they're coming from
airports.origin = airports
airports.dest = airports
colnames(airports.origin) = paste('Origin', colnames, sep=".")
colnames(airports.dest) = paste('Dest', colnames, sep=".")
col.index = c(2,3,4,5,7,8)

# Change IATA codes to characters
airports.origin$Origin.IATA = as.character(levels(airports.origin$Origin.IATA))[airports.origin$Origin.IATA]
airports.dest$Dest.IATA = as.character(levels(airports.dest$Dest.IATA))[airports.dest$Dest.IATA]

# Left join using the IATA codes
flights = dplyr::left_join(flights, airports.origin[,col.index], by = c("Origin" = "Origin.IATA"))
flights = dplyr::left_join(flights, airports.dest[,col.index], by = c("Dest" = "Dest.IATA"))

# Created MeanDelay variable
flights = cbind(flights, (flights$ArrDelay + flights$DepDelay)/2)
names(flights)[ncol(flights)] <- "MeanDelay"

# Create factor for delay
DelayYesNo = ifelse(flights$MeanDelay>20, "Yes", "No")
flights = cbind(flights, DelayYesNo)

# Split into training/testing sets
set.size = 100000
num.flights = nrow(flights)
index.train.test = sample(num.flights, set.size)
index.train = index.train.test[1:(set.size/2)]
index.test = index.train.test[(set.size/2+1):set.size]

flights.train = flights[index.train,]
flights.train = flights.train[!is.na(flights.train$DelayYesNo),]
flights.test = flights[index.test,]
flights.test = flights.test[!is.na(flights.test$DelayYesNo),]

##############################
# AdaBoost
##############################
# library(adabag)
# delay.model <- boosting(DelayYesNo ~ Month + DayofMonth + DepTime + Distance + DayOfWeek + Origin.Latitude + Origin.Longitude
#                         + Dest.Longitude + Dest.Latitude, data=flights.train, boos=TRUE, mfinal=50)
# 
# delay.pred <- predict.bagging(delay.model, newdata=flights.test)
# sum(delay.pred$class == flights.test$DelayYesNo) / length(flights.test$DelayYesNo)

##############################
# Decision trees
##############################
# library(rpart)
# delay.model.2 <- rpart(DelayYesNo ~ Month + DayofMonth + DepTime + Distance + DayOfWeek + Origin.Latitude + Origin.Longitude
#                        + Dest.Longitude + Dest.Latitude, data=flights.train, method="class")
# delay.pred.2 <- predict(delay.model.2, newdata=flights.test, type="class")
# sum(delay.pred.2 == flights.test$DelayYesNo) / length(flights.test$DelayYesNo)

# library(maps)
# mp <- NULL
# mapWorld <- borders("usa", colour="gray50", fill="gray50") # create a layer of borders
# mp <- ggplot() +   mapWorld
# mp <- mp+ geom_point(aes(x=flights$Origin.Longitude[1:500000], y=flights$Origin.Latitude[1:500000]) ,color=flights$MeanDelay[1:500000], size=3) 
# mp

##############################
# Name	Description
#1	Year	1987-2008
#2	Month	1-12
#3	DayofMonth	1-31
#4	DayOfWeek	1 (Monday) - 7 (Sunday)
#5	DepTime	actual departure time (local, hhmm)
#6	CRSDepTime	scheduled departure time (local, hhmm)
#7	ArrTime	actual arrival time (local, hhmm)
#8	CRSArrTime	scheduled arrival time (local, hhmm)
#9	UniqueCarrier	unique carrier code
#10	FlightNum	flight number
#11	TailNum	plane tail number
#12	ActualElapsedTime	in minutes
#13	CRSElapsedTime	in minutes
#14	AirTime	in minutes
#15	ArrDelay	arrival delay, in minutes
#16	DepDelay	departure delay, in minutes
#17	Origin	origin IATA airport code
#18	Dest	destination IATA airport code
#19	Distance	in miles
#20	TaxiIn	taxi in time, in minutes
#21	TaxiOut	taxi out time in minutes
#22	Cancelled	was the flight cancelled?
#23	CancellationCode	reason for cancellation (A = carrier, B = weather, C = NAS, D = security)
#24	Diverted	1 = yes, 0 = no
#25	CarrierDelay	in minutes
#26	WeatherDelay	in minutes
#27	NASDelay	in minutes
#28	SecurityDelay	in minutes
#29	LateAircraftDelay	in minutes