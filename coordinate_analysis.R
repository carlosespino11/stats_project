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
airports.dest$Origin.IATA = as.character(levels(airports.dest$Dest.IATA))[airports.dest$Dest.IATA]

# Left join using the IATA codes
flights = dplyr::left_join(flights, airports.origin[,col.index], by = c("Origin" = "Origin.IATA"))
flights = dplyr::left_join(flights, airports.dest[,col.index], by = c("Dest" = "Dest.IATA"))

# Created MeanDelay variable
flights = cbind(flights, (flights$ArrDelay + flights$DepDelay)/2)
names(flights)[ncol(flights)] <- "MeanDelay"

# Create factor for delay
DelayYesNo = ifelse(flights$MeanDelay>0, "Yes", "No")
flights = cbind(flights, DelayYesNo)


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
train <- sample(c(T,F),size=50000,prob=c(0.2,0.8),replace=T)

flights.train <- flights[train,]
flights.test <- flights[!train,]

library(rpart)
library(rpart.plot)
library(caret)


fit <- rpart(DelayYesNo~DayOfWeek+UniqueCarrier+Distance+Long_short+ Month + DayofMonth + DepTime + Origin.Latitude + Origin.Longitude
      + Dest.Longitude + Dest.Latitude, data=flights.train)

rpart.plot(fit)

pred <- predict(fit,newdata=flights.test, type="class")
act <- flights.test$DelayYesNo
confusionMatrix(pred,act)



