library(dplyr)
library(ggplot2)
library(data.table)
library(readr)

flights <- read_csv("2008.csv")

# Airport data
airports <- read.csv('airport_data/airports_data.csv', header=FALSE, sep=",")
colnames(airports) <- c("Airport_ID","Name","City","Country","IATA","ICAO","Latitude","Longitude","Altitude","Timezone","DST","Tz_database_time_zone")

flights

glimpse(flights)

cancellations_by_month = flights %>% group_by(Month) %>% 
  summarize(sum_canc = sum(Cancelled), flights_n = n()) %>% mutate(cancelled_ratio = sum_canc/flights_n) 

ggplot(cancellations_by_month) + geom_line(aes(x=Month, y = sum_canc)) + 
  scale_x_continuous(breaks=1:12) + ylab("Cancellations")


origins = flights %>% group_by(Origin) %>% summarise(count = n())

## MAIL FROM MOTTA

#chapters from the book:
#The final project is due on December 15 before 7:40pm. 
#Please e-mail your project directly to the TA.
#Your paper should cover at least one application in R from each of the following 3 chapters: 
#9 & 10 from IMS, and 7 from ISL.
#You can definitely apply other methods as well (logistic regression, bootstrap, etc).
#If you have questions about R, please feel free to e-mail the TA or attend his office hours. 
#Number pages: min 15 max 20. Format for submission: pdf or word. 
#Number of students per group: min 3 max 4. E-mail (as soon as possible) me if
#you are not yet part of a team.

## DATASET DESCRIPTION 
#http://stat-computing.org/dataexpo/2009/the-data.html

#Name	Description
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

#chap 7 islr
# 7.1 Polynomial Regression
# 7.2 Step Functions
# 7.3 Basis Functions
# 7.4 Regression Splines
# 7.5 Smoothing Splines
# 7.6 Local Regression
# 7.7 Generalized Additive Models

#chap 8-9
#8 Optimal Test of Hypotheses
#9 Inferences About Normal Models


