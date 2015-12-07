library(dplyr)
library(ggplot2)

setwd("~/Downloads/")

flights <- read.csv("2008.csv") %>% tbl_df()

flights

glimpse(flights)

cancellations_month = flights %>% group_by(Month, UniqueCarrier) %>% 
  summarize(sum_canc = sum(Cancelled)) %>% arrange( desc(UniqueCarrier)) %>% 

ggplot(cancellations_month) + geom_line(aes(x=Month, y = sum_canc, color = UniqueCarrier)) + 
  scale_x_continuous(breaks=1:12)