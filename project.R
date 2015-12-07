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

