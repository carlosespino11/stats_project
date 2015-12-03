library(dplyr)

setwd("~/Downloads/")


flights <- read.csv("2008.csv") %>% tbl_df()

flights

glimpse(flights)

#delays_month = flights %>% group_by(month) %>% 