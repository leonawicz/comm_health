setwd("C:/github/comm_health")
library(dplyr)
library(purrr)
library(randomForest)

d <- read.csv("NSF Community Master Database_June16_2016.csv", skip=1) %>% tbl_df
d[d==-9999 & !is.na(d)] <- NA # change -9999 to NA

d %>% map_chr(class) %>% unique # column classes present in source data frame
keep(d, is.integer) %>% map(max, na.rm=TRUE) %>% unlist %>% sort # max values in integer columns

int_and_bin <- function(x) is.integer(x) && all(x %in% c(0, 1, NA))
map_01_to_char <- function(x) c("absent", "present")[x+1]
int_15 <- function(x) is.integer(x) && all(x %in% c(0:15, NA)) && any(x > 1)

d <- map_if(d, int_and_bin, map_01_to_char) %>% map_if(int_15, as.factor)
d <- do.call(cbind.data.frame, d)
d <- na.roughfix(d) # impute missing values
# d <- d[complete.cases(d),] # random forests in R doesn't like NAs or Infs, so I just locate the complete.cases
