setwd("C:/github/comm_health")
library(dplyr)
library(purrr)
library(randomForest)

d <- read.csv("NSF Community Master Database_June16_2016.csv", skip=1) %>% tbl_df
d[d==-9999 & !is.na(d)] <- NA # change -9999 to NA
d[d==" -   " & !is.na(d)] <- NA # change bogus character strings to NA
d[d=="  " & !is.na(d)] <- NA
d <- keep(d, ~sum(!is.na(.x)) >= 30) # must have at least 30 observations in original 62-observation data set

d %>% map_chr(class) %>% unique # column classes present in source data frame
keep(d, is.integer) %>% map(max, na.rm=TRUE) %>% unlist %>% sort # max values in integer columns

int_and_bin <- function(x) is.integer(x) && all(x %in% c(0, 1, NA))
map_01_to_char <- function(x) c("absent", "present")[x+1]
int_15 <- function(x) is.integer(x) && all(x %in% c(0:15, NA)) && any(x > 1)

d <- map_if(d, int_and_bin, map_01_to_char) %>% map_if(int_15, as.factor)
d <- do.call(cbind.data.frame, d)

# Examine potential response variables
resp_cols <- match("studyear", names(d)):ncol(d)
#d.resp0 <- select(d, resp_cols)
d.resp0 <- select(d, Part_all, Att_lm, Att_f)
d.resp0 %>% map_int(~sum(!is.na(.x)))
d.resp0 %>% na.omit %>% cor
drop_cols <- setdiff(resp_cols, match("Att_lm", names(d)))

d <- select(d, -drop_cols, -Community) %>% filter(!is.na(Att_lm))
Att_lm <- d$Att_lm

set.seed(384)
d <- rfImpute(select(d, -Att_lm), Att_lm, iter=10, n.tree=500) # impute missing values
# d <- d[complete.cases(d),] # random forests in R doesn't like NAs or Infs, so I just locate the complete.cases

library('caret')

d2 <- keep(d, is.numeric) %>% cor
x <- sort(findCorrelation(d2, cutoff=0.9))
d <- d[, -c(x)]

saveRDS(d, "rf_prepped_data.rds")

