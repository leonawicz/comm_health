setwd("C:/github/comm_health")
library(dplyr)
library(purrr)
library(randomForest)

resp_var <- rep(c("Harv_all", "Harv_f", "Harv_lm", "Harv_m", "Give_all", "Give_f", "Give_lm", "Give_m"), 2)
varname <- gsub("_", "", resp_var)
cor_cap <- rep(c("cor60", "cor70"), each=length(resp_var)/2)

for(i in seq_along(varname)){

#d <- read.csv("NSF Community Master Database_June16_2016.csv", skip=1) %>% tbl_df
d <- read.csv(paste0("data09112016_", cor_cap[i], ".csv")) %>% tbl_df
d[d==-9999 & !is.na(d)] <- NA # change -9999 to NA
d[d==" -   " & !is.na(d)] <- NA # change bogus character strings to NA
d[d=="  " & !is.na(d)] <- NA
#d <- keep(d, ~sum(!is.na(.x)) >= 30) # must have at least 30 observations in original 62-observation data set

d <- filter(d, Rural==1)

d %>% map_chr(class) %>% unique # column classes present in source data frame
keep(d, is.integer) %>% map(max, na.rm=TRUE) %>% unlist %>% sort # max values in integer columns

int_and_bin <- function(x) is.integer(x) && all(x %in% c(0, 1, NA))
map_01_to_char <- function(x) c("absent", "present")[x+1]
int_15 <- function(x) is.integer(x) && all(x %in% c(0:5, NA)) && any(x > 1)

d <- map_if(d, int_and_bin, map_01_to_char) %>% map_if(int_15, as.factor)
d <- do.call(cbind.data.frame, d)

# Examine potential response variables
resp_cols <- match("Harv_all", names(d)):ncol(d)
#d.resp0 <- select(d, resp_cols)
d.resp0 <- select(d, Harv_all, Harv_lm, Harv_f, Harv_m) # select(d, Part_all, Att_lm, Att_f)
d.resp0 %>% map_int(~sum(!is.na(.x)))
d.resp0 %>% na.omit %>% cor
#drop_cols <- setdiff(resp_cols, match(resp_var, names(d)))
d$yVar <- d[[resp_var[i]]]
drop_cols <- resp_cols
d <- select(d, -drop_cols, -Community) %>% filter(!is.na(yVar))
yVar <- d$yVar

set.seed(384)
d <- rfImpute(select(d, -yVar), yVar, iter=10, n.tree=500) # impute missing values
# d <- d[complete.cases(d),] # random forests in R doesn't like NAs or Infs, so I just locate the complete.cases

#library('caret')
#d2 <- keep(d, is.numeric) %>% cor
#x <- sort(findCorrelation(d2, cutoff=0.9))
#d <- d[, -c(x)]

save(d, yVar, file=paste0("rf_prepped_data09112016_", cor_cap[i], "_rural_", varname[i], ".RData"))

}
