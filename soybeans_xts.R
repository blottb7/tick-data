setwd("~/Desktop/R_projects/soybeans")

#Libraries
library(tidyr)
library(dplyr)
library(xts)
library(lubridate)
library(timetk)
library(tidyquant)
library(tidyverse)

df <- read.csv("soybeans_sample_1week.csv")  #reads in a one week sample of soybeans tick data (June 1-7, 2016)

#Global vars
breakout <- 2.00  #set the breakout distance to 2.00

#convert .csv to xts
df1 <- df %>%
  select(-X)  #removes "X" col

df1$datetime <- as.POSIXct(paste(df1$date, df1$time), format = "%m/%d/%Y %H:%M:%S")  #combines date and time cols

df2 <- df1 %>%
  select(datetime, price, volume = size) %>%  #removes individual data and time cols
  arrange(datetime)  #arranges by datetime

df_xts <- as.xts(df2[,-1], order.by = df2$datetime)  #convert to xts class
df_xts <- make.index.unique(df_xts)  #makes each index unique; very important
df_xts1 <- df_xts["T09:30/T14:14"]  #subset to market hours only; MARKET HOURS ONLY DF

##### ##### #####
df_xts2 <- split(df_xts1, "days")
df_xts3 <- lapply(df_xts2, cummax)
df_xts4 <- do.call(rbind, df_xts3)
names(df_xts4) <- c("daily_cummax_price", "daily_cummax_volume")
df_xts5 <- merge(df_xts1, df_xts4)
#
df_open <- df_xts["T09:30/T09:44"]  #subsets opening range interval; OPENINGS MINUTES ONLY DF
df_open1 <- split(df_open, "days")
df_open2 <- lapply(df_open1, cummax)
df_open3 <- do.call(rbind, df_open2)
names(df_open3) <- c("opnRng_cummax_price", "opnRng_cummax_volume")
df_open4 <- merge(df_open, df_open3)

df_merged <- merge(df_open4, df_xts5)  #merge df_open4 and df_xts5; other args for merge: join = c("price", "volume"), fill = "last"
#for all opnRng_cummax_price == NA, assign the last value
df_merged$opnRng_cummax_price <- na.locf(df_merged$opnRng_cummax_price)

df_merged$orh_bo <- FALSE  #initializes conditional breakout value
df_merged$orh_bo <- ifelse((df_merged$daily_cummax_price - breakout) > df_merged$opnRng_cummax_price, TRUE, FALSE)
df_breakout <- df_merged[which(df_merged$orh_bo == 1)]  #subsets data by "breakout is True".

#NOTES:
# merge() alternatives?
#df_merged1 <- rbind(df_open4, df_xts5)  #this seems cleaner and faster, but drops cols from df_xts5
#df_merged1 <- rbind(df_xts4, df_open4)  #this doesn't work

#TASK: find tidyquant or whatever package fun to select out unwanted/duplicate col's.
#df_merged1 <- df_merged %>%
#  select(-price.1, -volume.1)  this doesn't work

##### ##### #####
#NEXT:
    #write next plan for code including retracement values in df_xts1.
