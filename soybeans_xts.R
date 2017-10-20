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
#
df_xts5$orh_bo <- FALSE
df_xts5$orh_bo <- ifelse((df_xts5$daily_cummax_price - breakout) > df_open4$opnRng_cummax_price, TRUE, FALSE)
#merge df_open4 and df_xts5
#assign all the missing values in df_xts5 the last value from df_open4$opnRng_cummax_price
df_merged <- merge(df_open4, df_xts5)
#find tidyquant or whatever package fun to select out unwanted/duplicate col's.
#df_merged1 <- df_merged %>%
#  select(-price.1, -volume.1)
#
#df_main <- df_xts["T09:45/T14:14"]  #main part of soybeans trading day; MARKET HOURS ONLY NOT INCLUDING OPENING MINUTES

##### ##### #####

orh <- apply.daily(df_open$price, max)  #saves opening range high in "orh"
orh_bo <- orh + breakout  #sets the daily high breakout level
orh_bo_index <- apply.daily(df_main, FUN = function(X) first(which(X %in% orh_bo)))  #gets index values of the breakout, relative to each day
orh_bo_matrix <- coredata(orh_bo_index)  #creates a matrix of the index breakout values

#####

ep_daily <- endpoints(df_main, on = "days")  #creates a daily index
daily_last_index <- ep_daily[-1]  #removes the "0" value at the beginning
daily_last_matrix <- t(t(daily_last_index))  #creates a matrix of last obs of day indexes
df_bo_indexes <- bind_cols(data.frame(orh_bo_matrix), data.frame(daily_last_matrix))  #creates a df of the indexes to use for the breakout

#find the breakout indexes for each day
df_bo_day1 <- df_main[df_bo_indexes[1,1]:df_bo_indexes[1,2]]  #this is right; this finds the breakout indexes of the first day

#this is what needs fixing:
#find breakout indexes for days 2:ndays(df_main)
i <- 2
for(i in 2:ndays(df_main)) {
  df_bo_all <- df_main[(df_bo_indexes[i-1,2]+df_bo_indexes[i,1]):df_bo_indexes[i,2]]
} 
#
#this doesn't work: window(df_main, start = df_main[2074,], end = df_main[52155,])
#this doesn't work: window(df_main, start = df_main[2074], end = df_main[52155])
#this doesn't work: window(df_main, start = df_main[df_bo_indexes[1,1], end = df_main[df_bo_indexes[1,2]]])
# get error messages: Error in which(in.index & all.indexes >= start) : 
#  dims [product 2] do not match the length of object [258363]
#In addition: Warning messages:
#1: In which(in.index & all.indexes >= start) :
#  Incompatible methods (">=.POSIXt", "Ops.xts") for ">="
#2: In all.indexes >= start :
#  longer object length is not a multiple of shorter object length 
