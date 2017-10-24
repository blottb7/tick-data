setwd("C:/Users/Ben/Desktop/Kibot_Data") #Set the working directory Asus
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
retracement <- sqrt(.618)

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
df_xts2 <- split(df_xts1, "days")  #split df into days
df_xts3 <- lapply(df_xts2, cummax)  #apply cummax fn to each day
df_xts4 <- do.call(rbind, df_xts3)  #bind the results together in a new df

df_xts3.1 <- lapply(df_xts2, cummin)  #apply cummin fn to each day
df_xts4.1 <- do.call(rbind, df_xts3.1)  #bind the results together in a new df

names(df_xts4) <- c("daily_cummax", "daily_cummax_volume")  #name new vars
names(df_xts4.1) <- c("daily_cummin", "daily_cummin_volume")  #name new vars

df_xts4$daily_cummax_volume <- NULL  #remove unwanted cum-vol col's
df_xts4.1$daily_cummin_volume <- NULL

df_xts5 <- merge(df_xts1, df_xts4, df_xts4.1)  #merge the two new df's with the original df
#df_xts5$retracement_78.6 <- df_xts5[,"daily_cummax"] - retracement * (df_xts5[,"daily_cummax"] - df_xts5[,"daily_cummin"])
df_xts5$retracement_78.6 <- round(4 * (df_xts5[,"daily_cummax"] - retracement * (df_xts5[,"daily_cummax"] - df_xts5[,"daily_cummin"]))) / 4

##### ##### #####
df_open <- df_xts["T09:30/T09:44"]  #subsets opening range interval; OPENINGS MINUTES ONLY DF
df_open1 <- split(df_open, "days")  #split opening range by days
df_open2 <- lapply(df_open1, cummax)  #apply cummax fn to each day
df_open3 <- do.call(rbind, df_open2)  #bind each day together
names(df_open3) <- c("OR_daily_cummax", "OR_daily_cummax_volume")  #name new vars
df_open3$OR_daily_cummax_volume <- NULL  #remove cummax volume
df_open4 <- merge(df_open, df_open3)  #merge new df to original opening range df_xts

df_merged <- merge(df_xts5, df_open4)  #merge entire day w new vars to opening df
df_merged$OR_daily_cummax <- na.locf(df_merged$OR_daily_cummax)  #for all OR_daily_cummax == NA, assign the last value

df_merged$price.1 <- NULL  #remove duplicate columns
df_merged$volume.1 <- NULL  #remove duplicate columns

df_merged$orh_bo <- FALSE  #initializes conditional breakout value
df_merged$orh_bo <- ifelse((df_merged$daily_cummax - breakout) > df_merged$OR_daily_cummax, TRUE, FALSE)  #assigns 1 to orh_bo once mkt has "broken out"
df_breakout <- df_merged[which(df_merged$orh_bo == 1)]  #subsets data by "breakout is True"
