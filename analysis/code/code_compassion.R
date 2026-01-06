library(ggplot2)
library(tidyr)
library(psych)
library(readr)
library(haven)
library(strengejacke)
library(dplyr)
library(stringr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(lubridate)

# import data files 'df_wide' / 'df_long_ema' / 'df_long_training'
# available at https://github.com/cwlab23/compassion/

## df_wide <- read_csv("designated pathway")
## df_long_ema <- read_csv("designated pathway")
## df_long_training <- read_csv("designated pathway")

#############
# data prep #
#############

#create composite scores for df_wide (baseline labeled .x ; endpoint labeled .y)

#PWB-18 - reverse 1, 2, 3, 8, 9, 11, 12, 13, 17, 18

df_wide <- mutate(df_wide, pwb_1r.x= case_when(
  pwb_1.x == 1  ~ 7, 
  pwb_1.x == 2  ~ 6, 
  pwb_1.x == 3  ~ 5, 
  pwb_1.x == 4  ~ 4, 
  pwb_1.x == 5  ~ 3,
  pwb_1.x == 6  ~ 2,
  pwb_1.x == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_2r.x= case_when(
  pwb_2.x == 1  ~ 7, 
  pwb_2.x == 2  ~ 6, 
  pwb_2.x == 3  ~ 5, 
  pwb_2.x == 4  ~ 4, 
  pwb_2.x == 5  ~ 3,
  pwb_2.x == 6  ~ 2,
  pwb_2.x == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_3r.x= case_when(
  pwb_3.x == 1  ~ 7, 
  pwb_3.x == 2  ~ 6, 
  pwb_3.x == 3  ~ 5, 
  pwb_3.x == 4  ~ 4, 
  pwb_3.x == 5  ~ 3,
  pwb_3.x == 6  ~ 2,
  pwb_3.x == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_8r.x= case_when(
  pwb_8.x == 1  ~ 7, 
  pwb_8.x == 2  ~ 6, 
  pwb_8.x == 3  ~ 5, 
  pwb_8.x == 4  ~ 4, 
  pwb_8.x == 5  ~ 3,
  pwb_8.x == 6  ~ 2,
  pwb_8.x == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_9r.x= case_when(
  pwb_9.x == 1  ~ 7, 
  pwb_9.x == 2  ~ 6, 
  pwb_9.x == 3  ~ 5, 
  pwb_9.x == 4  ~ 4, 
  pwb_9.x == 5  ~ 3,
  pwb_9.x == 6  ~ 2,
  pwb_9.x == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_11r.x= case_when(
  pwb_11.x == 1  ~ 7, 
  pwb_11.x == 2  ~ 6, 
  pwb_11.x == 3  ~ 5, 
  pwb_11.x == 4  ~ 4, 
  pwb_11.x == 5  ~ 3,
  pwb_11.x == 6  ~ 2,
  pwb_11.x == 7  ~ 1,
))


df_wide <- mutate(df_wide, pwb_12r.x= case_when(
  pwb_12.x == 1  ~ 7, 
  pwb_12.x == 2  ~ 6, 
  pwb_12.x == 3  ~ 5, 
  pwb_12.x == 4  ~ 4, 
  pwb_12.x == 5  ~ 3,
  pwb_12.x == 6  ~ 2,
  pwb_12.x == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_13r.x= case_when(
  pwb_13.x == 1  ~ 7, 
  pwb_13.x == 2  ~ 6, 
  pwb_13.x == 3  ~ 5, 
  pwb_13.x == 4  ~ 4, 
  pwb_13.x == 5  ~ 3,
  pwb_13.x == 6  ~ 2,
  pwb_13.x == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_17r.x= case_when(
  pwb_17.x == 1  ~ 7, 
  pwb_17.x == 2  ~ 6, 
  pwb_17.x == 3  ~ 5, 
  pwb_17.x == 4  ~ 4, 
  pwb_17.x == 5  ~ 3,
  pwb_17.x == 6  ~ 2,
  pwb_17.x == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_18r.x= case_when(
  pwb_18.x == 1  ~ 7, 
  pwb_18.x == 2  ~ 6, 
  pwb_18.x == 3  ~ 5, 
  pwb_18.x == 4  ~ 4, 
  pwb_18.x == 5  ~ 3,
  pwb_18.x == 6  ~ 2,
  pwb_18.x == 7  ~ 1,
))

df_wide$pwb.pre = rowMeans(df_wide[,c("pwb_1r.x", "pwb_2r.x", "pwb_3r.x",
                                        "pwb_4.x", "pwb_5.x", "pwb_6.x",
                                        "pwb_7.x", "pwb_8r.x", "pwb_9r.x",
                                        "pwb_10.x", "pwb_11r.x", "pwb_12r.x",
                                        "pwb_13r.x", "pwb_14.x", "pwb_15.x",
                                        "pwb_16.x", "pwb_17r.x", "pwb_18r.x")] %>%
                              mutate_all(as.numeric), na.rm = TRUE)

#CESD

df_wide <- mutate(df_wide, cesd_5r.x= case_when(
  cesd_5.x == 1  ~ 4, 
  cesd_5.x == 2  ~ 3, 
  cesd_5.x == 3  ~ 2, 
  cesd_5.x == 4  ~ 1,
))

df_wide <- mutate(df_wide, cesd_8r.x= case_when(
  cesd_8.x == 1  ~ 4, 
  cesd_8.x == 2  ~ 3, 
  cesd_8.x == 3  ~ 2, 
  cesd_8.x == 4  ~ 1,
))

df_wide$cesd.pre = rowMeans(df_wide[,c("cesd_1.x", "cesd_2.x","cesd_3.x", "cesd_4.x",
                                         "cesd_5r.x", "cesd_6.x", "cesd_7.x", "cesd_8r.x",
                                         "cesd_9.x", "cesd_10.x")] %>%
                               mutate_all(as.numeric), na.rm = TRUE)

#STAI - reverse 1, 2, 5, 8, 11, 15, 16, 19, 20

df_wide <- mutate(df_wide, stai_1r.x= case_when(
  stai_1.x == 1  ~ 4, 
  stai_1.x == 2  ~ 3, 
  stai_1.x == 3  ~ 2, 
  stai_1.x == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_2r.x= case_when(
  stai_2.x == 1  ~ 4, 
  stai_2.x == 2  ~ 3, 
  stai_2.x == 3  ~ 2, 
  stai_2.x == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_5r.x= case_when(
  stai_5.x == 1  ~ 4, 
  stai_5.x == 2  ~ 3, 
  stai_5.x == 3  ~ 2, 
  stai_5.x == 4  ~ 1,
))


df_wide <- mutate(df_wide, stai_8r.x= case_when(
  stai_8.x == 1  ~ 4, 
  stai_8.x == 2  ~ 3, 
  stai_8.x == 3  ~ 2, 
  stai_8.x == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_11r.x= case_when(
  stai_11.x == 1  ~ 4, 
  stai_11.x == 2  ~ 3, 
  stai_11.x == 3  ~ 2, 
  stai_11.x == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_15r.x= case_when(
  stai_15.x == 1  ~ 4, 
  stai_15.x == 2  ~ 3, 
  stai_15.x == 3  ~ 2, 
  stai_15.x == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_16r.x= case_when(
  stai_16.x == 1  ~ 4, 
  stai_16.x == 2  ~ 3, 
  stai_16.x == 3  ~ 2, 
  stai_16.x == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_19r.x= case_when(
  stai_19.x == 1  ~ 4, 
  stai_19.x == 2  ~ 3, 
  stai_19.x == 3  ~ 2, 
  stai_19.x == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_20r.x= case_when(
  stai_20.x == 1  ~ 4, 
  stai_20.x == 2  ~ 3, 
  stai_20.x == 3  ~ 2, 
  stai_20.x == 4  ~ 1,
))

df_wide$stai.pre = rowMeans(df_wide[,c("stai_1r.x", "stai_2r.x","stai_3.x", "stai_4.x",
                                         "stai_5r.x", "stai_6.x", "stai_7.x", "stai_8r.x",
                                         "stai_9.x", "stai_10.x", "stai_11r.x", "stai_12.x",
                                         "stai_13.x", "stai_14.x", "stai_15r.x", "stai_16r.x",
                                         "stai_17.x", "stai_18.x", "stai_19r.x", "stai_20r.x")]%>%
                               mutate_all(as.numeric), na.rm = TRUE)

#PSS

df_wide <- mutate(df_wide, pss_2r.x= case_when(
  pss_2.x == 1  ~ 5, 
  pss_2.x == 2  ~ 4, 
  pss_2.x == 3  ~ 3, 
  pss_2.x == 4  ~ 2,
  pss_2.x == 5  ~ 1,
))

df_wide <- mutate(df_wide, pss_3r.x= case_when(
  pss_3.x == 1  ~ 5, 
  pss_3.x == 2  ~ 4, 
  pss_3.x == 3  ~ 3, 
  pss_3.x == 4  ~ 2,
  pss_3.x == 5  ~ 1,
))

df_wide$pss.pre = rowMeans(df_wide[,c("pss_1.x", "pss_2r.x","pss_3r.x", "pss_4.x")]%>%
                              mutate_all(as.numeric), na.rm = TRUE)

#social support

df_wide$support.pre = rowMeans(df_wide[,c("esupport_1.x", "esupport_2.x", "esupport_3.x",
                                            "esupport_4.x", "esupport_5.x", "esupport_7.x",
                                            "esupport_8.x", "esupport_9.x", "esupport_10.x",
                                            "esupport_11.x", "esupport_12.x",
                                            "isupport_1.x", "isupport_2.x", "isupport_3.x", "isupport_4.x",
                                            "isupport_5.x", "isupport_6.x", "isupport_7.x", "isupport_8.x", "isupport_9.x")]%>%
                                  mutate_all(as.numeric), na.rm = TRUE)

#MDES

df_wide$mdes_pos.pre = rowMeans(df_wide[,c("mdes_1.x", "mdes_4.x","mdes_8.x", "mdes_11.x",
                                             "mdes_12.x", "mdes_13.x", "mdes_14.x", "mdes_15.x",
                                             "mdes_16.x", "mdes_19.x")]%>%
                                   mutate_all(as.numeric), na.rm = TRUE)

df_wide$mdes_neg.pre = rowMeans(df_wide[,c("mdes_2.x", "mdes_3.x","mdes_5.x", "mdes_6.x",
                                             "mdes_7.x", "mdes_9.x", "mdes_10.x", "mdes_17.x",
                                             "mdes_18.x", "mdes_20.x")]%>%
                                   mutate_all(as.numeric), na.rm = TRUE)


#repeat code for endpoint (.y)
#PWB-18 - reverse 1, 2, 3, 8, 9, 11, 12, 13, 17, 18

df_wide <- mutate(df_wide, pwb_1r.y= case_when(
  pwb_1.y == 1  ~ 7, 
  pwb_1.y == 2  ~ 6, 
  pwb_1.y == 3  ~ 5, 
  pwb_1.y == 4  ~ 4, 
  pwb_1.y == 5  ~ 3,
  pwb_1.y == 6  ~ 2,
  pwb_1.y == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_2r.y= case_when(
  pwb_2.y == 1  ~ 7, 
  pwb_2.y == 2  ~ 6, 
  pwb_2.y == 3  ~ 5, 
  pwb_2.y == 4  ~ 4, 
  pwb_2.y == 5  ~ 3,
  pwb_2.y == 6  ~ 2,
  pwb_2.y == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_3r.y= case_when(
  pwb_3.y == 1  ~ 7, 
  pwb_3.y == 2  ~ 6, 
  pwb_3.y == 3  ~ 5, 
  pwb_3.y == 4  ~ 4, 
  pwb_3.y == 5  ~ 3,
  pwb_3.y == 6  ~ 2,
  pwb_3.y == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_8r.y= case_when(
  pwb_8.y == 1  ~ 7, 
  pwb_8.y == 2  ~ 6, 
  pwb_8.y == 3  ~ 5, 
  pwb_8.y == 4  ~ 4, 
  pwb_8.y == 5  ~ 3,
  pwb_8.y == 6  ~ 2,
  pwb_8.y == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_9r.y= case_when(
  pwb_9.y == 1  ~ 7, 
  pwb_9.y == 2  ~ 6, 
  pwb_9.y == 3  ~ 5, 
  pwb_9.y == 4  ~ 4, 
  pwb_9.y == 5  ~ 3,
  pwb_9.y == 6  ~ 2,
  pwb_9.y == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_11r.y= case_when(
  pwb_11.y == 1  ~ 7, 
  pwb_11.y == 2  ~ 6, 
  pwb_11.y == 3  ~ 5, 
  pwb_11.y == 4  ~ 4, 
  pwb_11.y == 5  ~ 3,
  pwb_11.y == 6  ~ 2,
  pwb_11.y == 7  ~ 1,
))


df_wide <- mutate(df_wide, pwb_12r.y= case_when(
  pwb_12.y == 1  ~ 7, 
  pwb_12.y == 2  ~ 6, 
  pwb_12.y == 3  ~ 5, 
  pwb_12.y == 4  ~ 4, 
  pwb_12.y == 5  ~ 3,
  pwb_12.y == 6  ~ 2,
  pwb_12.y == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_13r.y= case_when(
  pwb_13.y == 1  ~ 7, 
  pwb_13.y == 2  ~ 6, 
  pwb_13.y == 3  ~ 5, 
  pwb_13.y == 4  ~ 4, 
  pwb_13.y == 5  ~ 3,
  pwb_13.y == 6  ~ 2,
  pwb_13.y == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_17r.y= case_when(
  pwb_17.y == 1  ~ 7, 
  pwb_17.y == 2  ~ 6, 
  pwb_17.y == 3  ~ 5, 
  pwb_17.y == 4  ~ 4, 
  pwb_17.y == 5  ~ 3,
  pwb_17.y == 6  ~ 2,
  pwb_17.y == 7  ~ 1,
))

df_wide <- mutate(df_wide, pwb_18r.y= case_when(
  pwb_18.y == 1  ~ 7, 
  pwb_18.y == 2  ~ 6, 
  pwb_18.y == 3  ~ 5, 
  pwb_18.y == 4  ~ 4, 
  pwb_18.y == 5  ~ 3,
  pwb_18.y == 6  ~ 2,
  pwb_18.y == 7  ~ 1,
))

df_wide$pwb.post = rowMeans(df_wide[,c("pwb_1r.y", "pwb_2r.y", "pwb_3r.y",
                                         "pwb_4.y", "pwb_5.y", "pwb_6.y",
                                         "pwb_7.y", "pwb_8r.y", "pwb_9r.y",
                                         "pwb_10.y", "pwb_11r.y", "pwb_12r.y",
                                         "pwb_13r.y", "pwb_14.y", "pwb_15.y",
                                         "pwb_16.y", "pwb_17r.y", "pwb_18r.y")] %>%
                               mutate_all(as.numeric), na.rm = TRUE)

#CESD

df_wide <- mutate(df_wide, cesd_5r.y= case_when(
  cesd_5.y == 1  ~ 4, 
  cesd_5.y == 2  ~ 3, 
  cesd_5.y == 3  ~ 2, 
  cesd_5.y == 4  ~ 1,
))

df_wide <- mutate(df_wide, cesd_8r.y= case_when(
  cesd_8.y == 1  ~ 4, 
  cesd_8.y == 2  ~ 3, 
  cesd_8.y == 3  ~ 2, 
  cesd_8.y == 4  ~ 1,
))

df_wide$cesd.post = rowMeans(df_wide[,c("cesd_1.y", "cesd_2.y","cesd_3.y", "cesd_4.y",
                                          "cesd_5r.y", "cesd_6.y", "cesd_7.y", "cesd_8r.y",
                                          "cesd_9.y", "cesd_10.y")] %>%
                                mutate_all(as.numeric), na.rm = TRUE)

#STAI - reverse 1, 2, 5, 8, 11, 15, 16, 19, 20

df_wide <- mutate(df_wide, stai_1r.y= case_when(
  stai_1.y == 1  ~ 4, 
  stai_1.y == 2  ~ 3, 
  stai_1.y == 3  ~ 2, 
  stai_1.y == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_2r.y= case_when(
  stai_2.y == 1  ~ 4, 
  stai_2.y == 2  ~ 3, 
  stai_2.y == 3  ~ 2, 
  stai_2.y == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_5r.y= case_when(
  stai_5.y == 1  ~ 4, 
  stai_5.y == 2  ~ 3, 
  stai_5.y == 3  ~ 2, 
  stai_5.y == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_8r.y= case_when(
  stai_8.y == 1  ~ 4, 
  stai_8.y == 2  ~ 3, 
  stai_8.y == 3  ~ 2, 
  stai_8.y == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_11r.y= case_when(
  stai_11.y == 1  ~ 4, 
  stai_11.y == 2  ~ 3, 
  stai_11.y == 3  ~ 2, 
  stai_11.y == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_15r.y= case_when(
  stai_15.y == 1  ~ 4, 
  stai_15.y == 2  ~ 3, 
  stai_15.y == 3  ~ 2, 
  stai_15.y == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_16r.y= case_when(
  stai_16.y == 1  ~ 4, 
  stai_16.y == 2  ~ 3, 
  stai_16.y == 3  ~ 2, 
  stai_16.y == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_19r.y= case_when(
  stai_19.y == 1  ~ 4, 
  stai_19.y == 2  ~ 3, 
  stai_19.y == 3  ~ 2, 
  stai_19.y == 4  ~ 1,
))

df_wide <- mutate(df_wide, stai_20r.y= case_when(
  stai_20.y == 1  ~ 4, 
  stai_20.y == 2  ~ 3, 
  stai_20.y == 3  ~ 2, 
  stai_20.y == 4  ~ 1,
))

df_wide$stai.post = rowMeans(df_wide[,c("stai_1r.y", "stai_2r.y","stai_3.y", "stai_4.y",
                                          "stai_5r.y", "stai_6.y", "stai_7.y", "stai_8r.y",
                                          "stai_9.y", "stai_10.y", "stai_11r.y", "stai_12.y",
                                          "stai_13.y", "stai_14.y", "stai_15r.y", "stai_16r.y",
                                          "stai_17.y", "stai_18.y", "stai_19r.y", "stai_20r.y")]%>%
                                mutate_all(as.numeric), na.rm = TRUE)


#PSS

df_wide <- mutate(df_wide, pss_2r.y= case_when(
  pss_2.y == 1  ~ 5, 
  pss_2.y == 2  ~ 4, 
  pss_2.y == 3  ~ 3, 
  pss_2.y == 4  ~ 2,
  pss_2.y == 5  ~ 1,
))

df_wide <- mutate(df_wide, pss_3r.y= case_when(
  pss_3.y == 1  ~ 5, 
  pss_3.y == 2  ~ 4, 
  pss_3.y == 3  ~ 3, 
  pss_3.y == 4  ~ 2,
  pss_3.y == 5  ~ 1,
))

df_wide$pss.post = rowMeans(df_wide[,c("pss_1.y", "pss_2r.y","pss_3r.y", "pss_4.y")]%>%
                               mutate_all(as.numeric), na.rm = TRUE)

#support

df_wide$support.post = rowMeans(df_wide[,c("esupport_1.y", "esupport_2.y", "esupport_3.y",
                                             "esupport_4.y", "esupport_5.y", "esupport_7.y",
                                             "esupport_8.y", "esupport_9.y", "esupport_10.y",
                                             "esupport_11.y", "esupport_12.y",
                                             "isupport_1.y", "isupport_2.y", "isupport_3.y", "isupport_4.y",
                                             "isupport_5.y", "isupport_6.y", "isupport_7.y", "isupport_8.y", "isupport_9.y")]%>%
                                   mutate_all(as.numeric), na.rm = TRUE)

#MDES

df_wide$mdes_pos.post = rowMeans(df_wide[,c("mdes_1.y", "mdes_4.y","mdes_8.y", "mdes_11.y",
                                              "mdes_12.y", "mdes_13.y", "mdes_14.y", "mdes_15.y",
                                              "mdes_16.y", "mdes_19.y")]%>%
                                    mutate_all(as.numeric), na.rm = TRUE)

df_wide$mdes_neg.post = rowMeans(df_wide[,c("mdes_2.y", "mdes_3.y","mdes_5.y", "mdes_6.y",
                                              "mdes_7.y", "mdes_9.y", "mdes_10.y", "mdes_17.y",
                                              "mdes_18.y", "mdes_20.y")]%>%
                                    mutate_all(as.numeric), na.rm = TRUE)

# EMA variables

## day
df_long_ema$`Start Time` <- as.POSIXct(df_long_ema$`Start Time`, format = "%m/%d/%Y %H:%M")
start_date <- as.Date(min(df_long_ema$`Start Time`, na.rm = TRUE))
df_long_ema$day <- as.numeric(as.Date(df_long_ema$`Start Time`) - start_date) + 1

## sleep (only morning)
df_long_ema <- df_long_ema %>%
  rename(sleep.quality = `How would you rate your quality of sleep last night?`)
df_long_ema <- df_long_ema %>%
  rename(sleep.duration = `How many hours and minutes of sleep did you get last night? This may be different from the number of hours spent in bed.`)
df_long_ema <- df_long_ema %>%
  rename(sleep.efficiency = `How long did you spend awake at night? This includes the time it took to fall asleep and awakenings from sleep.`)

### sleep.duration / sleep.efficiency  in minutes
df_long_ema <- df_long_ema %>%
  mutate(sleep.duration_minutes = as.numeric(hms(sleep.duration)) / 60)
df_long_ema <- df_long_ema %>%
  mutate(sleep.efficiency_minutes = as.numeric(hms(sleep.efficiency)) / 60)

## mood
df_long_ema <- df_long_ema %>%
  rename(mood_positive = `How POSITIVE do you feel right now?`)
df_long_ema <- df_long_ema %>%
  rename(mood_negative = `How NEGATIVE do you feel right now?`)
df_long_ema <- df_long_ema %>%
  rename(mood_angry = `Right now I feel ANGRY.`)
df_long_ema <- df_long_ema %>%
  rename(mood_anxious = `Right now I feel ANXIOUS.`)
df_long_ema <- df_long_ema %>%
  rename(mood_happy = `Right now I feel HAPPY.`)
df_long_ema <- df_long_ema %>%
  rename(mood_lonely = `Right now I feel LONELY.`)
df_long_ema <- df_long_ema %>%
  rename(mood_peaceful = `Right now I feel PEACEFUL.`)
df_long_ema <- df_long_ema %>%
  rename(mood_sad = `Right now I feel SAD.`)
df_long_ema <- df_long_ema %>%
  rename(mood_well = `Right now I feel WELL.`)

## stress
df_long_ema <- df_long_ema %>%
  rename(stress1 = `Right now, I feel like difficulties are piling up so high I cannot overcome them.`)
df_long_ema <- df_long_ema %>%
  rename(stress2 = `Right now, I feel stressed.`)

#composite scores by morning / evening ema

ema_morning <- df_long_ema %>% filter(ema_type == "morning")
ema_evening <- df_long_ema %>% filter(ema_type == "evening")

ema_morning$morning.stress = rowMeans(ema_morning[,c("stress1", "stress2")] %>%
                                        mutate_all(as.numeric), na.rm = TRUE)
ema_evening$evening.stress = rowMeans(ema_evening[,c("stress1", "stress2")] %>%
                                        mutate_all(as.numeric), na.rm = TRUE)
ema_morning$morning.positive = rowMeans(ema_morning[,c("mood_positive", "mood_happy", "mood_peaceful", "mood_well")] %>%
                                          mutate_all(as.numeric), na.rm = TRUE)
ema_evening$evening.positive = rowMeans(ema_evening[,c("mood_positive", "mood_happy", "mood_peaceful", "mood_well")] %>%
                                          mutate_all(as.numeric), na.rm = TRUE)
ema_morning$morning.negative = rowMeans(ema_morning[,c("mood_negative", "mood_sad", "mood_anxious", "mood_angry", "mood_lonely")] %>%
                                          mutate_all(as.numeric), na.rm = TRUE)
ema_evening$evening.negative = rowMeans(ema_evening[,c("mood_negative", "mood_sad", "mood_anxious", "mood_angry", "mood_lonely")] %>%
                                          mutate_all(as.numeric), na.rm = TRUE)

df_long_ema <- bind_rows(ema_morning, ema_evening)

# internal consistency

baseline_pwb <- df_wide[ , c("pwb_1r.x", "pwb_2r.x", "pwb_3r.x",
                              "pwb_4.x", "pwb_5.x", "pwb_6.x",
                              "pwb_7.x", "pwb_8r.x", "pwb_9r.x",
                              "pwb_10.x", "pwb_11r.x", "pwb_12r.x",
                              "pwb_13r.x", "pwb_14.x", "pwb_15.x",
                              "pwb_16.x", "pwb_17r.x", "pwb_18r.x")]    
alpha(baseline_pwb)

endpoint_pwb <- df_wide[ , c("pwb_1r.y", "pwb_2r.y", "pwb_3r.y",
                              "pwb_4.y", "pwb_5.y", "pwb_6.y",
                              "pwb_7.y", "pwb_8r.y", "pwb_9r.y",
                              "pwb_10.y", "pwb_11r.y", "pwb_12r.y",
                              "pwb_13r.y", "pwb_14.y", "pwb_15.y",
                              "pwb_16.y", "pwb_17r.y", "pwb_18r.y")]    
alpha(endpoint_pwb)

baseline_cesd <- df_wide[ , c("cesd_1.x", "cesd_2.x","cesd_3.x", "cesd_4.x",
                               "cesd_5r.x", "cesd_6.x", "cesd_7.x", "cesd_8r.x",
                               "cesd_9.x", "cesd_10.x")]    
alpha(baseline_cesd)

endpoint_cesd <- df_wide[ , c("cesd_1.y", "cesd_2.y","cesd_3.y", "cesd_4.y",
                               "cesd_5r.y", "cesd_6.y", "cesd_7.y", "cesd_8r.y",
                               "cesd_9.y", "cesd_10.y")]    
alpha(endpoint_cesd)

baseline_stai <- df_wide[ , c("stai_1r.x", "stai_2r.x","stai_3.x", "stai_4.x",
                               "stai_5r.x", "stai_6.x", "stai_7.x", "stai_8r.x",
                               "stai_9.x", "stai_10.x", "stai_11r.x", "stai_12.x",
                               "stai_13.x", "stai_14.x", "stai_15r.x", "stai_16r.x",
                               "stai_17.x", "stai_18.x", "stai_19r.x", "stai_20r.x")]    
alpha(baseline_stai)

endpoint_stai <- df_wide[ , c("stai_1r.y", "stai_2r.y","stai_3.y", "stai_4.y",
                               "stai_5r.y", "stai_6.y", "stai_7.y", "stai_8r.y",
                               "stai_9.y", "stai_10.y", "stai_11r.y", "stai_12.y",
                               "stai_13.y", "stai_14.y", "stai_15r.y", "stai_16r.y",
                               "stai_17.y", "stai_18.y", "stai_19r.y", "stai_20r.y")]    
alpha(endpoint_stai)

baseline_mdes_pos <- df_wide[ , c("mdes_1.x", "mdes_4.x","mdes_8.x", "mdes_11.x",
                                   "mdes_12.x", "mdes_13.x", "mdes_14.x", "mdes_15.x",
                                   "mdes_16.x", "mdes_19.x")]    
alpha(baseline_mdes_pos)

endpoint_mdes_pos <- df_wide[ , c("mdes_1.y", "mdes_4.y","mdes_8.y", "mdes_11.y",
                                   "mdes_12.y", "mdes_13.y", "mdes_14.y", "mdes_15.y",
                                   "mdes_16.y", "mdes_19.y")]    
alpha(endpoint_mdes_pos)


baseline_mdes_neg <- df_wide[ , c("mdes_2.x", "mdes_3.x","mdes_5.x", "mdes_6.x",
                                   "mdes_7.x", "mdes_9.x", "mdes_10.x", "mdes_17.x",
                                   "mdes_18.x", "mdes_20.x")]
alpha(baseline_mdes_neg)

endpoint_mdes_neg <- df_wide[ , c("mdes_2.y", "mdes_3.y","mdes_5.y", "mdes_6.y",
                                   "mdes_7.y", "mdes_9.y", "mdes_10.y", "mdes_17.y",
                                   "mdes_18.y", "mdes_20.y")]
alpha(endpoint_mdes_neg)

baseline_support <- df_wide[ , c("esupport_1.x", "esupport_2.x", "esupport_3.x",
                                  "esupport_4.x", "esupport_5.x", "esupport_7.x",
                                  "esupport_8.x", "esupport_9.x", "esupport_10.x",
                                  "esupport_11.x", "esupport_12.x",
                                  "isupport_1.x", "isupport_2.x", "isupport_3.x", "isupport_4.x",
                                  "isupport_5.x", "isupport_6.x", "isupport_7.x", "isupport_8.x", "isupport_9.x")]
alpha(baseline_support)

endpoint_support <- df_wide[ , c("esupport_1.y", "esupport_2.y", "esupport_3.y",
                                  "esupport_4.y", "esupport_5.y", "esupport_7.y",
                                  "esupport_8.y", "esupport_9.y", "esupport_10.y",
                                  "esupport_11.y", "esupport_12.y",
                                  "isupport_1.y", "isupport_2.y", "isupport_3.y", "isupport_4.y",
                                  "isupport_5.y", "isupport_6.y", "isupport_7.y", "isupport_8.y", "isupport_9.y")]
alpha(endpoint_support)

baseline_pss <- df_wide[ , c("pss_1.x", "pss_2r.x","pss_3r.x", "pss_4.x")]
alpha(baseline_pss)

endpoint_pss <- df_wide[ , c("pss_1.y", "pss_2r.y","pss_3r.y", "pss_4.y")]
alpha(endpoint_pss)

morning_pos.mood <- ema_morning[ , c("mood_positive", "mood_happy", "mood_peaceful", "mood_well")]
alpha(morning_pos.mood)

evening_pos.mood <- ema_evening[ , c("mood_positive", "mood_happy", "mood_peaceful", "mood_well")]
alpha(evening_pos.mood)

morning_neg.mood <- ema_morning[ , c("mood_negative", "mood_sad", "mood_anxious", "mood_angry", "mood_lonely")]
alpha(morning_neg.mood)

evening_neg.mood <- ema_evening[ , c("mood_negative", "mood_sad", "mood_anxious", "mood_angry", "mood_lonely")]
alpha(evening_neg.mood)

morning_stress <- ema_morning[ , c("stress1", "stress2")]
alpha(morning_stress)

evening_stress <- ema_evening[ , c("stress1", "stress2")]
alpha(evening_stress)

################################
#   PARTICIPANTS / PROCEDURE   #
################################

#remove participants without assigned group condition

frq(df_wide$`Package ID`) #143 NAs are individuals who did not enroll

df_wide <- df_wide %>% filter(!is.na(`Package ID`))

df_wide %>% count(ID)  #confirm 390 participants randomized

#create groupings for comparison - 4 conditions

df_wide <- df_wide %>% rename(condition = `Package ID`)
df_wide <- df_wide %>%
  mutate(condition = recode(condition,
                            "A100" = "Other Compassion",
                            "B100" = "Self Compassion",
                            "C100" = "Active Control",
                            "D100" = "No-Intervention"))

df_wide %>% count(condition)
#Other compassion 99
#Self Compassion 97
#Active Control 97
#No-Intervention 97

df_wide$condition <- factor(df_wide$condition)
df_wide$condition <- relevel(df_wide$condition, ref='No-Intervention')

#Demographic summary in manuscript

##AGE

frq(df_wide$age)
#(mean [SD] age, 37.21 [13.02] years; range, 18–77 years)
summary(df_wide$age)
#median 35.00

##GENDER

df_wide <- df_wide %>%
  mutate(gender = case_when(
    gender == "1" ~ "Man",
    gender == "2" ~ "Woman",
    gender %in% c("3", "4") ~ "Other/Declined"))

frq(df_wide$gender)
#148 men, 233 women, 8 other, 1 declined

df_wide$gender <- factor(df_wide$gender)
df_wide$gender <- relevel(df_wide$gender, ref='Other/Declined')

##RACE SELF-ID

df_wide <- df_wide %>%
  mutate(
    race_chr = as.character(race),
    white = if_else(!is.na(race_chr) & str_detect(race_chr, "1"), 1L, 0L)
  )
table(df_wide$white, useNA = "ifany")
df_wide$white <- factor(df_wide$white)

df_wide <- df_wide %>%
  mutate(
    race_chr = as.character(race),
    black = if_else(!is.na(race_chr) & str_detect(race_chr, "2"), 1L, 0L)
  )
table(df_wide$black, useNA = "ifany")
df_wide$black <- factor(df_wide$black)

df_wide <- df_wide %>%
  mutate(
    race_chr = as.character(race),
    native = if_else(!is.na(race_chr) & str_detect(race_chr, "3"), 1L, 0L)
  )
table(df_wide$native, useNA = "ifany")
df_wide$native <- factor(df_wide$native)

df_wide <- df_wide %>%
  mutate(
    race_chr = as.character(race),
    asian = if_else(!is.na(race_chr) & str_detect(race_chr, "4"), 1L, 0L)
  )
table(df_wide$asian, useNA = "ifany")
df_wide$asian <- factor(df_wide$asian)

df_wide <- df_wide %>%
  mutate(
    race_chr = as.character(race),
    pacific = if_else(!is.na(race_chr) & str_detect(race_chr, "5"), 1L, 0L)
  )
table(df_wide$pacific, useNA = "ifany")
df_wide$pacific <- factor(df_wide$pacific)

df_wide <- df_wide %>%
  mutate(
    race_chr = as.character(race),
    latin = if_else(!is.na(race_chr) & str_detect(race_chr, "6"), 1L, 0L)
  )
table(df_wide$latin, useNA = "ifany")
df_wide$latin <- factor(df_wide$latin)

df_wide <- df_wide %>%
  mutate(
    race_chr = as.character(race),
    other = if_else(!is.na(race_chr) & str_detect(race_chr, "7"), 1L, 0L)
  )
table(df_wide$other, useNA = "ifany")
df_wide$other <- factor(df_wide$other)

# days to complete endpoint survey

df_wide$EndDate <- as.POSIXct(df_wide$EndDate, format = "%m/%d/%Y %H:%M", tz = "UTC")
reference_date <- as.POSIXct("2025-03-31 00:00", format = "%Y-%m-%d %H:%M", tz = "UTC")
df_wide$complete.endpoint.days <- as.numeric(difftime(df_wide$EndDate, reference_date, units = "days"))
frq(df_wide$complete.endpoint.days)

df_wide %>%
  summarize(
    median_days = median(complete.endpoint.days, na.rm = TRUE),
    iqr_low = quantile(complete.endpoint.days, 0.25, na.rm = TRUE),
    iqr_high = quantile(complete.endpoint.days, 0.75, na.rm = TRUE),
    mean_days = mean(complete.endpoint.days, na.rm = TRUE),
    sd_days = sd(complete.endpoint.days, na.rm = TRUE),
    min_days = min(complete.endpoint.days, na.rm = TRUE),
    max_days = max(complete.endpoint.days, na.rm = TRUE)
  )

# EMA completion

ema_completion <- df_long_ema %>%
  group_by(ID) %>%
  summarize(
    n_completed = sum(`Completed Session` == 1, na.rm = TRUE),
    n_possible = n(),   
    completion_rate = n_completed / n_possible
  )

overall_completed <- sum(df_long_ema$`Completed Session` == 1, na.rm = TRUE)
overall_possible <- nrow(df_long_ema)
overall_rate <- overall_completed / overall_possible
overall_rate

ema_summary <- ema_completion %>%
  summarize(
    mean_rate = mean(completion_rate, na.rm = TRUE),
    sd_rate = sd(completion_rate, na.rm = TRUE),
    median_rate = median(completion_rate, na.rm = TRUE),
    iqr_low = quantile(completion_rate, 0.25, na.rm = TRUE),
    iqr_high = quantile(completion_rate, 0.75, na.rm = TRUE)
  )

print(ema_summary)
print(overall_completed)

##############################################
#   Table 1-                                 #
# Demographic Characteristics by Condition   #
##############################################

#age

df_wide %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean_age = formatC(mean(age, na.rm = TRUE), format = "f", digits = 3),
    sd_age   = formatC(sd(age, na.rm = TRUE), format = "f", digits = 3)
  )

#gender

df_wide %>%
  count(condition, gender) %>%
  group_by(condition) %>%
  mutate(pct = round(100 * n / sum(n), 3))

#race/ethnicity
frq(df_wide$white)
df_wide %>%
  count(condition, white) %>%
  group_by(condition) %>%
  mutate(pct = 100 * n / sum(n))

frq(df_wide$latin)
df_wide %>%
  count(condition, latin) %>%
  group_by(condition) %>%
  mutate(pct = 100 * n / sum(n))

frq(df_wide$asian)
df_wide %>%
  count(condition, asian) %>%
  group_by(condition) %>%
  mutate(pct = 100 * n / sum(n))

frq(df_wide$black)
df_wide %>%
  count(condition, black) %>%
  group_by(condition) %>%
  mutate(pct = 100 * n / sum(n))

df_wide <- df_wide %>%
  mutate(other_combined = ifelse(native == 1 | pacific == 1 | other == 1, 1, 0))

frq(df_wide$other_combined)

df_wide %>%
  count(condition, other_combined) %>%
  group_by(condition) %>%
  mutate(pct = 100 * n / sum(n))

# education

df_wide <- df_wide %>%
  mutate(education.years = recode(edu,
                                  '1' = 11,
                                  '2' = 12,
                                  '3' = 13,
                                  '4' = 14,
                                  '5' = 16,
                                  '6' = 18,
                                  '7' = 20
  ))

frq(df_wide$education.years)

df_wide %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean_edu = mean(education.years, na.rm = TRUE),
    sd_edu   = sd(education.years, na.rm = TRUE)
  )

#relationship status
df_wide <- df_wide %>%
  mutate(relationship = recode(relationship,
                               "1" = "Single",
                               "2" = "Open",
                               "3" = "Committed",
                               "4" = "Complicated"))
frq(df_wide$relationship)
df_wide$relationship <- factor(df_wide$relationship)
df_wide$relationship <- relevel(df_wide$relationship, ref='Open')

df_wide %>%
  count(condition, relationship) %>%
  group_by(condition) %>%
  mutate(pct = 100 * n / sum(n))

#income

df_wide <- df_wide %>%
  mutate(income = recode(income,
                         "1" = "$0 to $9,999",
                         "2" = "$10,000 to $14,999",
                         "3" = "$15,000 to $19,999",
                         "4" = "$20,000 to $34,999",
                         "5" = "$35,000 to $49,999",
                         "6" = "$50,000 to $74,999",
                         "7" = "$75,000 to $99,999",
                         "8" = "$100,000 to $199,999",
                         "9" = "$200,000 or more"
  ))

frq(df_wide$income)

df_wide %>%
  count(condition, income) %>%
  group_by(condition) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  print(n = Inf)

#political ideology

frq(df_wide$political)

df_wide %>%
  group_by(condition) %>%
  summarise(
    n = n(),
    mean_ideo = mean(political, na.rm = TRUE),
    sd_ideo  = sd(political, na.rm = TRUE)
  )

#partyID
df_wide <- df_wide %>%
  mutate(partyID = case_when(
    partyID == "1" ~ "Democrat",
    partyID == "2" ~ "Republican",
    partyID == "3" ~ "Independent",
    partyID %in% c("4", "5") ~ "None/Other"))

frq(df_wide$partyID)
df_wide$partyID <- factor(df_wide$partyID)
df_wide$partyID <- relevel(df_wide$partyID, ref='None/Other')

df_wide %>%
  count(condition, partyID) %>%
  group_by(condition) %>%
  mutate(pct = 100 * n / sum(n))

#################################
#  DATA EXCLUSION FOR ANALYSIS  #
#################################

#per pre-registration - exclude participants who complete fewer than 4 training sessions/less than 10%

df_wide <- df_wide %>% rename(total_training = `Total number of training`)
df_wide_final <- df_wide %>% filter(is.na(total_training) | total_training >= 5)

#Figure 1. numbers

##Endpoint 'no response'

endpoint_attrition_by_condition <- df_wide %>%
  filter(is.na(pwb.post)) %>%
  distinct(ID, condition) %>%
  count(condition, name = "n_missing")

endpoint_attrition_by_condition
#Other compassion 12
#Self Compassion 7
#Active Control 15
#No-Intervention 18

EMA_excluded <- df_wide %>%
  filter(!is.na(total_training) & total_training <= 4) %>%
  distinct(ID, condition) %>%
  count(condition, name = "n_excluded")

EMA_excluded
#Other compassion 27
#Self Compassion 33
#Active Control 32

prepost_analyzed <- df_wide_final %>% filter(is.na(total_training) | total_training <= 4)
prepost_analyzed <- df_wide_final %>% filter(!is.na(pwb.post))
n_flow <- nrow(prepost_analyzed)
n_flow
prepost_analyzed %>%  count(condition)
#Other compassion 70
#Self Compassion 63
#Active Control 58
#No-Intervention 79



# apply exclusions to df_long_ema

## merge df_wide_final (with exclusion) with df_long_ema
df_long_ema_final <- df_long_ema %>%
  left_join(df_wide_final, by = "ID") # exclusion applied through left join by ID

# check that numbers match between df_wide_final (without EMA) and df_long_ema_final
df_wide_final %>% count(condition)

df_long_ema_final %>%
  distinct(ID, condition) %>%
  count(condition)
#92 NAs are those excluded for completing <= 4 training sessions

df_wide_final$condition <- relevel(df_wide_final$condition, ref='No-Intervention')



# apply exclusions to df_long_training
df_long_training_final <- dplyr::select(df_long_training, -condition)

df_long_training_final <- df_long_training_final %>%
  left_join(df_wide_final, by = "ID")

# check that numbers match between df_wide_final and df_long_training_final
df_wide_final %>% count(condition)

df_long_training_final %>%
  distinct(ID, condition) %>%
  count(condition)

df_long_training_final$condition <- as.factor(df_long_training_final$condition)
df_long_training_final$condition <- relevel(df_long_training_final$condition, ref='No-Intervention')

#################
#  SI4 Table 1  #
#################

summary(lm(pwb.post ~ age, df_wide))
summary(lm(cesd.post ~ age, df_wide))
summary(lm(stai.post ~ age, df_wide))
summary(lm(mdes_pos.post ~ age, df_wide))
summary(lm(mdes_neg.post ~ age, df_wide))
summary(lm(support.post ~ age, df_wide))
summary(lm(pss.post ~ age, df_wide))

summary(lm(pwb.post ~ gender, df_wide))
summary(lm(cesd.post ~ gender, df_wide))
summary(lm(stai.post ~ gender, df_wide))
summary(lm(mdes_pos.post ~ gender, df_wide))
summary(lm(mdes_neg.post ~ gender, df_wide))
summary(lm(support.post ~ gender, df_wide))
summary(lm(pss.post ~ gender, df_wide))

summary(lm(pwb.post ~ white, df_wide))
summary(lm(cesd.post ~ white, df_wide))
summary(lm(stai.post ~ white, df_wide))
summary(lm(mdes_pos.post ~ white, df_wide))
summary(lm(mdes_neg.post ~ white, df_wide))
summary(lm(support.post ~ white, df_wide))
summary(lm(pss.post ~ white, df_wide))

summary(lm(pwb.post ~ latin, df_wide))
summary(lm(cesd.post ~ latin, df_wide))
summary(lm(stai.post ~ latin, df_wide))
summary(lm(mdes_pos.post ~ latin, df_wide))
summary(lm(mdes_neg.post ~ latin, df_wide))
summary(lm(support.post ~ latin, df_wide))
summary(lm(pss.post ~ latin, df_wide))

summary(lm(pwb.post ~ asian, df_wide))
summary(lm(cesd.post ~ asian, df_wide))
summary(lm(stai.post ~ asian, df_wide))
summary(lm(mdes_pos.post ~ asian, df_wide))
summary(lm(mdes_neg.post ~ asian, df_wide))
summary(lm(support.post ~ asian, df_wide))
summary(lm(pss.post ~ asian, df_wide))

summary(lm(pwb.post ~ black, df_wide))
summary(lm(cesd.post ~ black, df_wide))
summary(lm(stai.post ~ black, df_wide))
summary(lm(mdes_pos.post ~ black, df_wide))
summary(lm(mdes_neg.post ~ black, df_wide))
summary(lm(support.post ~ black, df_wide))
summary(lm(pss.post ~ black, df_wide))

summary(lm(pwb.post ~ education.years, df_wide))
summary(lm(cesd.post ~ education.years, df_wide))
summary(lm(stai.post ~ education.years, df_wide))
summary(lm(mdes_pos.post ~ education.years, df_wide))
summary(lm(mdes_neg.post ~ education.years, df_wide))
summary(lm(support.post ~ education.years, df_wide))
summary(lm(pss.post ~ education.years, df_wide))

summary(lm(pwb.post ~ relationship, df_wide))
summary(lm(cesd.post ~ relationship, df_wide))
summary(lm(stai.post ~ relationship, df_wide))
summary(lm(mdes_pos.post ~ relationship, df_wide))
summary(lm(mdes_neg.post ~ relationship, df_wide))
summary(lm(support.post ~ relationship, df_wide))
summary(lm(pss.post ~ relationship, df_wide))

summary(lm(pwb.post ~ income, df_wide))
summary(lm(cesd.post ~ income, df_wide))
summary(lm(stai.post ~ income, df_wide))
summary(lm(mdes_pos.post ~ income, df_wide))
summary(lm(mdes_neg.post ~ income, df_wide))
summary(lm(support.post ~ income, df_wide))
summary(lm(pss.post ~ income, df_wide))

summary(lm(pwb.post ~ political, df_wide))
summary(lm(cesd.post ~ political, df_wide))
summary(lm(stai.post ~ political, df_wide))
summary(lm(mdes_pos.post ~ political, df_wide))
summary(lm(mdes_neg.post ~ political, df_wide))
summary(lm(support.post ~ political, df_wide))
summary(lm(pss.post ~ political, df_wide))

summary(lm(pwb.post ~ partyID, df_wide))
summary(lm(cesd.post ~ partyID, df_wide))
summary(lm(stai.post ~ partyID, df_wide))
summary(lm(mdes_pos.post ~ partyID, df_wide))
summary(lm(mdes_neg.post ~ partyID, df_wide))
summary(lm(support.post ~ partyID, df_wide))
summary(lm(pss.post ~ partyID, df_wide))

#####################
#  SI4 Table 2 + 3  #
#####################

df_means <- df_long_ema_final %>%
  group_by(ID) %>%
  summarize(
    sleep.quality = mean(sleep.quality, na.rm = TRUE),
    sleep.duration_minutes = mean(sleep.duration_minutes, na.rm = TRUE),
    sleep.efficiency_minutes = mean(sleep.efficiency_minutes, na.rm = TRUE),
    stress = mean(c(morning.stress,   evening.stress),   na.rm = TRUE),
    positive = mean(c(morning.positive, evening.positive), na.rm = TRUE),
    negative = mean(c(morning.negative, evening.negative), na.rm = TRUE),
    morning.stress = mean(morning.stress, na.rm = TRUE),
    morning.positive = mean(morning.positive, na.rm = TRUE),
    morning.negative = mean(morning.negative, na.rm = TRUE),
    evening.stress = mean(evening.stress, na.rm = TRUE),
    evening.positive = mean(evening.positive, na.rm = TRUE),
    evening.negative = mean(evening.negative, na.rm = TRUE)) %>%
  left_join(df_wide_final %>% dplyr::select(ID, age, gender, white, black, native, asian, pacific, latin,
                                       education.years, relationship, income, political, partyID),
            by = "ID")


df_training_mean <- df_long_training_final %>%
  group_by(ID) %>%
  summarize(Well = mean(Well, na.rm = TRUE)) %>%
  left_join(df_wide_final %>% dplyr::select(ID, age, gender, white, black, native, asian, pacific, latin,
                                       education.years, relationship, income, political, partyID),
            by = "ID")

#association tests for continuous variables
cor.test(df_means$age, df_means$sleep.quality)
cor.test(df_means$age, df_means$sleep.duration_minutes)
cor.test(df_means$age, df_means$sleep.efficiency_minutes)
cor.test(df_means$age, df_means$positive)
cor.test(df_means$age, df_means$negative)
cor.test(df_means$age, df_means$stress)
cor.test(df_training_mean$age, df_training_mean$Well)

cor.test(df_means$education.years, df_means$sleep.quality)
cor.test(df_means$education.years, df_means$sleep.duration_minutes)
cor.test(df_means$education.years, df_means$sleep.efficiency_minutes)
cor.test(df_means$education.years, df_means$positive)
cor.test(df_means$education.years, df_means$negative)
cor.test(df_means$education.years, df_means$stress)
cor.test(df_training_mean$education.years, df_training_mean$Well)

cor.test(df_means$political, df_means$sleep.quality)
cor.test(df_means$political, df_means$sleep.duration_minutes)
cor.test(df_means$political, df_means$sleep.efficiency_minutes)
cor.test(df_means$political, df_means$positive)
cor.test(df_means$political, df_means$negative)
cor.test(df_means$political, df_means$stress)
cor.test(df_training_mean$political, df_training_mean$Well)

#association tests for categorical variables
summary(aov(sleep.quality ~ gender, data = df_means))
summary(aov(sleep.duration_minutes ~ gender, data = df_means))
summary(aov(sleep.efficiency_minutes ~ gender, data = df_means))
summary(aov(positive ~ gender, data = df_means))
summary(aov(negative ~ gender, data = df_means))
summary(aov(stress ~ gender, data = df_means))
summary(aov(Well ~ gender, data = df_training_mean))

t.test(sleep.quality ~ white, data = df_means)
t.test(sleep.duration_minutes ~ white, data = df_means)
t.test(sleep.efficiency_minutes ~ white, data = df_means)
t.test(positive ~ white, data = df_means)
t.test(negative ~ white, data = df_means)
t.test(stress ~ white, data = df_means)
t.test(Well ~ white, data = df_training_mean)

t.test(sleep.quality ~ latin, data = df_means)
t.test(sleep.duration_minutes ~ latin, data = df_means)
t.test(sleep.efficiency_minutes ~ latin, data = df_means)
t.test(positive ~ latin, data = df_means)
t.test(negative ~ latin, data = df_means)
t.test(stress ~ latin, data = df_means)
t.test(Well ~ latin, data = df_training_mean)

t.test(sleep.quality ~ asian, data = df_means)
t.test(sleep.duration_minutes ~ asian, data = df_means)
t.test(sleep.efficiency_minutes ~ asian, data = df_means)
t.test(positive ~ asian, data = df_means)
t.test(negative ~ asian, data = df_means)
t.test(stress ~ asian, data = df_means)
t.test(Well ~ asian, data = df_training_mean)

t.test(sleep.quality ~ black, data = df_means)
t.test(sleep.duration_minutes ~ black, data = df_means)
t.test(sleep.efficiency_minutes ~ black, data = df_means)
t.test(positive ~ black, data = df_means)
t.test(negative ~ black, data = df_means)
t.test(stress ~ black, data = df_means)
t.test(Well ~ black, data = df_training_mean)

summary(aov(sleep.quality ~ relationship, data = df_means))
summary(aov(sleep.duration_minutes ~ relationship, data = df_means))
summary(aov(sleep.efficiency_minutes ~ relationship, data = df_means))
summary(aov(positive ~ relationship, data = df_means))
summary(aov(negative ~ relationship, data = df_means))
summary(aov(stress ~ relationship, data = df_means))
summary(aov(Well ~ relationship, data = df_training_mean))

summary(aov(sleep.quality ~ income, data = df_means))
summary(aov(sleep.duration_minutes ~ income, data = df_means))
summary(aov(sleep.efficiency_minutes ~ income, data = df_means))
summary(aov(positive ~ income, data = df_means))
summary(aov(negative ~ income, data = df_means))
summary(aov(stress ~ income, data = df_means))
summary(aov(Well ~ income, data = df_training_mean))

summary(aov(sleep.quality ~ partyID, data = df_means))
summary(aov(sleep.duration_minutes ~ partyID, data = df_means))
summary(aov(sleep.efficiency_minutes ~ partyID, data = df_means))
summary(aov(positive ~ partyID, data = df_means))
summary(aov(negative ~ partyID, data = df_means))
summary(aov(stress ~ partyID, data = df_means))
summary(aov(Well ~ partyID, data = df_training_mean))

############
# Analysis #
############

####################
# immediate effect #
####################

# create daily dosage variable
df_long_training_final <- df_long_training_final %>% mutate(date = as.Date(Time))

daily_dosage <- df_long_training_final %>%
  group_by(ID, condition, date) %>%
  summarize(daily_dosage = if_else(first(condition) == "No-Intervention", 0L, n()), # count post-session events that day; no-intervention dosage set as 0
            .groups = "drop")

df_long_training_final <- df_long_training_final %>%
  left_join(daily_dosage, by = c("ID", "condition", "date"))

model_well_max <- lmer(Well ~ condition * daily_dosage + 
                         age + education.years + political + gender + relationship + income + partyID +
                         (1 + daily_dosage | ID),
                       data = df_long_training_final)

summary(model_well_max)

confint(model_well_max, method = "Wald")

####################
#    pre-to-post   #
####################

#condition effect (none observed)
summary(lm(pwb.post ~ condition + pwb.pre + age + black + education.years,  df_wide_final))
summary(lm(cesd.post ~ condition + cesd.pre + age + gender + education.years + income + political + partyID,  df_wide_final))
summary(lm(stai.post ~ condition + stai.pre + age + education.years + political + partyID,  df_wide_final))
summary(lm(mdes_pos.post ~ condition + mdes_pos.pre + age +  education.years + relationship + political + partyID,  df_wide_final))
summary(lm(mdes_neg.post ~ condition + mdes_neg.pre + age,  df_wide_final))
summary(lm(support.post ~ condition + support.pre + age + white + asian + education.years + income,  df_wide_final))
summary(lm(pss.post ~ condition + pss.pre + age + gender + education.years + income + political + partyID,  df_wide_final))

#within group

# T-tests by condition
ttest_pwb <- df_wide_final %>%
  group_by(condition) %>%
  summarize(
    mean_pre = mean(pwb.pre, na.rm = TRUE),
    mean_post = mean(pwb.post, na.rm = TRUE),
    t_test = list(t.test(pwb.post, pwb.pre, paired = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    ci_lower = sapply(t_test, function(x) x$conf.int[1]),
    ci_upper = sapply(t_test, function(x) x$conf.int[2])
  ) %>%
  dplyr::select(condition, t_stat, p_value, mean_pre, mean_post, ci_lower, ci_upper)%>%
  mutate(
    across(
      c(t_stat, mean_pre, mean_post, ci_lower, ci_upper),
      ~ formatC(.x, format = "f", digits = 3)
    ),
    p_value = ifelse(
      p_value < .001,
      "<.001",
      formatC(p_value, format = "f", digits = 3)
    )
  )

ttest_pwb

ttest_cesd <- df_wide_final %>%
  group_by(condition) %>%
  summarize(
    mean_pre = mean(cesd.pre, na.rm = TRUE),
    mean_post = mean(cesd.post, na.rm = TRUE),
    t_test = list(t.test(cesd.post, cesd.pre, paired = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    ci_lower = sapply(t_test, function(x) x$conf.int[1]),
    ci_upper = sapply(t_test, function(x) x$conf.int[2])
  ) %>%
  dplyr::select(condition, t_stat, p_value, mean_pre, mean_post, ci_lower, ci_upper)%>%
  mutate(
    across(
      c(t_stat, mean_pre, mean_post, ci_lower, ci_upper),
      ~ formatC(.x, format = "f", digits = 3)
    ),
    p_value = ifelse(
      p_value < .001,
      "<.001",
      formatC(p_value, format = "f", digits = 3)
    )
  )

ttest_cesd

ttest_stai <- df_wide_final %>%
  group_by(condition) %>%
  summarize(
    mean_pre = mean(stai.pre, na.rm = TRUE),
    mean_post = mean(stai.post, na.rm = TRUE),
    t_test = list(t.test(stai.post, stai.pre, paired = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    ci_lower = sapply(t_test, function(x) x$conf.int[1]),
    ci_upper = sapply(t_test, function(x) x$conf.int[2])
  ) %>%
  dplyr::select(condition, t_stat, p_value, mean_pre, mean_post, ci_lower, ci_upper)%>%
  mutate(
    across(
      c(t_stat, mean_pre, mean_post, ci_lower, ci_upper),
      ~ formatC(.x, format = "f", digits = 3)
    ),
    p_value = ifelse(
      p_value < .001,
      "<.001",
      formatC(p_value, format = "f", digits = 3)
    )
  )

ttest_stai


ttest_mdes_pos <- df_wide_final %>%
  group_by(condition) %>%
  summarize(
    mean_pre = mean(mdes_pos.pre, na.rm = TRUE),
    mean_post = mean(mdes_pos.post, na.rm = TRUE),
    t_test = list(t.test(mdes_pos.post, mdes_pos.pre, paired = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    ci_lower = sapply(t_test, function(x) x$conf.int[1]),
    ci_upper = sapply(t_test, function(x) x$conf.int[2])
  ) %>%
  dplyr::select(condition, t_stat, p_value, mean_pre, mean_post, ci_lower, ci_upper)%>%
  mutate(
    across(
      c(t_stat, mean_pre, mean_post, ci_lower, ci_upper),
      ~ formatC(.x, format = "f", digits = 3)
    ),
    p_value = ifelse(
      p_value < .001,
      "<.001",
      formatC(p_value, format = "f", digits = 3)
    )
  )

ttest_mdes_pos

ttest_mdes_neg <- df_wide_final %>%
  group_by(condition) %>%
  summarize(
    mean_pre = mean(mdes_neg.pre, na.rm = TRUE),
    mean_post = mean(mdes_neg.post, na.rm = TRUE),
    t_test = list(t.test(mdes_neg.post, mdes_neg.pre, paired = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    ci_lower = sapply(t_test, function(x) x$conf.int[1]),
    ci_upper = sapply(t_test, function(x) x$conf.int[2])
  ) %>%
  dplyr::select(condition, t_stat, p_value, mean_pre, mean_post, ci_lower, ci_upper)%>%
  mutate(
    across(
      c(t_stat, mean_pre, mean_post, ci_lower, ci_upper),
      ~ formatC(.x, format = "f", digits = 3)
    ),
    p_value = ifelse(
      p_value < .001,
      "<.001",
      formatC(p_value, format = "f", digits = 3)
    )
  )

ttest_mdes_neg

ttest_support <- df_wide_final %>%
  group_by(condition) %>%
  summarize(
    mean_pre = mean(support.pre, na.rm = TRUE),
    mean_post = mean(support.post, na.rm = TRUE),
    t_test = list(t.test(support.post, support.pre, paired = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    ci_lower = sapply(t_test, function(x) x$conf.int[1]),
    ci_upper = sapply(t_test, function(x) x$conf.int[2])
  ) %>%
  dplyr::select(condition, t_stat, p_value, mean_pre, mean_post, ci_lower, ci_upper)%>%
  mutate(
    across(
      c(t_stat, mean_pre, mean_post, ci_lower, ci_upper),
      ~ formatC(.x, format = "f", digits = 3)
    ),
    p_value = ifelse(
      p_value < .001,
      "<.001",
      formatC(p_value, format = "f", digits = 3)
    )
  )

ttest_support

ttest_pss <- df_wide_final %>%
  group_by(condition) %>%
  summarize(
    mean_pre = mean(pss.pre, na.rm = TRUE),
    mean_post = mean(pss.post, na.rm = TRUE),
    t_test = list(t.test(pss.post, pss.pre, paired = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    t_stat = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    ci_lower = sapply(t_test, function(x) x$conf.int[1]),
    ci_upper = sapply(t_test, function(x) x$conf.int[2])
  ) %>%
  dplyr::select(condition, t_stat, p_value, mean_pre, mean_post, ci_lower, ci_upper)%>%
  mutate(
    across(
      c(t_stat, mean_pre, mean_post, ci_lower, ci_upper),
      ~ formatC(.x, format = "f", digits = 3)
    ),
    p_value = ifelse(
      p_value < .001,
      "<.001",
      formatC(p_value, format = "f", digits = 3)
    )
  )

ttest_pss

############
#    EMA   #
############



#####sleep

#sleep quality
model_sleep_quality <- lmer(sleep.quality ~ day * condition + 
                              age + education.years + political + gender + relationship + income + partyID +
                              (1 + day | ID), 
                            data = df_long_ema_final)
#fail to converge

model_simple_sleep_quality <- lmer(sleep.quality ~ day * condition + 
                                     age + education.years + political + gender + relationship + income + partyID +
                                     (1 | ID), 
                                   data = df_long_ema_final)
summary(model_simple_sleep_quality)
confint(model_simple_sleep_quality, method = "Wald")

#sleep duration
model_sleep_duration <- lmer(sleep.duration_minutes ~ day * condition + 
                               education.years + asian + black + partyID +
                               (1 + day | ID), 
                             data = df_long_ema_final)

#fail to converge

model_simple_sleep_duration <- lmer(sleep.duration_minutes ~ day * condition + 
                                      education.years + asian + black + partyID +
                                      (1 | ID), 
                                    data = df_long_ema_final)

summary(model_simple_sleep_duration)
confint(model_simple_sleep_duration, method = "Wald")

#sleep efficiency

model_sleep_efficiency <- lmer(sleep.efficiency_minutes ~ day * condition + 
                                 education.years + income + partyID + 
                                 (1 + day | ID), 
                               data = df_long_ema_final)

#fail to converge

model_simple_sleep_efficiency <- lmer(sleep.efficiency_minutes ~ day * condition + 
                                        education.years + income + partyID + 
                                        (1 | ID), 
                                      data = df_long_ema_final)

summary(model_simple_sleep_efficiency)
confint(model_simple_sleep_efficiency, method = "Wald")

#####positive mood

df_positive <- df_long_ema_final %>%
  dplyr::select(ID, day, condition, morning.positive, evening.positive,
                age, education.years, political, gender, relationship, income, partyID) %>%
  pivot_longer(cols = c(morning.positive, evening.positive),
               names_to = "time_of_day",
               values_to = "positive_mood") %>%
  mutate(time_of_day = factor(ifelse(time_of_day == "morning.positive", "morning", "evening")))

#Model A: Day × Condition (no time of day)
model_a <- lmer(positive_mood ~ day * condition + 
                  age + education.years + political + gender + relationship + income + partyID +
                  (1 | ID),
                data = df_positive,
                REML = FALSE)

#Model B: Time of day x Condition (no day)
model_b <- lmer(positive_mood ~ time_of_day * condition +
                  age + education.years + political + gender + relationship + income + partyID +
                  (1 | ID),
                data = df_positive,
                REML = FALSE)

#Model C: Day × Condition + Time of Day
model_c <- lmer(positive_mood ~ day * condition + time_of_day + 
                  age + education.years + political + gender + relationship + income + partyID +
                  (1 | ID),
                data = df_positive,
                REML = FALSE)

#AIC comparison
aic_pos <- AIC(model_a, model_b, model_c) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("model") %>%
  dplyr::mutate(outcome = "positive_mood")

aic_pos

summary(model_c)
confint(model_c, method = "Wald")

###### negative mood

df_negative <- df_long_ema_final %>%
  dplyr::select(ID, day, condition, morning.negative, evening.negative,
                age, political, relationship, income) %>%
  pivot_longer(cols = c(morning.negative, evening.negative),
               names_to = "time_of_day",
               values_to = "negative_mood") %>%
  mutate(time_of_day = factor(ifelse(time_of_day == "morning.negative", "morning", "evening")))

#Model A: Day × Condition (no time of day)

model_neg_a <- lmer(negative_mood ~ day * condition + 
                      age + political + relationship + income +
                      (1 | ID),
                    data = df_negative,
                    REML = FALSE)

#Model B: Time of day x Condition (no day)
model_neg_b <- lmer(negative_mood ~ time_of_day * condition +
                      age + political + relationship + income +
                      (1 + day | ID),
                    data = df_negative,
                    REML = FALSE)


#Model C: Day × Condition + Time of Day
model_neg_c <- lmer(negative_mood ~ day * condition + time_of_day + 
                      age + political + relationship + income + 
                      (1 | ID),
                    data = df_negative,
                    REML = FALSE)

# AIC comparison
aic_neg <- AIC(model_neg_a, model_neg_b, model_neg_c) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("model") %>%
  dplyr::mutate(outcome = "negative_mood")
aic_neg

summary(model_neg_a)
confint(model_neg_a, method = "Wald")

###### stress

df_stress <- df_long_ema_final %>%
  dplyr::select(ID, day, condition, morning.stress, evening.stress,
                age, education.years, political, gender, relationship, income) %>%
  pivot_longer(cols = c(morning.stress, evening.stress),
               names_to = "time_of_day",
               values_to = "stress") %>%
  mutate(time_of_day = factor(ifelse(time_of_day == "morning.stress", "morning", "evening")))

#Model A: Day × Condition (no time of day)
model_stress_a <- lmer(stress ~ day * condition + 
                         age + education.years + political + gender + relationship + income +
                         (1 | ID),
                       data = df_stress,
                       REML = FALSE)

#Model B: Time of day x Condition (no day)
model_stress_b <- lmer(stress ~ time_of_day * condition + 
                         age + education.years + political + gender + relationship + income +
                         (1 | ID),
                       data = df_stress,
                       REML = FALSE)

#Model C: Day × Condition + Time of Day
model_stress_c <- lmer(stress ~ day * condition + time_of_day + 
                         age + education.years + political + gender + relationship + income +
                         (1 | ID),
                       data = df_stress,
                       REML = FALSE)

aic_stress <- AIC(model_stress_a, model_stress_b, model_stress_c) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("model") %>%
  dplyr::mutate(outcome = "stress")
aic_stress

summary(model_stress_c)
confint(model_stress_c, method = "Wald")

##########################
#      dosage effect     #
##########################

df_dosage <- df_wide_final %>% filter(condition %in% c("Other Compassion", "Self Compassion"))

training_summary <- df_dosage %>%
  summarize(
    median_training = median(total_training, na.rm = TRUE),
    iqr_low = quantile(total_training, 0.25, na.rm = TRUE),
    iqr_high = quantile(total_training, 0.75, na.rm = TRUE)
  )

training_summary
frq(df_dosage$total_training)

df_dosage$pwb.change <- df_dosage$pwb.post - df_dosage$pwb.pre
df_dosage$cesd.change <- df_dosage$cesd.post - df_dosage$cesd.pre
df_dosage$stai.change <- df_dosage$stai.post - df_dosage$stai.pre
df_dosage$mdes_pos.change <- df_dosage$mdes_pos.post - df_dosage$mdes_pos.pre
df_dosage$mdes_neg.change <- df_dosage$mdes_neg.post - df_dosage$mdes_neg.pre
df_dosage$support.change <- df_dosage$support.post - df_dosage$support.pre
df_dosage$pss.change <- df_dosage$pss.post - df_dosage$pss.pre


library(segmented)

psi_start <- median(df_dosage$total_training, na.rm = TRUE)

# Step 1: Fit a basic linear model
model_pwb <- lm(pwb.change ~ total_training, data = df_dosage)
summary(model_pwb)
segmented_model_pwb <- segmented(model_pwb, seg.Z = ~ total_training, psi = psi_start)
summary(segmented_model_pwb)

# Step 2: Apply segmented regression
seg_model_pwb <- segmented(model_pwb, seg.Z = ~total_training, psi = psi_start)

# Step 3: Visualize
plot(df_dosage$total_training, df_dosage$pwb.change, 
     xlab = "Total training completed", ylab = "Pre-to-post changes in Well-Being",
     pch = 19, cex = 1,
     col = "black")

plot(seg_model_pwb, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_pwb$psi["psi1.total_training", "Est."], col = "#EB5E29", lty = 2, lwd = 3)

#mdes neg
model_mdes_neg <- lm(mdes_neg.change ~ total_training, data = df_dosage)
summary(model_mdes_neg)
segmented_model_mdes_neg <- segmented(model_mdes_neg, seg.Z = ~ total_training, psi = psi_start)
summary(segmented_model_mdes_neg)

seg_model_mdes_neg <- segmented(model_mdes_neg, seg.Z = ~total_training, psi = psi_start)
plot(df_dosage$total_training, df_dosage$mdes_neg.change, 
     xlab = "Total training completed", ylab = "Pre-to-post changes in Negative Affect",
     ylim = c(-2, 2.5),
     pch = 19, cex = 1,
     col = "black")

plot(seg_model_mdes_neg, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_mdes_neg$psi["psi1.total_training", "Est."], col = "#EB5E29", lty = 2, lwd = 3)

# Figure 2
# Set up 1 row, 2 columns
par(mfrow = c(1, 2),     # 1 row, 2 panels
    mar = c(4, 4, 3, 1)) # margins: bottom, left, top, right

# ---- Panel A: Well-being ----
plot(df_dosage$total_training, df_dosage$pwb.change, 
     xlab = "Total training completed",
     ylab = "Pre-to-post changes in Well-Being",
     pch = 19, cex = 1,
     col = "black")

plot(seg_model_pwb, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_pwb$psi["psi1.total_training", "Est."],
       col = "#EB5E29", lty = 2, lwd = 3)

title("A. Well-Being")

# ---- Panel B: Negative affect ----
plot(df_dosage$total_training, df_dosage$mdes_neg.change, 
     xlab = "Total training completed",
     ylab = "Pre-to-post changes in Negative Affect",
     ylim = c(-2, 2.5),
     pch = 19, cex = 1,
     col = "black")

plot(seg_model_mdes_neg, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_mdes_neg$psi["psi1.total_training", "Est."],
       col = "#EB5E29", lty = 2, lwd = 3)

title("B. Negative Affect")

# Reset layout (important)
par(mfrow = c(1, 1))

#CESD

model_cesd <- lm(cesd.change ~ total_training, data = df_dosage)
summary(model_cesd)
segmented_model_cesd <- segmented(model_cesd, seg.Z = ~ total_training, psi = psi_start)
summary(segmented_model_cesd)

seg_model_cesd <- segmented(model_cesd, seg.Z = ~total_training, psi = psi_start)
plot(df_dosage$total_training, df_dosage$cesd.change, 
     xlab = "", ylab = "",
     ylim = c(-1.5, 1.5),
     pch = 19, cex = 1,
     col = "black")

plot(seg_model_cesd, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_cesd$psi["psi1.total_training", "Est."], col = "#EB5E29", lty = 2, lwd = 3)

#STAI
model_stai <- lm(stai.change ~ total_training, data = df_dosage)
summary(model_stai)
segmented_model_stai <- segmented(model_stai, seg.Z = ~ total_training, psi = psi_start)
summary(segmented_model_stai)

seg_model_stai <- segmented(model_stai, seg.Z = ~total_training, psi = psi_start)
plot(df_dosage$total_training, df_dosage$stai.change, 
     xlab = "", ylab = "",
     ylim = c(-1.5, 1.5),
     pch = 19, cex = 1,
     col = "black")

plot(seg_model_stai, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_stai$psi["psi1.total_training", "Est."], col = "#EB5E29", lty = 2, lwd = 3)

#mdes pos
model_mdes_pos <- lm(mdes_pos.change ~ total_training, data = df_dosage)
summary(model_mdes_pos)
segmented_model_mdes_pos <- segmented(model_mdes_pos, seg.Z = ~ total_training, psi = psi_start)
summary(segmented_model_mdes_pos)

seg_model_mdes_pos <- segmented(model_mdes_pos, seg.Z = ~total_training, psi = psi_start)
plot(df_dosage$total_training, df_dosage$mdes_pos.change, 
     xlab = "", ylab = "",
     ylim = c(-2, 2.5),
     pch = 19, cex = 1,
     col = "black")

plot(seg_model_mdes_pos, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_mdes_pos$psi["psi1.total_training", "Est."], col = "#EB5E29", lty = 2, lwd = 3)

#support
model_support <- lm(support.change ~ total_training, data = df_dosage)
summary(model_support)
segmented_model_support <- segmented(model_support, seg.Z = ~ total_training, psi = psi_start)
summary(segmented_model_support)

seg_model_support <- segmented(model_support, seg.Z = ~total_training, psi = psi_start)
plot(df_dosage$total_training, df_dosage$support.change, 
     xlab = "", ylab = "",
     ylim = c(-1.5, 2),
     pch = 19, cex = 1,
     col = "black")

plot(seg_model_support, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_support$psi["psi1.total_training", "Est."], col = "#EB5E29", lty = 2, lwd = 3)

#pss
model_pss <- lm(pss.change ~ total_training, data = df_dosage)
summary(model_pss)
segmented_model_pss <- segmented(model_pss, seg.Z = ~ total_training, psi = psi_start)
summary(segmented_model_pss)

seg_model_pss <- segmented(model_pss, seg.Z = ~total_training, psi = psi_start)
plot(df_dosage$total_training, df_dosage$pss.change, 
     xlab = "", ylab = "",
     ylim = c(-1.5, 2),
     pch = 19, cex = 1,
     col = "black")

plot(seg_model_pss, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_pss$psi["psi1.total_training", "Est."], col = "#EB5E29", lty = 2, lwd = 3)

#SI Figure

# --- 5 stacked panels ---
par(mfrow = c(3, 2),
    mar = c(2.2, 4.2, 2.2, 1.0),  # bottom, left, top, right
    oma = c(3.5, 0, 0, 0))        # outer margin for shared x label

# Helper to avoid repeating code
plot_seg <- function(x, y, seg_model, ylim, panel_title, ylab_txt) {
  plot(x, y,
       xlab = "", ylab = ylab_txt,
       ylim = ylim,
       pch = 19, cex = 1,
       col = "black")
  plot(seg_model, add = TRUE, col = "blue", lwd = 2.7)
  abline(v = seg_model$psi["psi1.total_training", "Est."],
         col = "#EB5E29", lty = 2, lwd = 3)
  title(panel_title, adj = 0.5)  
}

# A. Depression (CESD)
plot_seg(df_dosage$total_training, df_dosage$cesd.change,
         seg_model_cesd, ylim = c(-1.5, 1.5),
         panel_title = "A. Depression", ylab_txt = "Pre-to-post change")

# B. Anxiety (STAI)
plot_seg(df_dosage$total_training, df_dosage$stai.change,
         seg_model_stai, ylim = c(-1.5, 1.5),
         panel_title = "B. Anxiety", ylab_txt = "Pre-to-post change")

# C. Positive Affect (MDES-Pos)
plot_seg(df_dosage$total_training, df_dosage$mdes_pos.change,
         seg_model_mdes_pos, ylim = c(-2, 2.5),
         panel_title = "C. Positive Affect", ylab_txt = "Pre-to-post change")

# D. Social Support
plot_seg(df_dosage$total_training, df_dosage$support.change,
         seg_model_support, ylim = c(-1.5, 2),
         panel_title = "D. Social Support", ylab_txt = "Pre-to-post change")

# E. Stress (PSS)  + show x-axis labels only on bottom plot
plot(df_dosage$total_training, df_dosage$pss.change,
     xlab = "", ylab = "Pre-to-post change",
     ylim = c(-1.5, 2),
     pch = 19, cex = 1,
     col = "black")
plot(seg_model_pss, add = TRUE, col = "blue", lwd = 2.7)
abline(v = seg_model_pss$psi["psi1.total_training", "Est."],
       col = "#EB5E29", lty = 2, lwd = 3)
title("E. Stress", adj = 0.5)

# Shared x-axis label (bottom, for the whole figure)
mtext("Total training completed", side = 1, outer = TRUE, line = 1.5)

# Reset
par(mfrow = c(1, 1))



