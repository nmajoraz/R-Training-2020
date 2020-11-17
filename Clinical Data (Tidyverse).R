####################################################
# Program Name:                adsl.R
# Author:                      Nicole Major
# Purpose:                     code to generate adsl
# Initial creation date:       2019-MAR-05
# Last modification date:      2020-NOV-04 
# Modified by:                 NA
# Current program Version:     2.2
# Previous Versions:           1.0, 2.0, 2.1 
# Modification History:
# 1.0     -   Initial release
# 2.0     -   added special conditions
# 2.1     -    fixed errors xxxxx 
# 2.2     -    spec change yyyy applied 
###################################################
# libraries + working directory
library(tidyverse)
library(lubridate)
setwd("C:/Users/kpkb060/Desktop/R Training/sdtm")

# files
dm = read_csv(file = "dm.csv")
ds = read_csv(file = "ds.csv")
adlb = read_csv(file = "lb.csv")
suppdm = read_csv(file = "suppdm.csv")
ae = read_csv(file = "ae.csv")
vs = read_csv(file = "vs.csv", guess_max = 5000)

# adsl dataset
dm %>% 
  select(-RFICDTC, -ACTARMCD) %>% 
  mutate(trtfl = if_else(ARMCD == "Pbo" | ARMCD == "Xan_Lo", "Y", 
                         if_else(ARMCD == "Scrnfail", "N", NA_character_))) %>% 
  # may need to add + 1 here
  mutate(dur_trt = as.double(difftime(time1 = RFENDTC, 
                                      time2 = RFSTDTC, 
                                      units = "days"))) %>% 
  mutate(rand_to_death = if_else(!is.na(DTHDTC), 
                                 as.double(difftime(time1 = DTHDTC, # DTHDTC isn't missing
                                            time2 = RFSTDTC, 
                                            unit = "days")), 
                                 0)) %>% # value of 0 if it is
  mutate(agegrp = case_when(AGE < 65 ~ "<65",
                            AGE >= 65 & AGE <= 75 ~ "65 to 75",
                            AGE > 75 ~ ">75")) %>%
  arrange(USUBJID, RFSTDTC) -> adsl

ds %>% 
  filter(DSDECOD == "COMPLETED") -> ds

adsl %>% 
  left_join(ds, c("STUDYID", "USUBJID")) %>% 
  mutate(compfl = if_else(DSDECOD == "COMPLETED", "Y", "N")) -> adsl
  
# adlb dataset
adlb %>% 
  arrange(USUBJID, LBTEST, VISITNUM) %>% 
  group_by(USUBJID, LBTEST) %>% 
  mutate(ord_test = seq(1:n())) %>% 
  mutate(first_last_3_flag = case_when(first(ord_test) == ord_test ~ "F",
                                       last(ord_test) == ord_test ~ "L",
                                       nth(ord_test, n = 3) == ord_test ~ "3rd")) %>% 
  mutate(mean_test = mean(LBSTRESN)) %>% 
  mutate(basefl = if_else(VISITNUM == 1, "Y", NA_character_)) %>% 
  mutate(pchg_base = (
    (LBSTRESN - first(LBSTRESN, order_by = VISITNUM))/first(LBSTRESN, order_by = VISITNUM)) * 100) %>% 
  arrange(desc(USUBJID), LBTEST) %>% 
  write_csv(file = "C:/Users/kpkb060/Desktop/R Training/sdtm/adlb_final.csv", 
            append = FALSE)

suppdm %>% 
  pivot_wider(id_cols = c(USUBJID, IDVAR), names_from = QNAM, values_from = QVAL) -> suppdm

suppdm

dm %>% 
  left_join(suppdm) %>% select(USUBJID, IDVAR:SAFETY) -> addm

#  R Function
count_ae = function(ae.Tibble, term = "FATIGUE"){
  ae.Tibble %>% 
    filter(AETERM == term) %>% 
    distinct(USUBJID, AETERM) %>% 
    group_by(AETERM) %>% 
    summarize(count_ae = n()) %>% 
    ungroup()
}

count_ae(ae, term = "HIATUS HERNIA")

ae %>% 
  distinct(USUBJID, AETERM) %>% 
  group_by(AETERM) %>% 
  summarize(count_ae = n()) %>% 
  ungroup() %>% 
  arrange(desc(count_ae))

# ggplot
tests = c("SYSBP", "PULSE", "WEIGHT")
vs %>% 
  filter(VSTESTCD %in% tests & VISITNUM %in% c(3:5)) %>% 
  ggplot(aes(x = VISIT, y = VSSTRESN, color = VSTESTCD)) + 
    geom_jitter(show.legend = FALSE) + 
    stat_summary(geom = "errorbar", fun = mean, fun.min = mean, fun.max = mean, 
                 color = "gray", size = 1) + 
    facet_grid(vars(VSTESTCD)) + 
    ggtitle("Vitals") + xlab("Visit") + ylab("Results") + 
    theme(plot.title = element_text(hjust = 0.5))

# testing 
         