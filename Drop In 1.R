library(tidyverse)
library(lubridate)

# Parsing dates
date1 = "2020-03-12" %>% ymd() 
date2 = "4th June 2020" %>% dmy()

datetime1 = "September 5th 2020 11:00" %>% mdy_hm()
datetime2 = "September 5th 2020 11:00 PM" %>% mdy_hm()
"Sep 5th 2020 11:00" %>% mdy_hm()

# Some don't parse
"Sept 5th 2020 11:00" %>% mdy_hm()

# Extracting parts of a date
year(date1)
hour(datetime2)

# Time zones
OlsonNames()
with_tz(datetime1, "US/Mountain")

# Calculations
# Time difference
date2 - date1

# Add a duration
date1 + ddays(20)
