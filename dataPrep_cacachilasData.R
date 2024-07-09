library(tidyverse)
library(daymetr)

cacachilaDaymet <- download_daymet(lat = 25.707422,
                                   lon = -101.416848,
                                   start = 1/07/2021,
                                   end = 18/10/2022,
                                   internal = TRUE,
                                   simplify = TRUE)

cacachilaDaymetMeanTemp <- cacachilaDaymet %>% 
  filter(measurement %in% c("tmax..deg.c.",  "tmin..deg.c." )) %>% 
  spread(measurement, value) %>% 
  mutate(temp = (`tmax..deg.c.` + `tmin..deg.c.`)/2) %>% 
  dplyr::select(year, yday, temp)

cacachilaDaymetPrecip <- cacachilaDaymet %>% 
  filter(measurement == "prcp..mm.day.") %>% 
  rename(precip = value) %>% 
  dplyr::select(year, yday, precip)

cacachilaNew <- verA %>% 
  mutate(TF_hora = time_spent/3600) %>% 
  filter(readsite != "carmen") %>% 
  mutate(month_numeric = month(datetime),
         Disp = case_when(
           month_numeric %in% c(1, 11, 12) ~ "low",
           month_numeric %in% c(2, 3, 4) ~ "nectar",
           month_numeric %in% c(5, 6, 7) ~ "nectar and fruit",
           month_numeric %in% c(8, 9, 10) ~ "fruit"
         ),
         yday = yday(datetime)) %>% 
  left_join(cacachilaDaymetMeanTemp, by = c("year", "yday")) %>% 
  left_join(cacachilaDaymetPrecip, by = c("year", "yday"))


writexl::write_xlsx(cacachilaDaymetPrecip, paste0(here::here(), "/hip2mm.xlsx"))


write_csv(hipolitoNew, here::here("data", "analyses_data", "hipolitoNew.csv"))