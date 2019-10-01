library(vroom)
library(tidyverse)

arrest_df = vroom("data/raw-data/arrest_2010_present.csv", delim = "," ) %>% as_tibble()
arr = arrest_df %>% 
  mutate(list = str_split(Location, ","),
         Latitude = map(list, 1) %>% unlist() %>% str_remove("\\(") %>% str_trim(),
         Longitude = map(list, 2) %>% unlist() %>% str_remove("\\)") %>% str_trim())
arr = arr %>% select(Latitude, Longitude, `Report ID`, `Charge Group Code`)


crimes_df = vroom("data/raw-data/crime_2010_present.csv", delim = "," ) %>% as_tibble()
crimes = crimes_df %>% 
  rename(Latitude = LAT, 
         Longitude= LON)
crimes = crimes %>% select(DR_NO, Latitude, Longitude)
  


write_csv(arr, "data/cleaned-data/arrests_2010_present.csv" )
write_csv(crimes, "data/cleaned-data/crimes_2010_present.csv" )