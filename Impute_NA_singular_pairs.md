make dataset for fare estimator
================
Ngoc Duong - nqd2000
12/4/2019

``` r
library(tidyverse)
library(readr)
library(purrr)
```

Load data

``` r
taxi_data = read_csv("./data/transport_final.csv")

transport_final = taxi_data %>% 
  separate(pickup_time, into = c("pickup_hr", "pickup_min", "pickup_sec"), sep = ":") %>% 
  separate(dropoff_time, into = c("dropoff_hr", "dropoff_min", "dropoff_sec"), sep = ":") %>% 
  select(-pickup_sec, -dropoff_sec) %>% 
  mutate(
    pickup_hr = as.numeric(pickup_hr),
    pickup_min = as.numeric(pickup_min),
    dropoff_hr = as.numeric(dropoff_hr),
    dropoff_min = as.numeric(dropoff_min))
```

Impute matrix for average distance

``` r
#Now we can find the pairs w.o. flipped duplicates
tp_impute_origin = 
transport_final %>% 
  filter(pu_boro != "Staten Island",
         type != "for hire") %>% 
  select(trip_distance, duration, do_neiborhood, pu_neiborhood) %>% 
  group_by(do_neiborhood, pu_neiborhood) %>%
  summarize(avg_dist = mean(trip_distance),
            avg_duration = mean(duration)) %>%
  ungroup() 

tp_impute = 
transport_final %>% 
  filter(pu_boro != "Staten Island",
         type != "for hire") %>% 
  select(trip_distance, duration, do_neiborhood, pu_neiborhood) %>% 
  group_by(do_neiborhood, pu_neiborhood) %>%
  summarize(avg_dist = mean(trip_distance),
            avg_duration = mean(duration)) %>%
  ungroup() %>% 
  select(-avg_dist, -avg_duration)

tp_impute_single = 
  tp_impute[!duplicated(lapply(as.data.frame(t(tp_impute), stringsAsFactors=FALSE), sort)),]

tp_impute_final =
  left_join(tp_impute_single, tp_impute_origin, by = c("do_neiborhood" = "do_neiborhood", "pu_neiborhood" = "pu_neiborhood"))


#Now we can use this wrangling to impute data for pairs that dont have a flipped duplicate
tp_impute_final =
  tp_impute_final %>% 
  mutate(pu_neighborhood = do_neiborhood,
         do_neighborhood = pu_neiborhood)

tp_impute_reverse = 
  tp_impute_final[,3:6] %>% 
  mutate(pu_neiborhood = pu_neighborhood,
         do_neiborhood = do_neighborhood)

impute_final = rbind(tp_impute_final, tp_impute_reverse) %>% 
  distinct() %>% 
  select(-pu_neighborhood, -do_neighborhood)

#obtain dataset with double flipped pairs
tp_impute_double = 
  tp_impute[duplicated(lapply(as.data.frame(t(tp_impute), stringsAsFactors=FALSE), sort)),]

tp_impute_double = left_join(tp_impute_double, tp_impute_origin, by = c("do_neiborhood" = "do_neiborhood", "pu_neiborhood" = "pu_neiborhood"))

impute_final = rbind(impute_final, tp_impute_double) %>% 
  distinct()
```

Export this dataset

``` r
write_csv(impute_final, "impute_final.csv")
```
