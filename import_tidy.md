final\_project
================

## Importing data in and initial tidying data

``` r
green_taxi_full_data <- read_csv("./data/green_tripdata_2019-02.csv") %>% 
  janitor::clean_names() %>% 
  rename(
    pickup_datetime = lpep_pickup_datetime,
    dropoff_datetime = lpep_dropoff_datetime
  ) %>% 
  separate(pickup_datetime, into = c("pickup_date", "pickup_time"), sep = " ") %>% 
  separate(dropoff_datetime, into = c("dropoff_date", "dropoff_time"), sep = " ")
```

``` r
yellow_taxi_full_data <- read_csv("./data/yellow_tripdata_2019-02.csv") %>% 
  janitor::clean_names() %>% 
  rename(
   pickup_datetime = tpep_pickup_datetime,
   dropoff_datetime = tpep_dropoff_datetime
  ) %>% 
  separate(pickup_datetime, into = c("pickup_date", "pickup_time"), sep = " ") %>% 
  separate(dropoff_datetime, into = c("dropoff_date", "dropoff_time"), sep = " ") 
```

``` r
for_hire_vehicle_data <- read_csv("./data/fhvhv_tripdata_2019-02.csv") %>% 
  janitor::clean_names() %>% 
  separate(pickup_datetime, into = c("pickup_date", "pickup_time"), sep = " ") %>% 
  separate(dropoff_datetime, into = c("dropoff_date", "dropoff_time"), sep = " ")
```

## Filtering for Valentine’s Day

``` r
val_day_green <- green_taxi_full_data %>% 
  filter(pickup_date %in% "2019-02-14")

val_night_green = green_taxi_full_data %>% 
 filter(pickup_date == "2019-02-15") %>% 
 filter(stringr::str_detect(pickup_time, '^00|^01'))

vday_green_taxi <- rbind(val_day_green, val_night_green)
```

``` r
val_day_yellow <- yellow_taxi_full_data %>%
  filter(pickup_date %in% "2019-02-14")

val_night_yellow = yellow_taxi_full_data %>% 
 filter(pickup_date == "2019-02-15") %>% 
 filter(stringr::str_detect(pickup_time, '^00|^01'))

vday_yellow_taxi <- rbind(val_day_yellow, val_night_yellow)
```

``` r
val_day_for_hire <- for_hire_vehicle_data %>% 
  filter(pickup_date %in% "2019-02-14") 

val_night_for_hire = for_hire_vehicle_data %>% 
 filter(pickup_date == "2019-02-15") %>% 
 filter(stringr::str_detect(pickup_time, '^00|^01'))

vday_for_hire_vehicle <- rbind(val_day_for_hire, val_night_for_hire)
```

# Export datasets

``` r
write_csv(vday_green_taxi, "vday_green_taxi.csv")
write_csv(vday_yellow_taxi, "vday_yellow_taxi.csv")
write_csv(vday_for_hire_vehicle, "vday_for_hire_vehicle.csv")
```

# Reimporting data from csv (trying to make sure that it works)

``` r
vday_green_taxi <- read_csv("./data/vday_green_taxi.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   pickup_date = col_date(format = ""),
    ##   pickup_time = col_time(format = ""),
    ##   dropoff_date = col_date(format = ""),
    ##   dropoff_time = col_time(format = ""),
    ##   store_and_fwd_flag = col_character(),
    ##   ehail_fee = col_logical()
    ## )

    ## See spec(...) for full column specifications.

``` r
vday_yellow_taxi <- read_csv("./data/vday_yellow_taxi.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   pickup_date = col_date(format = ""),
    ##   pickup_time = col_time(format = ""),
    ##   dropoff_date = col_date(format = ""),
    ##   dropoff_time = col_time(format = ""),
    ##   store_and_fwd_flag = col_character()
    ## )
    ## See spec(...) for full column specifications.

``` r
vday_for_hire_vehicle <- read_csv("./data/vday_for_hire_vehicle.csv") 
```

    ## Parsed with column specification:
    ## cols(
    ##   hvfhs_license_num = col_character(),
    ##   dispatching_base_num = col_character(),
    ##   pickup_date = col_date(format = ""),
    ##   pickup_time = col_time(format = ""),
    ##   dropoff_date = col_date(format = ""),
    ##   dropoff_time = col_time(format = ""),
    ##   pu_location_id = col_double(),
    ##   do_location_id = col_double(),
    ##   sr_flag = col_double()
    ## )

# Filtering for Uber and Lyft Post Cleaned Data

``` r
vday_for_hire_vehicle <- vday_for_hire_vehicle %>% 
  filter(hvfhs_license_num %in% c("HV0003", "HV0005"))
```

# Sampling datasets

``` r
yellow_taxi_vday_samp <- sample_frac(vday_yellow_taxi, size = 0.1) %>% 
  mutate(type = "yellow")
green_taxi_vday_samp <- sample_frac(vday_green_taxi, size = 0.2) %>% 
  mutate(type = "green")
for_hire_vday_samp <- sample_frac(vday_for_hire_vehicle, size = 0.1) %>% 
  mutate(type = "for hire")
```

Merge datasets

``` r
zone = read_csv("./data/taxi_zones.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   zone_id = col_double(),
    ##   borough = col_character(),
    ##   zone = col_character(),
    ##   x = col_double(),
    ##   y = col_double(),
    ##   shape_length = col_double(),
    ##   shape_area = col_double()
    ## )

``` r
transport = bind_rows(yellow_taxi_vday_samp, green_taxi_vday_samp, for_hire_vday_samp) 

transport_final = left_join(transport, zone, by = c("pu_location_id" = "zone_id")) %>% 
  rename(pu_neiborhood = zone,
         pu_boro = borough) %>% 
  left_join(., zone, 
            by = c("do_location_id" = "zone_id")) %>%
  rename(do_neiborhood = zone,
         do_boro = borough) %>% 
  select(-ends_with("location_id")) %>% 
  filter(do_boro == "Manhattan") 
```

Add duration variable

``` r
transport_final = transport_final %>% 
  mutate(
  pu_time = paste(pickup_date, pickup_time, sep = " "),
  do_time = paste(dropoff_date, dropoff_time, sep = " "),
  duration = as.numeric(difftime(do_time, pu_time, units = "mins"))
) %>% 
  select(-do_time, -pu_time, -do_boro)
```

\<\<\<\<\<\<\< HEAD

``` r
test = transport_final %>% 
  filter(pickup_date == "2019-02-14" & dropoff_date == "2019-02-15") %>% 
  filter(duration >100)

# Export Final Transport Data into CSV
write_csv(transport_final, "./data/transport_final.csv")
```
