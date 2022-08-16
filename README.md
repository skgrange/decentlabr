# **decentlabr**

**decentlabr** is an R interface to the [Decentlab](https://www.decentlab.com) API. **decentlabr** uses a collection of [provided functions](https://docs.decentlab.com/data-access-guide/v5/index.html) and formats the returns for rapid analysis and use. 

To gain access to the API, credentials are needed in the form of a domain and key.

## Installation

To install the development version the [`remotes`](https://github.com/r-lib/remotes) package will need to be installed first. Then:

```
# Install envirologgerr
remotes::install_github("skgrange/decentlabr")
```

## Usage

**decentlabr** has two primary functions: `get_decent_lab_last_values` and `get_decent_lab_time_series`. `get_decent_lab_last_values` returns a table that contains the devices and sensors that are available for a given domain (user). For example, using demo credentials, there are 105 time series (device-sensor pairs) that can be downloaded:

``` r
# Load packages
library(dplyr)
library(decentlabr)

# Get all device-sensor time series for a domain/user
get_decent_lab_last_values(
  domain = "demo.decentlab.com",
  key = "eyJrIjoiclhMRFFvUXFzQXpKVkZydm52b0VMRVg3M3U2b3VqQUciLCJuIjoiZGF0YS1xdWVyeS1hcGktZGVtby0yIiwiaWQiOjF9"
)

#> # A tibble: 105 × 8
#>    date                device sensor          unit   value channel locat…¹ uqk  
#>    <dttm>              <chr>  <chr>           <chr>  <dbl> <chr>   <chr>   <chr>
#>  1 2021-12-12 09:22:42 1404   battery         V     1.86e0 <NA>    <NA>    1404…
#>  2 2018-02-22 16:40:53 1404   battery         V     3.02e0 <NA>    dlr13-… 1404…
#>  3 2021-12-12 09:22:42 1404   bosch-bmp280-p… Pa    9.90e4 <NA>    <NA>    1404…
#>  4 2018-02-22 16:40:53 1404   bosch-bmp280-p… Pa    9.63e4 <NA>    dlr13-… 1404…
#>  5 2021-12-12 09:22:42 1404   bosch-bmp280-t… °C    1.96e1 <NA>    <NA>    1404…
#>  6 2018-02-22 16:40:53 1404   bosch-bmp280-t… °C    2.22e1 <NA>    dlr13-… 1404…
#>  7 2021-12-12 09:22:42 1404   senseair-lp8-c… ppm   1.38e3 <NA>    <NA>    1404…
#>  8 2018-02-22 16:40:53 1404   senseair-lp8-c… ppm   1.42e3 <NA>    dlr13-… 1404…
#>  9 2021-12-12 09:22:42 1404   senseair-lp8-c… ppm   1.38e3 <NA>    <NA>    1404…
#> 10 2018-02-22 16:40:53 1404   senseair-lp8-c… ppm   1.43e3 <NA>    dlr13-… 1404…
#> # … with 95 more rows, and abbreviated variable name ¹location
#> # ℹ Use `print(n = ...)` to see more rows
```

To get the observations for a time series, the device must be known. Say a user was interested in CO~2~ concentrations, a low-cost sensor with the device number of 1404 can be accessed with these demo credentials. 

``` r
# Get time series for a particular device
get_decent_lab_last_values(
  domain = "demo.decentlab.com",
  key = "eyJrIjoiclhMRFFvUXFzQXpKVkZydm52b0VMRVg3M3U2b3VqQUciLCJuIjoiZGF0YS1xdWVyeS1hcGktZGVtby0yIiwiaWQiOjF9"
) %>% 
  filter(device == "1404") %>% 
  select(date,
         device,
         sensor,
         uqk)
         
#> # A tibble: 26 × 4
#>    date                device sensor                    uqk                     
#>    <dttm>              <chr>  <chr>                     <chr>                   
#>  1 2021-12-12 09:22:42 1404   battery                   1404.battery            
#>  2 2018-02-22 16:40:53 1404   battery                   1404.battery            
#>  3 2021-12-12 09:22:42 1404   bosch-bmp280-pressure     1404.bosch-bmp280-press…
#>  4 2018-02-22 16:40:53 1404   bosch-bmp280-pressure     1404.bosch-bmp280-press…
#>  5 2021-12-12 09:22:42 1404   bosch-bmp280-temperature  1404.bosch-bmp280-tempe…
#>  6 2018-02-22 16:40:53 1404   bosch-bmp280-temperature  1404.bosch-bmp280-tempe…
#>  7 2021-12-12 09:22:42 1404   senseair-lp8-co2          1404.senseair-lp8-co2   
#>  8 2018-02-22 16:40:53 1404   senseair-lp8-co2          1404.senseair-lp8-co2   
#>  9 2021-12-12 09:22:42 1404   senseair-lp8-co2-filtered 1404.senseair-lp8-co2-f…
#> 10 2018-02-22 16:40:53 1404   senseair-lp8-co2-filtered 1404.senseair-lp8-co2-f…
#> # … with 16 more rows
#> # ℹ Use `print(n = ...)` to see more rows
```

After the device is known, this is simply passed to the `get_decent_lab_time_series` function that will get/import/fetch the data that is available for the device. To get data for the device for the first day of 2021, `get_decent_lab_time_series` is used like this: 

``` r
# Get the time series for a device for a short time period
get_decent_lab_time_series(
  domain = "demo.decentlab.com",
  key = "eyJrIjoiclhMRFFvUXFzQXpKVkZydm52b0VMRVg3M3U2b3VqQUciLCJuIjoiZGF0YS1xdWVyeS1hcGktZGVtby0yIiwiaWQiOjF9",
  device = "1404",
  start = "2021-01-01",
  end = "2021-01-02"
)

#> # A tibble: 1,352 × 5
#>    date                  date_unix device sensor  value
#>    <dttm>                    <dbl>  <int> <chr>   <dbl>
#>  1 2021-01-01 00:09:28 1609459769.   1404 battery  2.54
#>  2 2021-01-01 00:19:31 1609460371.   1404 battery  2.54
#>  3 2021-01-01 00:29:31 1609460971.   1404 battery  2.54
#>  4 2021-01-01 00:39:26 1609461566.   1404 battery  2.54
#>  5 2021-01-01 00:49:29 1609462169.   1404 battery  2.54
#>  6 2021-01-01 01:09:29 1609463370.   1404 battery  2.54
#>  7 2021-01-01 01:19:29 1609463970.   1404 battery  2.54
#>  8 2021-01-01 01:29:28 1609464569.   1404 battery  2.54
#>  9 2021-01-01 01:39:31 1609465171.   1404 battery  2.54
#> 10 2021-01-01 01:49:31 1609465771.   1404 battery  2.54
#> # … with 1,342 more rows
#> # ℹ Use `print(n = ...)` to see more rows
```

Currently, all sensors' data will be returned for each device when using `get_decent_lab_time_series`, but this will likely change in the future. By default, the observations are provided in "long" format, but to reshape these data to wide format, use the `as_wide` argument: 

``` r
get_decent_lab_time_series(
  domain = "demo.decentlab.com",
  key = "eyJrIjoiclhMRFFvUXFzQXpKVkZydm52b0VMRVg3M3U2b3VqQUciLCJuIjoiZGF0YS1xdWVyeS1hcGktZGVtby0yIiwiaWQiOjF9",
  device = "1404",
  start = "2021-01-01",
  end = "2021-01-02",
  as_wide = TRUE
)

#> # A tibble: 104 × 16
#>    date                 date_unix device battery bosch…¹ bosch…² sense…³ sense…⁴
#>    <dttm>                   <dbl>  <int>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#>  1 2021-01-01 00:09:28     1.61e9   1404    2.54   95518    22.3     532     537
#>  2 2021-01-01 00:19:31     1.61e9   1404    2.54   95520    22.3     531     537
#>  3 2021-01-01 00:29:31     1.61e9   1404    2.54   95530    22.3     530     535
#>  4 2021-01-01 00:39:26     1.61e9   1404    2.54   95540    22.3     534     535
#>  5 2021-01-01 00:49:29     1.61e9   1404    2.54   95544    22.3     533     534
#>  6 2021-01-01 01:09:29     1.61e9   1404    2.54   95548    22.2     542     537
#>  7 2021-01-01 01:19:29     1.61e9   1404    2.54   95554    22.2     576     560
#>  8 2021-01-01 01:29:28     1.61e9   1404    2.54   95568    22.2     569     577
#>  9 2021-01-01 01:39:31     1.61e9   1404    2.54   95570    22.2     550     555
#> 10 2021-01-01 01:49:31     1.61e9   1404    2.54   95578    22.2     539     542
#> # … with 94 more rows, 8 more variables: senseair_lp8_ir <dbl>,
#> #   senseair_lp8_ir_filtered <dbl>, senseair_lp8_status <dbl>,
#> #   senseair_lp8_temperature <dbl>, senseair_lp8_vcap1 <dbl>,
#> #   senseair_lp8_vcap2 <dbl>, sensirion_sht21_humidity <dbl>,
#> #   sensirion_sht21_temperature <dbl>, and abbreviated variable names
#> #   ¹bosch_bmp280_pressure, ²bosch_bmp280_temperature, ³senseair_lp8_co2,
#> #   ⁴senseair_lp8_co2_filtered
#> # ℹ Use `print(n = ...)` to see more rows, and `colnames()` to see all variable names
```

These data can now be used for analysis. 
