reading\_data\_from\_the\_web
================
Wenzhao Wu
10/20/2020

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.0.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(httr)


knitr::opts_chunk$set(
  fig.width = 12,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Scrape a tables

read in the html

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_html = read_html(url)

drug_use_html
```

    ## {html_document}
    ## <html lang="en">
    ## [1] <head>\n<link rel="P3Pv1" href="http://www.samhsa.gov/w3c/p3p.xml">\n<tit ...
    ## [2] <body>\r\n\r\n<noscript>\r\n<p>Your browser's Javascript is off. Hyperlin ...

extract the table(s); focus on the first one

``` r
drug_use_html %>%
  html_nodes(css = "table")
```

    ## {xml_nodeset (15)}
    ##  [1] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ##  [2] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ##  [3] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ##  [4] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ##  [5] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ##  [6] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ##  [7] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ##  [8] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ##  [9] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ## [10] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ## [11] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ## [12] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ## [13] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ## [14] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...
    ## [15] <table class="rti" border="1" cellspacing="0" cellpadding="1" width="100 ...

``` r
table_marj = 
  drug_use_html %>% 
  html_nodes(css = "table") %>% 
  first() %>%
  html_table() %>%
  slice(-1) %>% 
  as_tibble()

table_marj
```

    ## # A tibble: 56 x 16
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-201~
    ##    <chr> <chr>            <chr>            <chr>          <chr>           
    ##  1 Tota~ 12.90a           13.36            0.002          13.28b          
    ##  2 Nort~ 13.88a           14.66            0.005          13.98           
    ##  3 Midw~ 12.40b           12.76            0.082          12.45           
    ##  4 South 11.24a           11.64            0.029          12.02           
    ##  5 West  15.27            15.62            0.262          15.53a          
    ##  6 Alab~ 9.98             9.60             0.426          9.90            
    ##  7 Alas~ 19.60a           21.92            0.010          17.30           
    ##  8 Ariz~ 13.69            13.12            0.364          15.12           
    ##  9 Arka~ 11.37            11.59            0.678          12.79           
    ## 10 Cali~ 14.49            15.25            0.103          15.03           
    ## # ... with 46 more rows, and 11 more variables: `12-17(2014-2015)` <chr>,
    ## #   `12-17(P Value)` <chr>, `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `18-25(P Value)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `26+(P Value)` <chr>, `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>,
    ## #   `18+(P Value)` <chr>

# Learning assessment

``` r
url_i = ("https://www.bestplaces.net/cost_of_living/city/new_york/new_york")

cost_living_html = read_html(url_i)

cost_living_html
```

    ## {html_document}
    ## <html xmlns="//www.w3.org/1999/xhtml">
    ## [1] <head>\n<!-- Google Tag Manager --><script>(function (w, d, s, l, i) {\r\ ...
    ## [2] <body><form method="post" action="/cost_of_living/city/new_york/new_york? ...

``` r
table_cost = 
  cost_living_html %>% 
  html_nodes(css = "table") %>%
  first() %>%
  html_table(header = T) 
  
table_cost
```

    ##     COST OF LIVING New York New York      USA
    ## 1          Overall    187.2    120.5      100
    ## 2          Grocery    116.6    103.8      100
    ## 3           Health    112.6    105.8      100
    ## 4          Housing    294.3    132.1      100
    ## 5 Median Home Cost $680,500 $305,400 $231,200
    ## 6        Utilities    150.5    115.9      100
    ## 7   Transportation    181.1    140.7      100
    ## 8    Miscellaneous    121.2    101.6      100

## Star Wars Movie info

``` r
swm_html = 
  read_html("https://www.imdb.com/list/ls070150896/")
```

grab elements that I want

``` r
title_vec = 
  swm_html %>%
  html_nodes(css = ".lister-item-header a") %>%
  html_text()

gross_rev_vec = 
  swm_html %>%
  html_nodes(css = ".text-small:nth-child(7) span:nth-child(5)") %>%
  html_text()

runtime_vec = 
  swm_html %>%
  html_nodes(css = ".runtime") %>%
  html_text()

swm_df = 
  tibble(
    title = title_vec,
    rev = gross_rev_vec,
    runtime = runtime_vec)
```

# Learning assessment

``` r
review_html = read_html("https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1")
```

``` r
review_title = 
  review_html %>%
  html_nodes(css = ".a-text-bold span") %>%
  html_text()

profile_vec = 
  review_html %>%
  html_nodes(css = "#cm_cr-review_list .a-profile-name") %>%
  html_text()

review_df = 
  tibble(customers = profile_vec,
         reviews = review_title)
review_df
```

    ## # A tibble: 10 x 2
    ##    customers          reviews                                                   
    ##    <chr>              <chr>                                                     
    ##  1 Ed Burger          I LOVE THIS MOVIE                                         
    ##  2 Larry Washington   Don't you wish you could go back?                         
    ##  3 Brett Ronald Mass~ Stupid, but very funny!                                   
    ##  4 franciso lopez j   The beat                                                  
    ##  5 Christinal         Hilarious                                                 
    ##  6 Margo 1            Love this movie                                           
    ##  7 AJ                 Entertaining, limited quality                             
    ##  8 AUCharger          Boo                                                       
    ##  9 Rhonda S.          Movie is still silly fun....amazon streamling leaves much~
    ## 10 donna              Brilliant and awkwardly funny.

## Get some water data

This is coming from an API

``` r
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.csv") %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   new_york_city_population = col_double(),
    ##   nyc_consumption_million_gallons_per_day = col_double(),
    ##   per_capita_gallons_per_person_per_day = col_double()
    ## )

``` r
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.json") %>% 
  content("text") %>%
  jsonlite::fromJSON() %>%
  as_tibble()
```

``` r
brfss_smart2010 = 
  GET("https://chronicdata.cdc.gov/resource/acme-vg9e.csv",
      query = list("$limit" = 5000)) %>% 
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   year = col_double(),
    ##   sample_size = col_double(),
    ##   data_value = col_double(),
    ##   confidence_limit_low = col_double(),
    ##   confidence_limit_high = col_double(),
    ##   display_order = col_double(),
    ##   locationid = col_logical()
    ## )

    ## See spec(...) for full column specifications.

some data aren’t so nice

``` r
poke = 
  GET("http://pokeapi.co/api/v2/pokemon/1") %>%
  content()

poke$name
```

    ## [1] "bulbasaur"

``` r
poke$height
```

    ## [1] 7

``` r
poke$abilities
```

    ## [[1]]
    ## [[1]]$ability
    ## [[1]]$ability$name
    ## [1] "overgrow"
    ## 
    ## [[1]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/65/"
    ## 
    ## 
    ## [[1]]$is_hidden
    ## [1] FALSE
    ## 
    ## [[1]]$slot
    ## [1] 1
    ## 
    ## 
    ## [[2]]
    ## [[2]]$ability
    ## [[2]]$ability$name
    ## [1] "chlorophyll"
    ## 
    ## [[2]]$ability$url
    ## [1] "https://pokeapi.co/api/v2/ability/34/"
    ## 
    ## 
    ## [[2]]$is_hidden
    ## [1] TRUE
    ## 
    ## [[2]]$slot
    ## [1] 3
