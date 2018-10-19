STAT545\_hw05\_JasmineLib
================

cowplot gridextratidy

Homework 5: Factor and Figure Management
========================================

``` r
library(gapminder)
library(tidyverse)
```

    ## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.7
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:viridis':
    ## 
    ##     viridis_pal

    ## The following object is masked from 'package:purrr':
    ## 
    ##     discard

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

``` r
#library(dplyr)
```

### Part 1: Factor Management

Task 1.1: Drop a factor to remove Oceania. Remove unused factor levels. Provide concrete information before and after removing these rows and the number of levels of affected factors. explore the effects of arrange() explore effects of reordering a factor and try this coupled with arrange() How does this affect figure output?

``` r
gapminder %>% 
  filter(continent != "Oceania") %>% 
  ggplot(aes(continent)) + geom_bar() + 
  scale_x_discrete(drop = FALSE) #shows "hidden factor Oceania"
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
?fct_infreq()
str(gapminder$country)
```

    ##  Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
gapminder %>% 
  filter(continent != "Oceania") %>% 
  droplevels() %>%  #removes oceania.
  levels()
```

    ## NULL

``` r
  levels(gapminder)
```

    ## NULL

``` r
  #ggplot(aes(country, gdpPercap)) +
  #geom_point() 
  
  gap_america_2007 = gapminder %>% 
  filter(continent == "Americas", year == 2007)

gap_america_2007 %>% 
mutate(country = fct_reorder(country, gdpPercap)) %>%
 ggplot(aes(gdpPercap, country)) + geom_point()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-2-2.png)

### Part 2: File I/O

``` r
oil_consumption = read_csv(file ="Oil_Consumption_per_capita.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   `Oil Consumption per capita (tonnes per year)` = col_character()
    ## )

    ## See spec(...) for full column specifications.

``` r
head(oil_consumption)
```

    ## # A tibble: 6 x 47
    ##   `Oil Consumptio… `1965` `1966` `1967` `1968` `1969` `1970` `1971`  `1972`
    ##   <chr>             <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
    ## 1 Algeria           0.108  0.138  0.126  0.130  0.133  0.148  0.162  0.172 
    ## 2 Argentina         0.985  1.00   1.01   1.02   1.04   0.915  0.964  0.934 
    ## 3 Australia         1.49   1.56   1.67   1.80   1.88   1.92   1.99   1.95  
    ## 4 Austria           0.761  0.830  0.883  1.02   1.11   1.21   1.35   1.45  
    ## 5 Azerbaijan       NA     NA     NA     NA     NA     NA     NA     NA     
    ## 6 Bangladesh       NA     NA     NA     NA     NA     NA     NA      0.0111
    ## # ... with 38 more variables: `1973` <dbl>, `1974` <dbl>, `1975` <dbl>,
    ## #   `1976` <dbl>, `1977` <dbl>, `1978` <dbl>, `1979` <dbl>, `1980` <dbl>,
    ## #   `1981` <dbl>, `1982` <dbl>, `1983` <dbl>, `1984` <dbl>, `1985` <dbl>,
    ## #   `1986` <dbl>, `1987` <dbl>, `1988` <dbl>, `1989` <dbl>, `1990` <dbl>,
    ## #   `1991` <dbl>, `1992` <dbl>, `1993` <dbl>, `1994` <dbl>, `1995` <dbl>,
    ## #   `1996` <dbl>, `1997` <dbl>, `1998` <dbl>, `1999` <dbl>, `2000` <dbl>,
    ## #   `2001` <dbl>, `2002` <dbl>, `2003` <dbl>, `2004` <dbl>, `2005` <dbl>,
    ## #   `2006` <dbl>, `2007` <dbl>, `2008` <dbl>, `2009` <dbl>, `2010` <dbl>

``` r
?saveRDS()


oil_consumption_2007 = oil_consumption %>% 
  select(`Oil Consumption per capita (tonnes per year)`, "2007") %>% 
  mutate(country = `Oil Consumption per capita (tonnes per year)`, 
         tonnes_per_capita_2007 = `2007`) %>% 
  select(country, tonnes_per_capita_2007)



head(oil_consumption_2007)
```

    ## # A tibble: 6 x 2
    ##   country    tonnes_per_capita_2007
    ##   <chr>                       <dbl>
    ## 1 Algeria                    0.381 
    ## 2 Argentina                  0.589 
    ## 3 Australia                  1.98  
    ## 4 Austria                    1.61  
    ## 5 Azerbaijan                 0.510 
    ## 6 Bangladesh                 0.0315

``` r
gapminder_2007 = gapminder %>% 
  filter (year == "2007") %>% 
  select(country = "country", pop, gdpPercap, continent) #select the population and gdpPercap data for the year 2007. 
  
gapminder_2007 %>% 
  head() %>% 
  knitr::kable()
```

| country     |       pop|   gdpPercap| continent |
|:------------|---------:|-----------:|:----------|
| Afghanistan |  31889923|    974.5803| Asia      |
| Albania     |   3600523|   5937.0295| Europe    |
| Algeria     |  33333216|   6223.3675| Africa    |
| Angola      |  12420476|   4797.2313| Africa    |
| Argentina   |  40301927|  12779.3796| Americas  |
| Australia   |  20434176|  34435.3674| Oceania   |

``` r
gapminder_oil_2007 = left_join(gapminder_2007, oil_consumption_2007, by= "country") %>% 
  na.omit() 
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

``` r
gapminder_oil_2007
```

    ## # A tibble: 53 x 5
    ##    country           pop gdpPercap continent tonnes_per_capita_2007
    ##    <chr>           <int>     <dbl> <fct>                      <dbl>
    ##  1 Algeria      33333216     6223. Africa                    0.381 
    ##  2 Argentina    40301927    12779. Americas                  0.589 
    ##  3 Australia    20434176    34435. Oceania                   1.98  
    ##  4 Austria       8199783    36126. Europe                    1.61  
    ##  5 Bangladesh  150448339     1391. Asia                      0.0315
    ##  6 Brazil      190010647     9066. Americas                  0.526 
    ##  7 Bulgaria      7322858    10681. Europe                    0.692 
    ##  8 Canada       33390141    36319. Americas                  3.12  
    ##  9 Chile        16284741    13172. Americas                  0.970 
    ## 10 China      1318683096     4959. Asia                      0.275 
    ## # ... with 43 more rows

``` r
gapminder_oil_2007 %>% 
  mutate(country = fct_reorder(country, tonnes_per_capita_2007)) %>% 
  ggplot(aes(country,tonnes_per_capita_2007)) + geom_point()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
gapminder_oil_2007 %>% 
  ggplot(aes(pop, tonnes_per_capita_2007))+
  geom_point(alpha = 0.33)+
  theme_bw()+
  facet_wrap(~continent)
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-5-2.png)

### Part 3: Visualization Design

### More?
