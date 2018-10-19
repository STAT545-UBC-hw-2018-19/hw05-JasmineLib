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
library(RColorBrewer)
#library(dplyr)
```

### Part 1: Factor Management

Task 1.1: Drop a factor to remove Oceania. Remove unused factor levels. Provide concrete information before and after removing these rows and the number of levels of affected factors.

``` r
gapminder_no_oceania = gapminder %>% 
  filter(continent != "Oceania") %>% 
  droplevels()

str(gapminder_no_oceania$continent)
```

    ##  Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...

``` r
str(gapminder_no_oceania$country)
```

    ##  Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
gapminder_highGDP = gapminder %>% 
  filter(gdpPercap > 30000)

str(gapminder_highGDP)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    56 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 6 6 7 7 10 10 21 21 35 35 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 5 5 4 4 4 4 2 2 4 4 ...
    ##  $ year     : int  2002 2007 2002 2007 2002 2007 2002 2007 2002 2007 ...
    ##  $ lifeExp  : num  80.4 81.2 79 79.8 78.3 ...
    ##  $ pop      : int  19546792 20434176 8148312 8199783 10311970 10392226 31902268 33390141 5374693 5468120 ...
    ##  $ gdpPercap: num  30688 34435 32418 36126 30486 ...

``` r
gapminder_highGDP %>% 
  ggplot(aes(continent)) + geom_bar() + 
  scale_x_discrete(drop = FALSE)
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
gapminder_highGDP = gapminder_highGDP %>% 
  droplevels()

str(gapminder_highGDP)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    56 obs. of  6 variables:
    ##  $ country  : Factor w/ 21 levels "Australia","Austria",..: 1 1 2 2 3 3 4 4 5 5 ...
    ##  $ continent: Factor w/ 4 levels "Americas","Asia",..: 4 4 3 3 3 3 1 1 3 3 ...
    ##  $ year     : int  2002 2007 2002 2007 2002 2007 2002 2007 2002 2007 ...
    ##  $ lifeExp  : num  80.4 81.2 79 79.8 78.3 ...
    ##  $ pop      : int  19546792 20434176 8148312 8199783 10311970 10392226 31902268 33390141 5374693 5468120 ...
    ##  $ gdpPercap: num  30688 34435 32418 36126 30486 ...

Conclusion:

| Function           | Continent Factor | Country Factor |
|--------------------|------------------|----------------|
| Filter out Oceania | 5 continents     | 142 countries  |
| Drop Levels        | 4 continents     | 140 countries  |

Explore the effects of arrange(). Does merely arranging the data have any effect on, say, a figure?

``` r
gapminder %>% 
  filter(continent == "Americas" & year == 2007) %>% 
  arrange(desc(country)) %>% 
  ggplot(aes(gdpPercap, country)) + geom_point()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-3-1.png)

explore effects of reordering a factor and try this coupled with arrange() How does this affect figure output?

``` r
gap_america_2007 = gapminder %>% 
  filter(continent == "Americas" & year == 2007)

gap_america_2007 %>% 
  mutate(country = fct_reorder(country, gdpPercap)) %>%
  ggplot(aes(gdpPercap, country)) + geom_point()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
gap_america_2007 %>% 
  mutate(country = fct_reorder(country, lifeExp)) %>% #want to reorder country. how? order by lifeexp
  arrange(desc(country)) %>% 
  ggplot(aes(lifeExp, country)) + geom_point()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-4-2.png)

``` r
gap_america_2007 %>% 
  arrange(desc(country)) %>% 
  mutate(country = fct_reorder(country, lifeExp)) %>% 
  ggplot(aes(lifeExp, country)) + geom_point()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-4-3.png)

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
```

``` r
gapminder_oil_2007 = left_join(gapminder_2007, oil_consumption_2007, by= "country") %>% 
  na.omit() 
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

``` r
write.csv(gapminder_oil_2007,  "hw5_gapminder_oil_2007.csv")

str(gapminder_oil_2007)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    53 obs. of  5 variables:
    ##  $ country               : chr  "Algeria" "Argentina" "Australia" "Austria" ...
    ##  $ pop                   : int  33333216 40301927 20434176 8199783 150448339 190010647 7322858 33390141 16284741 1318683096 ...
    ##  $ gdpPercap             : num  6223 12779 34435 36126 1391 ...
    ##  $ continent             : Factor w/ 5 levels "Africa","Americas",..: 1 2 5 4 3 2 4 2 2 3 ...
    ##  $ tonnes_per_capita_2007: num  0.3806 0.5891 1.976 1.6118 0.0315 ...
    ##  - attr(*, "na.action")= 'omit' Named int  1 2 4 8 10 11 12 13 14 17 ...
    ##   ..- attr(*, "names")= chr  "1" "2" "4" "8" ...

``` r
#continent factor with 5 levels

gapminder_oil_2007 = gapminder_oil_2007 %>% 
  mutate(continent = fct_reorder(country, tonnes_per_capita_2007))

gapminder_oil_2007
```

    ## # A tibble: 53 x 5
    ##    country           pop gdpPercap continent  tonnes_per_capita_2007
    ##    <chr>           <int>     <dbl> <fct>                       <dbl>
    ##  1 Algeria      33333216     6223. Algeria                    0.381 
    ##  2 Argentina    40301927    12779. Argentina                  0.589 
    ##  3 Australia    20434176    34435. Australia                  1.98  
    ##  4 Austria       8199783    36126. Austria                    1.61  
    ##  5 Bangladesh  150448339     1391. Bangladesh                 0.0315
    ##  6 Brazil      190010647     9066. Brazil                     0.526 
    ##  7 Bulgaria      7322858    10681. Bulgaria                   0.692 
    ##  8 Canada       33390141    36319. Canada                     3.12  
    ##  9 Chile        16284741    13172. Chile                      0.970 
    ## 10 China      1318683096     4959. China                      0.275 
    ## # ... with 43 more rows

``` r
gapminder_oil_2007 %>% 
  mutate(country = fct_reorder(country, tonnes_per_capita_2007)) %>% 
  ggplot(aes(tonnes_per_capita_2007, country)) + geom_point()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
gapminder_oil_2007 %>% 
  ggplot(aes(pop, tonnes_per_capita_2007))+
  geom_point(alpha = 0.33)+
  theme_bw()+
  facet_wrap(~continent)
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-8-2.png)

### Part 3: Visualization Design

Old graph:

``` r
 ggplot(gapminder_2007, aes(continent, gdpPercap)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  geom_violin(fill = "darkslategray1", alpha = 0.5) +
  geom_jitter(alpha = 0.33) +
  xlab("Continent")+
  ylab("GDP Per Capita") +
  ggtitle("GDP Per Capita by Continent in 2007")
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-9-1.png)

-   it's hard to tell visually which continent has a higher mean GDP. I was not able to tell the difference between America vs. Asia, and Europe vs. Oceania.

New graph:

``` r
display.brewer.all()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
gapminder_2007 %>% 
  mutate(continent = fct_reorder(gapminder_2007$continent, gdpPercap)) %>% 
 ggplot( aes(continent, gdpPercap)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_color_continuous(palette = "Greens") +
  geom_violin(alpha = 0.5) +
  geom_jitter(alpha = 0.33) +
  xlab("Continent")+
  ylab("GDP Per Capita") +
  ggtitle("GDP Per Capita by Continent in 2007") +
  theme_bw() 
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-10-2.png)

``` r
gapminder_oil_2007 %>% 
  ggplot(aes(gdpPercap, tonnes_per_capita_2007)) + 
  geom_point(aes(colour = pop)) +
  scale_x_log10(labels = dollar_format())+
  scale_colour_distiller(
    trans="log10",
    breaks = 10^(1:10),
    labels = comma_format(),
    palette = "BrBg"
  ) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  theme_bw()+
  ggtitle("Tonnes of Oil per Capita by GDP per Capita ")
```

    ## Warning in pal_name(palette, type): Unknown palette BrBg

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-10-3.png)

Conclude: There is a trend showing increasing oil usate as GDP per Capita increases. Using the R Colour Distiller, we also notice that countries with a higher population tend towards lower oil usage, while those with a low, or mid-range population size use more oil.

### More?
