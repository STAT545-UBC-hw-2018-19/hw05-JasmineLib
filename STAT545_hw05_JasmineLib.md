STAT545\_hw05\_JasmineLib
================

cowplot gridextratidy

Homework 5: Factor and Figure Management
========================================

``` r
library(gapminder)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.7
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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
levels(gapminder$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"   "Oceania"

``` r
nlevels(gapminder$country)
```

    ## [1] 142

``` r
gapminder_no_oceania = gapminder %>% 
  filter(continent != "Oceania") %>% 
  droplevels()

levels(gapminder_no_oceania$continent)
```

    ## [1] "Africa"   "Americas" "Asia"     "Europe"

``` r
nlevels(gapminder_no_oceania$country)
```

    ## [1] 140

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
  mutate(country = fct_reorder(country, lifeExp)) %>% #want to reorder country. how? order by lifeexp
  arrange(desc(country)) %>% 
  ggplot(aes(lifeExp, country)) + 
  geom_point(color = "dodgerblue3") +
  theme_bw() +
  ggtitle("Life Expectancy in the Americas in 2007") +
  labs(x = "Life Expectancy (Years)", 
       y = "Country")
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
gap_america_2007 %>% 
  arrange(desc(country)) %>% 
  mutate(country = fct_reorder(country, lifeExp)) %>% 
  ggplot(aes(lifeExp, country)) + 
  geom_point() +
  geom_point(color = "dodgerblue3") +
  theme_bw() +
  ggtitle("Life Expectancy in the Americas in 2007") +
  labs(x = "Life Expectancy (Years)", 
       y = "Country")
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-4-2.png) Conclude: - when using both the arrange() function and the fct\_reorder() function, the fct\_reorder function takes precedence, even when arrange() is called second. - Canada has the highest life expectancy out of all the Americas, while Haiti has the lowest.

### Part 2: File I/O

Explore read\_csv(): Use data downloaded from gapminder.org/data

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
oil_consumption_2007 = oil_consumption %>% 
  select(`Oil Consumption per capita (tonnes per year)`, "2007") %>% 
  mutate(country = `Oil Consumption per capita (tonnes per year)`, 
         tonnes_per_capita_2007 = `2007`) %>% 
  select(country, tonnes_per_capita_2007)

head(oil_consumption_2007) %>% 
  knitr::kable(col.names = c("Country ", "Tonnes per Capita") )
```

| Country    |  Tonnes per Capita|
|:-----------|------------------:|
| Algeria    |          0.3805912|
| Argentina  |          0.5891271|
| Australia  |          1.9759598|
| Austria    |          1.6118351|
| Azerbaijan |          0.5096921|
| Bangladesh |          0.0315428|

``` r
gapminder_2007 = gapminder %>% 
  filter (year == "2007") %>% 
  select(country = "country", pop, gdpPercap, continent) 
```

``` r
gapminder_oil_2007 = left_join(gapminder_2007, oil_consumption_2007, by= "country") %>% 
  na.omit() 
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

``` r
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

gapminder_oil_2007 %>% 
  ggplot(aes(continent, tonnes_per_capita_2007)) + geom_boxplot()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
gapminder_oil_2007$continent = fct_relevel(gapminder_oil_2007$continent, "Americas")

write.csv(gapminder_oil_2007,  "hw5_gapminder_oil_2007.csv")

gapminder_oil_2007 %>% 
  ggplot(aes(continent, tonnes_per_capita_2007), fill =continent ) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.5)+
  scale_fill_brewer(palette="BuPu")
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-7-2.png)

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
  ggplot(aes(tonnes_per_capita_2007, country)) + geom_point()
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
gapminder_oil_2007 %>% 
  ggplot(aes(pop, tonnes_per_capita_2007))+
  geom_point(alpha = 0.33)+
  theme_bw()
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

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
plot_oilvsgdp = gapminder_oil_2007 %>% 
  ggplot(aes(gdpPercap, tonnes_per_capita_2007)) + 
  geom_point(aes(colour = pop)) +
  scale_x_log10(labels = dollar_format())+
  scale_colour_distiller(
    trans="log10",
    breaks = 10^(0:9),
    labels = comma_format(),
    palette = "YlOrRd"
  ) +
  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  theme_bw()+
  ggtitle("Tonnes of Oil per Capita by GDP per Capita ")

plot_oilvsgdp
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-10-2.png)

``` r
ggplotly(plot_oilvsgdp)
```

<!--html_preserve-->

<script type="application/json" data-for="htmlwidget-79469f2f6a5cb18bbeaf">{"x":{"data":[{"x":[3.79402544509666,4.10650977201769,4.53700472145269,4.55782580062218,3.143406361057,3.95740617340857,4.02860349094832,4.56013669242519,4.11963981438709,3.6954041667466,3.84550611104731,4.54750911081685,3.83716291957619,3.74672610693845,4.52123074598147,4.48387269225076,4.50745611553652,4.43993889119905,4.59906367268864,4.25548825822109,4.55847803563392,3.38955773121288,3.54908318988216,4.06467188236304,4.60933819969144,4.45590597950278,4.50045697098462,4.3682522837381,4.67492531406996,4.09522710701315,4.07836889751077,4.56582342810493,4.40114211228811,4.69335042800253,3.41596567540354,3.86975405900188,3.50385616482709,4.18723649435127,4.31195820191596,4.03376444688129,4.33555481758265,4.67341887079622,4.27133768016358,3.96706370233863,4.4597100052953,4.52968372605252,4.57410560181062,4.45815837772449,3.87264545733796,3.92728187211911,4.52118074293136,4.632979883279,4.05750656798335],"y":[0.380591215,0.589127111,1.975959835,1.61183511,0.03154276,0.525743489,0.692317942,3.118106515,0.96964244,0.274536179,0.240298463,1.695516472,0.642814177,0.397184503,2.008818752,1.479151309,1.363257976,1.928086856,2.321354968,0.770320591,3.400786992,0.109479648,0.24242808,1.138649009,2.194228359,1.411440575,1.812484758,2.279249299,5.572449422,0.815091429,0.818101803,2.795082708,1.705707389,2.154276479,0.116657274,0.251232706,0.157481418,0.63297069,1.359078344,0.477618593,3.768907884,10.34192721,0.761557687,0.5280535,1.767247811,1.606221949,1.503683319,2.296677545,0.56337803,0.435654911,1.30125267,3.117270453,1.087567702],"text":["pop: 7.522877<br />gdpPercap:  6223.367<br />tonnes_per_capita_2007:  0.38059121","pop: 7.605326<br />gdpPercap: 12779.380<br />tonnes_per_capita_2007:  0.58912711","pop: 7.310357<br />gdpPercap: 34435.367<br />tonnes_per_capita_2007:  1.97595984","pop: 6.913802<br />gdpPercap: 36126.493<br />tonnes_per_capita_2007:  1.61183511","pop: 8.177387<br />gdpPercap:  1391.254<br />tonnes_per_capita_2007:  0.03154276","pop: 8.278778<br />gdpPercap:  9065.801<br />tonnes_per_capita_2007:  0.52574349","pop: 6.864681<br />gdpPercap: 10680.793<br />tonnes_per_capita_2007:  0.69231794","pop: 7.523618<br />gdpPercap: 36319.235<br />tonnes_per_capita_2007:  3.11810652","pop: 7.211781<br />gdpPercap: 13171.639<br />tonnes_per_capita_2007:  0.96964244","pop: 9.120140<br />gdpPercap:  4959.115<br />tonnes_per_capita_2007:  0.27453618","pop: 7.645693<br />gdpPercap:  7006.580<br />tonnes_per_capita_2007:  0.24029846","pop: 6.737838<br />gdpPercap: 35278.419<br />tonnes_per_capita_2007:  1.69551647","pop: 7.138482<br />gdpPercap:  6873.262<br />tonnes_per_capita_2007:  0.64281418","pop: 7.904524<br />gdpPercap:  5581.181<br />tonnes_per_capita_2007:  0.39718450","pop: 6.719204<br />gdpPercap: 33207.084<br />tonnes_per_capita_2007:  2.00881875","pop: 7.785927<br />gdpPercap: 30470.017<br />tonnes_per_capita_2007:  1.47915131","pop: 7.915932<br />gdpPercap: 32170.374<br />tonnes_per_capita_2007:  1.36325798","pop: 7.029639<br />gdpPercap: 27538.412<br />tonnes_per_capita_2007:  1.92808686","pop: 6.843881<br />gdpPercap: 39724.979<br />tonnes_per_capita_2007:  2.32135497","pop: 6.998090<br />gdpPercap: 18008.944<br />tonnes_per_capita_2007:  0.77032059","pop: 5.479908<br />gdpPercap: 36180.789<br />tonnes_per_capita_2007:  3.40078699","pop: 9.045478<br />gdpPercap:  2452.210<br />tonnes_per_capita_2007:  0.10947965","pop: 8.349369<br />gdpPercap:  3540.652<br />tonnes_per_capita_2007:  0.24242808","pop: 7.841695<br />gdpPercap: 11605.714<br />tonnes_per_capita_2007:  1.13864901","pop: 6.613745<br />gdpPercap: 40675.996<br />tonnes_per_capita_2007:  2.19422836","pop: 7.764533<br />gdpPercap: 28569.720<br />tonnes_per_capita_2007:  1.41144058","pop: 8.105401<br />gdpPercap: 31656.068<br />tonnes_per_capita_2007:  1.81248476","pop: 7.690593<br />gdpPercap: 23348.140<br />tonnes_per_capita_2007:  2.27924930","pop: 6.398905<br />gdpPercap: 47306.990<br />tonnes_per_capita_2007:  5.57244942","pop: 7.394824<br />gdpPercap: 12451.656<br />tonnes_per_capita_2007:  0.81509143","pop: 8.036233<br />gdpPercap: 11977.575<br />tonnes_per_capita_2007:  0.81810180","pop: 7.219339<br />gdpPercap: 36797.933<br />tonnes_per_capita_2007:  2.79508271","pop: 6.614451<br />gdpPercap: 25185.009<br />tonnes_per_capita_2007:  1.70570739","pop: 6.665386<br />gdpPercap: 49357.190<br />tonnes_per_capita_2007:  2.15427648","pop: 8.228582<br />gdpPercap:  2605.948<br />tonnes_per_capita_2007:  0.11665727","pop: 7.457500<br />gdpPercap:  7408.906<br />tonnes_per_capita_2007:  0.25123271","pop: 7.959410<br />gdpPercap:  3190.481<br />tonnes_per_capita_2007:  0.15748142","pop: 7.585666<br />gdpPercap: 15389.925<br />tonnes_per_capita_2007:  0.63297069","pop: 7.027057<br />gdpPercap: 20509.648<br />tonnes_per_capita_2007:  1.35907834","pop: 7.347838<br />gdpPercap: 10808.476<br />tonnes_per_capita_2007:  0.47761859","pop: 7.440925<br />gdpPercap: 21654.832<br />tonnes_per_capita_2007:  3.76890788","pop: 6.658299<br />gdpPercap: 47143.180<br />tonnes_per_capita_2007: 10.34192721","pop: 6.736197<br />gdpPercap: 18678.314<br />tonnes_per_capita_2007:  0.76155769","pop: 7.643431<br />gdpPercap:  9269.658<br />tonnes_per_capita_2007:  0.52805350","pop: 7.606899<br />gdpPercap: 28821.064<br />tonnes_per_capita_2007:  1.76724781","pop: 6.955740<br />gdpPercap: 33859.748<br />tonnes_per_capita_2007:  1.60622195","pop: 6.878215<br />gdpPercap: 37506.419<br />tonnes_per_capita_2007:  1.50368332","pop: 7.365007<br />gdpPercap: 28718.277<br />tonnes_per_capita_2007:  2.29667755","pop: 7.813368<br />gdpPercap:  7458.396<br />tonnes_per_capita_2007:  0.56337803","pop: 7.852228<br />gdpPercap:  8458.276<br />tonnes_per_capita_2007:  0.43565491","pop: 7.783734<br />gdpPercap: 33203.261<br />tonnes_per_capita_2007:  1.30125267","pop: 8.478768<br />gdpPercap: 42951.653<br />tonnes_per_capita_2007:  3.11727045","pop: 7.416385<br />gdpPercap: 11415.806<br />tonnes_per_capita_2007:  1.08756770"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":["rgba(254,171,73,1)","rgba(254,175,75,1)","rgba(254,160,68,1)","rgba(253,139,59,1)","rgba(255,206,106,1)","rgba(254,211,112,1)","rgba(252,134,57,1)","rgba(254,171,73,1)","rgba(254,155,66,1)","rgba(255,255,178,1)","rgba(254,177,76,1)","rgba(250,122,52,1)","rgba(254,152,64,1)","rgba(255,191,90,1)","rgba(250,120,51,1)","rgba(254,185,83,1)","rgba(255,192,91,1)","rgba(253,146,62,1)","rgba(252,132,56,1)","rgba(253,144,61,1)","rgba(189,0,38,1)","rgba(255,251,172,1)","rgba(254,215,116,1)","rgba(254,188,86,1)","rgba(248,109,47,1)","rgba(254,183,82,1)","rgba(255,202,102,1)","rgba(254,179,78,1)","rgba(244,85,39,1)","rgba(254,165,70,1)","rgba(255,198,98,1)","rgba(254,156,66,1)","rgba(248,109,47,1)","rgba(249,114,49,1)","rgba(254,208,109,1)","rgba(254,168,71,1)","rgba(255,194,93,1)","rgba(254,174,74,1)","rgba(253,146,62,1)","rgba(254,162,69,1)","rgba(254,167,71,1)","rgba(249,114,49,1)","rgba(250,122,52,1)","rgba(254,177,76,1)","rgba(254,175,75,1)","rgba(253,142,60,1)","rgba(252,136,58,1)","rgba(254,163,69,1)","rgba(254,186,85,1)","rgba(254,188,87,1)","rgba(254,184,83,1)","rgba(254,221,125,1)","rgba(254,166,70,1)"],"opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":["rgba(254,171,73,1)","rgba(254,175,75,1)","rgba(254,160,68,1)","rgba(253,139,59,1)","rgba(255,206,106,1)","rgba(254,211,112,1)","rgba(252,134,57,1)","rgba(254,171,73,1)","rgba(254,155,66,1)","rgba(255,255,178,1)","rgba(254,177,76,1)","rgba(250,122,52,1)","rgba(254,152,64,1)","rgba(255,191,90,1)","rgba(250,120,51,1)","rgba(254,185,83,1)","rgba(255,192,91,1)","rgba(253,146,62,1)","rgba(252,132,56,1)","rgba(253,144,61,1)","rgba(189,0,38,1)","rgba(255,251,172,1)","rgba(254,215,116,1)","rgba(254,188,86,1)","rgba(248,109,47,1)","rgba(254,183,82,1)","rgba(255,202,102,1)","rgba(254,179,78,1)","rgba(244,85,39,1)","rgba(254,165,70,1)","rgba(255,198,98,1)","rgba(254,156,66,1)","rgba(248,109,47,1)","rgba(249,114,49,1)","rgba(254,208,109,1)","rgba(254,168,71,1)","rgba(255,194,93,1)","rgba(254,174,74,1)","rgba(253,146,62,1)","rgba(254,162,69,1)","rgba(254,167,71,1)","rgba(249,114,49,1)","rgba(250,122,52,1)","rgba(254,177,76,1)","rgba(254,175,75,1)","rgba(253,142,60,1)","rgba(252,136,58,1)","rgba(254,163,69,1)","rgba(254,186,85,1)","rgba(254,188,87,1)","rgba(254,184,83,1)","rgba(254,221,125,1)","rgba(254,166,70,1)"]}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[3.47712125471966],"y":[0],"name":"99_9ca1b73db17aeb3adde2c52193248bbb","type":"scatter","mode":"markers","opacity":0,"hoverinfo":"none","showlegend":false,"marker":{"color":[0,1],"colorscale":[[0,"#BD0026"],[0.0526315789473685,"#CA1725"],[0.105263157894737,"#D82624"],[0.157894736842105,"#E53222"],[0.210526315789474,"#F14121"],[0.263157894736842,"#F55A28"],[0.315789473684211,"#F86F30"],[0.368421052631579,"#FB8237"],[0.421052631578947,"#FD913E"],[0.473684210526316,"#FE9B42"],[0.526315789473684,"#FEA546"],[0.578947368421053,"#FEAE4A"],[0.631578947368421,"#FEB853"],[0.684210526315789,"#FFC35E"],[0.736842105263158,"#FFCD69"],[0.789473684210526,"#FED774"],[0.842105263157895,"#FFE183"],[0.894736842105263,"#FFEB92"],[0.947368421052632,"#FFF5A2"],[1,"#FFFFB2"]],"colorbar":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"thickness":23.04,"title":"pop","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"tickmode":"array","ticktext":["1,000,000","10,000,000","100,000,000","1,000,000,000"],"tickvals":[0.142873363505588,0.417581074018251,0.692288784530915,0.966996495043578],"tickfont":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"ticklen":2,"len":0.5}},"xaxis":"x","yaxis":"y","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":"Tonnes of Oil per Capita by GDP per Capita ","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[3.06590915770972,4.77084763134981],"tickmode":"array","ticktext":["$3,000.00","$10,000.00","$30,000.00"],"tickvals":[3.47712125471966,4,4.47712125471966],"categoryorder":"array","categoryarray":["$3,000.00","$10,000.00","$30,000.00"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":"gdpPercap","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.4839764625,10.8574464325],"tickmode":"array","ticktext":["0","2","4","6","8","10"],"tickvals":[0,2,4,6,8,10],"categoryorder":"array","categoryarray":["0","2","4","6","8","10"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":"tonnes_per_capita_2007","titlefont":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0.66417600664176,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"source":"A","attrs":{"e11a7bf145a0":{"colour":{},"x":{},"y":{},"type":"scatter"}},"cur_data":"e11a7bf145a0","visdat":{"e11a7bf145a0":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":[]}</script>
<!--/html_preserve-->
``` r
  #geom_text(aes(label = country),  hjust = 1, vjust = 2, font = 4, angle = 90))
```

Conclude: There is a trend showing increasing oil usate as GDP per Capita increases. Using the R Colour Distiller, we also notice that countries with a higher population tend towards lower oil usage, while those with a low, or mid-range population size use more oil.

Create a new factor:

``` r
gapminder_2007_sport = gapminder_2007%>% 
  filter(country == "Brazil" | country == "Canada" |country == "France") %>% 
  mutate(sport = factor(c("Capoeira","Hockey", "Soccer")))
  
levels(gapminder_2007_sport$sport)
```

    ## [1] "Capoeira" "Hockey"   "Soccer"

``` r
str(gapminder_2007_sport)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    3 obs. of  5 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 15 21 45
    ##  $ pop      : int  190010647 33390141 61083916
    ##  $ gdpPercap: num  9066 36319 30470
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 2 2 4
    ##  $ sport    : Factor w/ 3 levels "Capoeira","Hockey",..: 1 2 3

### More?
