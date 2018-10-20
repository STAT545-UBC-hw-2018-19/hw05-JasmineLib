STAT545\_hw05\_JasmineLib
================

cowplot gridextratidy

Homework 5: Factor and Figure Management
========================================

``` r
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(gridExtra))
```

### Part 1: Factor Management

#### Task 1.1: Drop a factor

-   remove Oceania.
-   Remove unused factor levels.
-   Provide concrete information before and after removing these rows and the number of levels of affected factors.

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

| Function Applied   | Resulting Continent Factor | Resulting Country Factor |
|--------------------|----------------------------|--------------------------|
| Filter out Oceania | 5 continents               | 142 countries            |
| Drop Levels        | 4 continents               | 140 countries            |

#### Explore the effects of arrange( ).

-   Does merely arranging the data have any effect on, say, a figure?

``` r
#using arrange in a table:
gapminder %>% 
  filter(continent == "Americas" & year == 2007) %>% 
  arrange(desc(lifeExp)) %>% 
  head() %>% 
  knitr::kable()
```

| country       | continent |  year|  lifeExp|        pop|  gdpPercap|
|:--------------|:----------|-----:|--------:|----------:|----------:|
| Canada        | Americas  |  2007|   80.653|   33390141|  36319.235|
| Costa Rica    | Americas  |  2007|   78.782|    4133884|   9645.061|
| Puerto Rico   | Americas  |  2007|   78.746|    3942491|  19328.709|
| Chile         | Americas  |  2007|   78.553|   16284741|  13171.639|
| Cuba          | Americas  |  2007|   78.273|   11416987|   8948.103|
| United States | Americas  |  2007|   78.242|  301139947|  42951.653|

``` r
#before arrange:
gapminder %>% 
  filter(continent == "Americas" & year == 2007) %>% 
  ggplot(aes(lifeExp, country)) + 
  geom_point() +
  ggtitle("Life Expectancy of Countries in Americas in 2007") +
  theme_bw() +
  labs(x = "Life Expectancy (Years)", y = "Country")
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/Arrange%20Effects-1.png)

``` r
#using arrange(desc())
gapminder %>% 
  filter(continent == "Americas" & year == 2007) %>% 
  arrange(desc(lifeExp)) %>% 
  ggplot(aes(lifeExp, country)) + 
  geom_point() +
  ggtitle("Life Expectancy of Countries in Americas in 2007") +
  theme_bw() +
  labs(x = "Life Expectancy (Years)", y = "Country")
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/Arrange%20Effects-2.png)

Conclude:
- using arrange(desc()) works when making tables
- both figures are the same, despite one of them having an arrange(desc()) function
- I conclude that the arrange function has no effect on figures.

#### Explore effects of reordering a factor

-   try this coupled with arrange()
-   How does this affect figure output?

``` r
gap_america_2007 = gapminder %>% 
  filter(continent == "Americas" & year == 2007)

#fct_reorder first, then arrange:

gap_america_2007 %>% 
  mutate(country = fct_reorder(country, lifeExp)) %>% 
  arrange(pop) %>% 
  ggplot(aes(lifeExp, country)) + 
  geom_point(color = "dodgerblue3") +
  theme_bw() +
  ggtitle("Life Expectancy in the Americas in 2007") +
  labs(x = "Life Expectancy (Years)", 
       y = "Country")
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/Reorder%20Factor-1.png)

``` r
#arrange first, then fct_reorder:

gap_america_2007 %>% 
  arrange(pop) %>% 
  mutate(country = fct_reorder(country, lifeExp)) %>% 
  ggplot(aes(lifeExp, country)) + 
  geom_point() +
  geom_point(color = "dodgerblue3") +
  theme_bw() +
  ggtitle("Life Expectancy in the Americas in 2007") +
  labs(x = "Life Expectancy (Years)", 
       y = "Country")
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/Reorder%20Factor-2.png) Conclude:
- Succesfully sorted by life expectancy - The fct\_reorder function sorted by order of life expectancy, while arrange tried to sort by population size.
- when using both the arrange() function and the fct\_reorder() function, the fct\_reorder function takes precedence
- This was the case regardless of the order in which the two functions are called (both graphs are the same) - Canada has the highest life expectancy out of all the Americas, while Haiti has the lowest.

### Part 2: File I/O

-   Explore read\_csv( ):
-   Used data downloaded from gapminder.org/data to read in a file
-   upload data here

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

#check to ensure csv file was properly loaded:
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

-   Next, make a corresponding gapminder subset which we will use to join:

``` r
gapminder_2007 = gapminder %>% 
  filter (year == "2007") %>% 
  select(country = "country", pop, gdpPercap, continent, lifeExp) 
```

-   Perform a join:

``` r
gapminder_oil_2007 = left_join(gapminder_2007, oil_consumption_2007, by= "country") %>% 
  na.omit() 
```

    ## Warning: Column `country` joining factor and character vector, coercing
    ## into character vector

``` r
str(gapminder_oil_2007$continent)
```

    ##  Factor w/ 5 levels "Africa","Americas",..: 1 2 5 4 3 2 4 2 2 3 ...

``` r
str(gapminder_oil_2007$country)
```

    ##  chr [1:53] "Algeria" "Argentina" "Australia" "Austria" "Bangladesh" ...

``` r
nlevels(gapminder$country)
```

    ## [1] 142

Conclude:
- after the join, we have 5 levels of continents, as expected
- after the join, the country factor was changed to a character variable. I will address this later on in the assignment.
- the number of levels is still 142, suggesting that oil use data was complete and available for all countries in gapminder.

#### Further Testing of I/O with help from [Class Notes](http://stat545.com/block026_file-out-in.html#retaining-factor-levels-upon-re-import)

``` r
#first make the country variable into a factor (in order to look at how factors are affected in I/O)
gapminder_oil_2007$country = as.factor(gapminder_oil_2007$country)
#now we check that this is indeed a factor.
str(gapminder_oil_2007$country) 
```

    ##  Factor w/ 53 levels "Algeria","Argentina",..: 1 2 3 4 5 6 7 8 9 10 ...

``` r
test_export = gapminder_oil_2007 %>% 
  mutate(country = fct_reorder(country, tonnes_per_capita_2007))

#make a tibble with original country level
check_order_of_countries = tibble(before_export = head(levels(test_export$country)))
                                  
#write to file using write_csv, saveRDS, and dput: 
write_csv(test_export, "test_export.csv")
saveRDS(test_export, "test_export.rds")
dput(test_export, "test_export_dput.txt")

#import corresponding files using read_csv, readRDS, and dget: 
test_import_csv = read_csv("test_export.csv") %>% 
  mutate(country = factor(country))
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double(),
    ##   continent = col_character(),
    ##   lifeExp = col_double(),
    ##   tonnes_per_capita_2007 = col_double()
    ## )

``` r
test_import_rds = readRDS("test_export.rds")
test_import_dget = dget("test_export_dput.txt")

#look at the first few countries listed to see if the non-alphabetical order "survived" the round trip of writing then reading back in:
check_order_of_countries = check_order_of_countries %>% 
  mutate(imported_by_csv = head(levels(test_import_csv$country)), 
         imported_by_RDS = head(levels(test_import_rds$country)),
         imported_by_dput_dget = head(levels(test_import_dget$country)))
  
check_order_of_countries %>% 
  knitr::kable(col.names = c("Before Export/Import ", "by CSV", "by RDS", "by dput dget"))
```

| Before Export/Import | by CSV     | by RDS      | by dput dget |
|:---------------------|:-----------|:------------|:-------------|
| Bangladesh           | Algeria    | Bangladesh  | Bangladesh   |
| India                | Argentina  | India       | India        |
| Pakistan             | Australia  | Pakistan    | Pakistan     |
| Philippines          | Austria    | Philippines | Philippines  |
| Colombia             | Bangladesh | Colombia    | Colombia     |
| Indonesia            | Brazil     | Indonesia   | Indonesia    |

Conclude:
- using read csv and write csv will NOT preserve changes in factor order
- using read RDS and write RDS will preserve changes in factor order
- using dput and dget will preserve changes in factor order.

### Part 3: Visualization Design

-   Here is an old graph from an earlier assignment:

``` r
 ggplot(gapminder_2007, aes(continent, gdpPercap)) +
  scale_y_log10(labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  geom_violin(fill = "darkslategray1", alpha = 0.5) +
  geom_jitter(alpha = 0.33) +
  xlab("Continent")+
  ylab("GDP Per Capita") +
  ggtitle("GDP Per Capita by Continent in 2007")
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-6-1.png)

-   In this old graph it's hard to tell visually which continent has a higher mean GDP
-   I was not able to tell the difference between America vs. Asia, and Europe vs. Oceania.
-   notice the y-scales are not very easy to read

Using new knowledge, I can make various types of new plots:

``` r
gdpPercap_by_continent_updated = gapminder_2007%>% 
  mutate(continent = fct_reorder(gapminder_2007$continent, gdpPercap)) %>% 
 ggplot( aes(continent, gdpPercap)) +
  scale_y_log10(labels = dollar_format()) +
  geom_violin(fill="wheat", color="slateblue4",alpha = 0.5) +
  geom_jitter(alpha = 0.33) +
  xlab("Continent")+
  ylab("GDP Per Capita") +
  ggtitle("GDP Per Capita by Continent in 2007") +
  theme_bw() 

gdpPercap_by_continent_updated
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-7-1.png)

Changes:
- nicer colours
- updated theme
- y-axis easier to read
- Mean GDP per capita for each continent in ascending order

### Part 3: Visualization Design Continued

Other plots that we have learned to make:

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
  ggtitle("Tonnes of Oil per Capita by GDP per Capita in 2007")+
  labs(y = "Oil Use Per Capita (Tonnes)", x = "GDP Per Capita")

plot_oilvsgdp
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
#Table showing the 5 countries with the lowest oil usage per capita in 2007
gapminder_oil_2007_min = gapminder_oil_2007%>% 
  select (country,tonnes_per_capita_2007) %>% 
  top_n (-5) %>% 
  arrange(tonnes_per_capita_2007)
```

    ## Selecting by tonnes_per_capita_2007

``` r
#Table showing the 5 countries with highest oil usage per capita in 2007 
gapminder_oil_2007_max = gapminder_oil_2007 %>% 
  select (country, tonnes_per_capita_2007) %>% 
  top_n (5) %>% 
  arrange(desc(tonnes_per_capita_2007) )
```

    ## Selecting by tonnes_per_capita_2007

``` r
max_oil_tbl = tableGrob(gapminder_oil_2007_max,  cols = c("Country", "Tonnes per Capita"))
min_oil_tbl = tableGrob(gapminder_oil_2007_min, cols = c("Country", "Tonnes per Capita"))

#use gridarrange to nicely display countries using the most and least oil per capita.
grid.arrange(top="Worldwide Oil Use 2007", plot_oilvsgdp, arrangeGrob(max_oil_tbl, min_oil_tbl, ncol = 2),
             heights=c(2.5/4,1.5/4),ncol = 1)
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-8-2.png)

Conclude:
- There is a trend showing increasing oil usate as GDP per Capita increases.
- Using the R Colour Distiller, we also notice that countries with a higher population tend towards lower oil usage
- Countries with a low, or mid-range population size appear to use more oil. - a plotly graph would be useful for a user to zoom in and see which countries are where.

### Self-contained plotly code:

-   Refer to file called "plotly\_hw05\_JasmineLib.Rmd" to run the plotly graph

### Part 4: Writing figures to file

``` r
plot_oilvsgdp
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggsave("tonnes_per_capita_vs_gdpPercap.jpeg", scale = 1, width = 6, height = 4, units = "in")

gdpPercap_by_continent_updated
```

![](STAT545_hw05_JasmineLib_files/figure-markdown_github/unnamed-chunk-9-2.png)

``` r
ggsave("gdpPercap_by_continent_updated_plot.jpeg", scale = 1, width = 6, height = 4, units = "in")
```

### Bonus - Creating a new factor:

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

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    3 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 15 21 45
    ##  $ pop      : int  190010647 33390141 61083916
    ##  $ gdpPercap: num  9066 36319 30470
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 2 2 4
    ##  $ lifeExp  : num  72.4 80.7 80.7
    ##  $ sport    : Factor w/ 3 levels "Capoeira","Hockey",..: 1 2 3
