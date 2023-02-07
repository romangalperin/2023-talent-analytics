Exercise 4
================

## Setting up data and environment

We first need to do a few things before we can manipulate the data.

``` r
# set path for R to find our data
data_path = "~/Dropbox/McGill/teaching/2022-2023/ORGB671/data/"
```

### Load data saved in exercise 2

We’ll load application data with the additional fields for gender, race
and tenure.

``` r
app_data <- read_parquet(paste0(data_path,"apps_gender_rate.parquet"))
```

## Objectives

### 1. Research how the composition of and art unit at time *t* affects likelihood of examiner transition to another art unit at time *t+1*

- Consider different aspects of the composition: size, gender ratio,
  race representation, seniority distribution of peers
- Optionally, consider different (heterogeneous) effects by focal
  examiner’s gender, race and seniority
- Clearly explain your causal model, the assumptions it depends on and
  the limitations of your inference

#### Identify movers and years when they move

We’ll aggregate art units data on annual level for each examiner, to
flag when they move.

``` r
movers <- app_data |> 
  mutate(
    app_year = year(filing_date),
    ) |>  
  group_by(examiner_id, app_year) |> 
  summarise(examiner_art_unit = min(examiner_art_unit, na.rm = TRUE)) |> # eliminate duplicates
  arrange(examiner_id, app_year, examiner_art_unit) |> # sort for lag()
  group_by(examiner_id) |> 
  mutate(moved_aus = if_else(examiner_art_unit!=lag(examiner_art_unit),1,0)) |>
  mutate(moved_aus = if_else(is.na(moved_aus) & app_year==min(app_year),0,moved_aus)) #fixing NAs in examiner's first year
```

    ## `summarise()` has grouped output by 'examiner_id'. You can override using the
    ## `.groups` argument.

``` r
movers
```

    ## # A tibble: 56,836 × 4
    ## # Groups:   examiner_id [5,649]
    ##    examiner_id app_year examiner_art_unit moved_aus
    ##          <dbl>    <dbl>             <dbl>     <dbl>
    ##  1       59012     2004              1717         0
    ##  2       59012     2006              1716         1
    ##  3       59012     2007              1716         0
    ##  4       59012     2008              1716         0
    ##  5       59012     2010              1716         0
    ##  6       59025     2009              2465         0
    ##  7       59025     2010              2465         0
    ##  8       59025     2011              2465         0
    ##  9       59025     2012              2465         0
    ## 10       59025     2013              2465         0
    ## # … with 56,826 more rows

``` r
# check the distribution of number of moves
movers |> group_by(examiner_id) |> summarise(total_moves = sum(moved_aus, na.rm = TRUE)) |> count(total_moves)
```

    ## # A tibble: 10 × 2
    ##    total_moves     n
    ##          <dbl> <int>
    ##  1           0  3438
    ##  2           1  1121
    ##  3           2   553
    ##  4           3   295
    ##  5           4   121
    ##  6           5    80
    ##  7           6    19
    ##  8           7    15
    ##  9           8     6
    ## 10          11     1

It looks like some people are moving almost every year. This is unlikely
to reflect actual underlying process and more likely results from some
data noise: for example, art unit may have been split into two, and
applications got reclassified retrospectively. In any case, it’s
unlikely to be useful information for us.

To address this problem in a way that keeps the information in the data
but doesn’t include those records in the estimations, let’s define an
estimation sample indicator `est_sample`. We’ll only include those who
didn’t move for at least three years.

``` r
movers <- movers |> 
  group_by(examiner_id) |> 
  mutate(
    est_sample = if_else(n() - sum(moved_aus)>2,1,0)
    )

movers
```

    ## # A tibble: 56,836 × 5
    ## # Groups:   examiner_id [5,649]
    ##    examiner_id app_year examiner_art_unit moved_aus est_sample
    ##          <dbl>    <dbl>             <dbl>     <dbl>      <dbl>
    ##  1       59012     2004              1717         0          1
    ##  2       59012     2006              1716         1          1
    ##  3       59012     2007              1716         0          1
    ##  4       59012     2008              1716         0          1
    ##  5       59012     2010              1716         0          1
    ##  6       59025     2009              2465         0          1
    ##  7       59025     2010              2465         0          1
    ##  8       59025     2011              2465         0          1
    ##  9       59025     2012              2465         0          1
    ## 10       59025     2013              2465         0          1
    ## # … with 56,826 more rows

``` r
movers |> ungroup() |> count(est_sample) |> mutate(pct = n/sum(n))
```

    ## # A tibble: 2 × 3
    ##   est_sample     n    pct
    ##        <dbl> <int>  <dbl>
    ## 1          0  1283 0.0226
    ## 2          1 55553 0.977

#### Create art units dataset

Because we’ll be using characteristics of each art unit as predictors,
let’s calculate them first, for each art unit in each year. For brevity,
we’ll just consider gender. The same code can be extended to include
race, seniority, etc.

``` r
au_data <- app_data |> 
  mutate(
    app_year = year(filing_date),
    examiner_male = if_else(gender == "male",1,0),
    examiner_female = if_else(gender == "female",1,0),
    ) |> 
  group_by(examiner_art_unit, app_year) |> 
  distinct(examiner_id, .keep_all = TRUE) |> 
  summarise(
    au_size = n(),
    au_nfemale = sum(examiner_female, na.rm = TRUE),
    au_nmale = sum(examiner_male, na.rm = TRUE)
            )
```

    ## `summarise()` has grouped output by 'examiner_art_unit'. You can override using
    ## the `.groups` argument.

``` r
au_data
```

    ## # A tibble: 4,915 × 5
    ## # Groups:   examiner_art_unit [291]
    ##    examiner_art_unit app_year au_size au_nfemale au_nmale
    ##                <dbl>    <dbl>   <int>      <dbl>    <dbl>
    ##  1              1600     2000       1          0        1
    ##  2              1600     2001       1          0        1
    ##  3              1600     2002       2          1        1
    ##  4              1600     2003       3          2        1
    ##  5              1600     2004       2          1        1
    ##  6              1600     2005       1          0        1
    ##  7              1600     2006       1          0        1
    ##  8              1600     2007       3          2        1
    ##  9              1600     2008       1          1        0
    ## 10              1600     2010       1          1        0
    ## # … with 4,905 more rows

We are getting very small art unit sizes for some AU-year cells. For
now, let’s only consider art units with 5 or more examiners.

``` r
au_data <- au_data |> 
  filter(au_size>=5)

au_data
```

    ## # A tibble: 4,483 × 5
    ## # Groups:   examiner_art_unit [284]
    ##    examiner_art_unit app_year au_size au_nfemale au_nmale
    ##                <dbl>    <dbl>   <int>      <dbl>    <dbl>
    ##  1              1609     2003       9          6        3
    ##  2              1609     2004      13          5        7
    ##  3              1609     2005       9          3        4
    ##  4              1609     2006       6          2        3
    ##  5              1611     2001      10          7        3
    ##  6              1611     2002      12          8        3
    ##  7              1611     2003      16         10        5
    ##  8              1611     2004      20         11        8
    ##  9              1611     2005      23         11       11
    ## 10              1611     2006      22         12        9
    ## # … with 4,473 more rows

#### Estimate regressions

Let’s join the art unit information to our individual-level panel and
estimate the correlations between art unit size, share of women and
moves. Note that we need to have lagged values as predictors. We could
either “shift” all predictors (like art unit size) forward in time, or
our outcome (move indicator) back in time for one year. It’s easier to
do the latter, so we’ll create a “will_move” indicator for year *t* that
will be equal to 1 if the examiner moves next year (at *t+1*).

``` r
reg_sample <- movers |> 
  filter(est_sample==1) |> 
  left_join(au_data) |> 
  select(-est_sample) |> 
  group_by(examiner_id) |> 
  mutate(
    will_move = if_else(lead(moved_aus)==1,1,0),
    share_female = au_nfemale / au_size, # we'll use this below
    ) |> 
  ungroup()
```

    ## Joining, by = c("app_year", "examiner_art_unit")

``` r
reg_sample
```

    ## # A tibble: 55,553 × 9
    ##    examiner_id app_year examiner_art_unit moved_aus au_size au_nfemale au_nmale
    ##          <dbl>    <dbl>             <dbl>     <dbl>   <int>      <dbl>    <dbl>
    ##  1       59012     2004              1717         0       8          2        6
    ##  2       59012     2006              1716         1      18          6       11
    ##  3       59012     2007              1716         0      19          7       11
    ##  4       59012     2008              1716         0      19          7       11
    ##  5       59012     2010              1716         0      21          7       12
    ##  6       59025     2009              2465         0      19          3       13
    ##  7       59025     2010              2465         0      18          3       12
    ##  8       59025     2011              2465         0      19          3       12
    ##  9       59025     2012              2465         0      19          3       12
    ## 10       59025     2013              2465         0      17          2       11
    ## # … with 55,543 more rows, and 2 more variables: will_move <dbl>,
    ## #   share_female <dbl>

Note that we’ll be automatically dropping the last observation for each
examiner, because `will_move` indicator is set to NA for last observed
year.

Let’s run some regressions. We’ll start with a linear probability model
(LMP) that is simply an OLS with a binary outcome. It’s robust enough
for rough estimations, although a better model to use would be logit.

``` r
library(modelsummary)
models <- list()
models[['m1']] <- lm(will_move ~ 1 + au_size, data = reg_sample) 
models[['m2']] <- lm(will_move ~ 1 + au_size + share_female, 
         data = reg_sample) 
modelsummary(models)
```

|              |    m1     |    m2     |
|:-------------|:---------:|:---------:|
| (Intercept)  |   0.015   |   0.031   |
|              |  (0.003)  |  (0.003)  |
| au_size      |   0.003   |   0.003   |
|              |  (0.000)  |  (0.000)  |
| share_female |           |  -0.059   |
|              |           |  (0.008)  |
| Num.Obs.     |   50335   |   50335   |
| R2           |   0.016   |   0.017   |
| R2 Adj.      |   0.016   |   0.017   |
| AIC          |  10556.0  |  10507.7  |
| BIC          |  10582.5  |  10543.1  |
| Log.Lik.     | -5274.996 | -5249.873 |
| F            |  825.302  |  438.189  |
| RMSE         |   0.27    |   0.27    |

### 2. Find a factor that may be considered exogenous (i.e., random) and improve your inference

- Use Diff-in-diff or IV, as you wish
- Clearly explain the logic of your research design
- Hints: other people’s moves may be exogenous; workload may be
  exogenous; types of applicants are exogenous because of random
  assignment of applications to examiners
