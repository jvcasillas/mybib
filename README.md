
# mybib

Version controlled .bib file for my articles, proceedings,
presentations, posters, and workshops.

This is a ‘clean’ version. It only contains UTF-8 chars (i.e.  no
`tipa`).

This is **NOT** synchronized with my online CV (for the TODO list).

-----

Last Updated: 2019-08-17 06:09:27

License: Public Domain (CC-0)

I am still testing this. Initial commit is a direct copy of example done
by [leeper](https://github.com/leeper/references).

Here are some basic statistics on its contents:

``` r
library(RefManageR)
library(tidyverse)
library(here)

bib <- suppressWarnings(ReadBib(here("publicationsCVclean.bib"), 
                                check = FALSE))

dat <- bib %>% 
  as_tibble(.) %>% 
  mutate(., year = as.numeric(year))
```

## Citation Types

``` r
counts <- xtabs(~bibtype, data = bib) %>% as.tibble
```

    ## Warning: `as.tibble()` is deprecated, use `as_tibble()` (but mind the new semantics).
    ## This warning is displayed once per session.

``` r
counts %>% 
  mutate(., bibtype = fct_reorder(bibtype, n)) %>% 
  ggplot(., aes(x = bibtype, y = n, label = n)) + 
    geom_bar(stat = 'identity', color = 'black', 
             fill = 'lightblue', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Citation Type") + 
    coord_flip() + 
    my_theme()
```

<img src="https://i.imgur.com/faRsLup.png" width="768" />

## Journals

``` r
datj <- aggregate(bibtype ~ journal, data = bib, FUN = length)

dat %>% 
  group_by(., journal) %>% 
  summarize(., counts = n()) %>% 
  na.omit() %>% 
  mutate(., journal = fct_reorder(journal, counts)) %>% 
  ggplot(., aes(x = journal, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'lightblue', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Journal") + 
    coord_flip() + 
    my_theme()
```

<img src="https://i.imgur.com/SHojI5y.png" width="768" />

## Authors

``` r
# Initialize list
authors <- list()

# For each element in list, get last name of author and store in 
# 'authors' list
for (i in 1:length(bib)) {
  authors[[i]] <- bib[i]$author$family %>% unlist(.)
}

# Convert to tibble and plot
authors %>% 
  unlist(.) %>% 
  as.tibble(.) %>% 
  group_by(., value) %>% 
  summarize(., counts = n()) %>% 
  mutate(., value = fct_reorder(value, counts)) %>% 
  ggplot(., aes(x = value, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'lightblue', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Author") + 
    coord_flip() + 
    my_theme()
```

<img src="https://i.imgur.com/PNzGdpZ.png" width="768" />

## Publication Years

``` r
prod <- dat %>% 
  select(., year) %>% 
  na.omit(.) %>% 
  group_by(., year) %>% 
  summarize(., counts = n()) %>% 
  ungroup(.) %>% 
  mutate(., status = if_else(year <=2012, 'MA\nStudent', 
                             if_else(year <=2016, 'PhD\nStudent', 'Asst.\nProf')), 
            status = fct_relevel(status, 
                                 c('MA\nStudent', 'PhD\nStudent', 'Asst.\nProf')))

year_max <- max(prod$counts)

prod %>% 
  ggplot(., aes(x = year, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'black', width = 0.1) + 
    geom_point(aes(shape = status, fill = status), size = 10, color = 'black') + 
    scale_shape_manual(name = '', values = 21:23) + 
    geom_text(color = 'white') + 
    scale_fill_brewer(name = '', palette = 'Set1') + 
    labs(y = "Count", x = "Year", 
         title = "Productivity as a function of year and status") + 
    expand_limits(y = c(0, year_max + 5)) + 
    my_theme()
```

<img src="https://i.imgur.com/gu2AXvP.png" width="768" />

## H-index stuff

``` r
# Load package
library("scholar")
```

    ## Registered S3 method overwritten by 'R.oo':
    ##   method        from       
    ##   throw.default R.methodsS3

``` r
# Include ID
my_id <- "6sd7cVAAAAAJ"

# Get h-index and citation history
my_h <- predict_h_index(my_id)
my_c <- get_citation_history(my_id)
```

My current h-index is 4. I don’t really know what this means (yet), but
I can preduct how this will grow over the next ten years.

``` r
my_h %>% 
  ggplot(., aes(x = years_ahead, y = h_index)) + 
    geom_path() + 
    geom_point(pch = 24, fill = "grey90", size = 3) + 
    ylim(0, 30) + 
    my_theme()
```

<img src="https://i.imgur.com/ZuqGINd.png" width="768" />

So it looks like I can plan on my h-index improving, but I have no
context (yet) for what this means. I would like to add a few influential
people to the plot to see when I currently fit in in relation to them
and where I would (theoretically) need to be and by when in order to
emulate their career.

Now I will take a look at my citation history.

``` r
my_c %>% 
  ggplot(., aes(x = year, y = cites)) + 
    geom_path() + 
    geom_point(pch = 24, fill = "grey90", size = 3) + 
    ylim(0, 30) + 
    my_theme()
```

<img src="https://i.imgur.com/bWhJNdQ.png" width="768" />

It looks like 2017 was a good year for getting cited.

# Journals

## Already published

  - Journal of Second Language Studies
  - Second Language Research
  - Phonetica
  - Journal of the Acoustical Society of America
  - Journal of Phonetics
  - Bilingualism: Language and Cognition
  - Language and Speech

## Submitted

  - Language Learning
  - Studies in SLA (replication study)

## In prep

  - Journal of Second Language Pronunciation

## On deck

  - International Journal of Bilingualism

## Wishlist

  - Applied Psycholinguistics (10k)
  - Heritage Language Journal
  - Frontiers in Psychology
