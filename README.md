---
output: 
  html_document: 
    keep_md: yes
---

# mybib

Version controlled .bib file for my articles, proceedings, 
presentations, posters, and workshops.

This is a 'clean' version. It only contains UTF-8 chars (i.e. 
no ```tipa```).

This is **NOT** synchronized with my online CV (for the TODO list).

----



```
## Last Updated: 2018-05-17 00:42:50
```

License: Public Domain (CC-0)

I am still testing this. Initial commit is a direct copy of example done by 
[@leeper](https://github.com/leeper/references). 




Here are some basic statistics on its contents:


```r
library(RefManageR)
library(tidyverse)
```

```
## ── Attaching packages ─────────────── tidyverse 1.2.1 ──
```

```
## ✔ ggplot2 2.2.1.9000     ✔ purrr   0.2.4     
## ✔ tibble  1.4.2          ✔ dplyr   0.7.4     
## ✔ tidyr   0.8.0          ✔ stringr 1.3.1     
## ✔ readr   1.1.1          ✔ forcats 0.3.0
```

```
## ── Conflicts ────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
```

```r
library(here)
```

```
## here() starts at /Users/casillas/academia/research/mybib
```

```r
bib <- suppressWarnings(ReadBib(here("publicationsCVclean.bib"), 
                                check = FALSE))

dat <- bib %>% 
  as.tibble(.) %>% 
  mutate(., year = as.numeric(year))
```

```
## Warning in evalq(as.numeric(year), <environment>): NAs introduced by
## coercion
```

```r
# dat$year <- as.numeric(dat$year)
# dat$journal[is.na(dat$journal)] <- dat$journal[is.na(dat$journal)]
```



## Citation Types


```r
counts <- xtabs(~bibtype, data = bib) %>% as.tibble

counts %>% 
  mutate(., bibtype = fct_reorder(bibtype, n)) %>% 
  ggplot(., aes(x = bibtype, y = n, label = n)) + 
    geom_bar(stat = 'identity', color = 'black', 
             fill = 'lightblue', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Citation Type") + 
    coord_flip() + 
    theme_test()
```

<img src="https://i.imgur.com/JfssOiG.png" width="768" />

## Journals


```r
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
    theme_test()
```

<img src="https://i.imgur.com/d163VSn.png" width="768" />

## Authors


```r
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
    theme_test()
```

<img src="https://i.imgur.com/SuSwQAf.png" width="768" />

## Publication Years


```r
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
             fill = 'lightblue', width = 0.1) + 
    geom_point(aes(shape = status, fill = status), size = 10, color = 'black') + 
    scale_shape_manual(name = '', values = 21:23) + 
    geom_text(color = 'white') + 
    scale_fill_brewer(name = '', palette = 'Set1') + 
    labs(y = "Count", x = "Year", 
         title = "Productivity as a function of year and status") + 
    expand_limits(y = c(0, year_max + 5))
```

<img src="https://i.imgur.com/nE6hUzx.png" width="768" />


```r
unlink("cache", recursive = TRUE)
unlink("figure", recursive = TRUE)
```
