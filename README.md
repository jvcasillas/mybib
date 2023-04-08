
# mybib

**Last Updated**: 2023-04-08 14:13:43  
**License**: Public Domain (CC-0)

Version controlled .bib files for my scholarly work, as well as some
light analyses and a complete reference list.

This repo now includes two main bib files: **publications_html.bib** and
**publications_latex.bib**.

I call these files in to other projects (e.g., cv, personal website)
when I want to print a list of references.

Initial commit is a direct copy of example done by
[leeper](https://github.com/leeper/references).

------------------------------------------------------------------------

Load bibs and generate some useful files and dataframes:

``` r
# Load bib
bib <- suppressWarnings(ReadBib(here("publications_html.bib"), 
                                check = FALSE))

# Create csv of citekeys
cite_key_list <- bind_cols(
  bib$key %>% unlist %>% tibble::enframe(name = NULL), 
  bib$bibtype %>% unlist %>% tibble::enframe(name = NULL), 
  bib$year %>% unlist %>% tibble::enframe(name = NULL)
  ) %>% 
  rename(citekey = value...1, type = value...2, year = value...3) %>% 
  write_csv(here("cite_key_list.csv"))

# Set bib opions for printing
BibOptions(bib.style = "authoryear", style = "text", max.names = 10, 
           first.inits = TRUE, check.entries = FALSE)

# Convert to dataframe for analyses
dat <- bib %>% 
  as_tibble(.) %>% 
  map_df(.f = HTMLdecode) %>% 
  mutate(year = as.numeric(year))
```

And now some basic statistics on its contents:

## Citation Types

``` r
counts <- xtabs(~bibtype, data = bib) %>% as_tibble

counts %>% 
  mutate(., bibtype = fct_reorder(bibtype, n)) %>% 
  ggplot(., aes(x = bibtype, y = n, label = n)) + 
    geom_bar(stat = 'identity', color = 'black', 
             fill = 'darkred', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Citation Type") + 
    coord_flip() + 
    my_theme()
```

    ## Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
    ## ℹ Please use the `linewidth` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

<img src="README_files/figure-gfm/bibtype-1.png" width="768" />

## Journals

``` r
datj <- aggregate(bibtype ~ journal, data = bib, FUN = length)

dat %>% 
  group_by(., journal) %>% 
  summarize(., counts = n(), .groups = "drop") %>% 
  na.omit() %>% 
  mutate(., journal = fct_reorder(journal, counts)) %>% 
  ggplot(., aes(x = journal, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'darkred', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Journal") + 
    coord_flip() + 
    my_theme()
```

<img src="README_files/figure-gfm/journal-1.png" width="768" />

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
map(authors, HTMLdecode) %>% 
  unlist(.) %>% 
  enframe(.) %>% 
  group_by(., value) %>% 
  summarize(., counts = n(), .groups = "drop") %>% 
  mutate(., value = fct_reorder(value, counts)) %>% 
  ggplot(., aes(x = value, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'darkred', width = 0.1) + 
    geom_point(pch = 21, size = 10, color = 'black', fill = 'lightgrey') + 
    geom_text() + 
    labs(y = "Count", x = "Author") + 
    coord_flip() + 
    my_theme()
```

<img src="README_files/figure-gfm/authors-1.png" width="768" />

## Publication Years

``` r
prod <- dat %>% 
  select(., year) %>% 
  na.omit(.) %>% 
  group_by(., year) %>% 
  summarize(., counts = n(), .groups = "drop") %>% 
  ungroup(.) %>% 
  mutate(., status = if_else(year <=2012, 'MA\nStudent', 
                             if_else(year <=2016, 'PhD\nStudent', 
                                     'Asst.\nProf')), 
            status = fct_relevel(status, 
                                 c('MA\nStudent', 'PhD\nStudent', 
                                   'Asst.\nProf')))

year_max <- max(prod$counts)
year_current <- prod$year %>% unique %>% max

prod %>% 
  ggplot(., aes(x = year, y = counts, label = counts)) + 
    geom_bar(stat = "identity", color = 'black', 
             fill = 'black', width = 0.1) + 
    geom_point(aes(shape = status, fill = status), size = 10, color = 'black') + 
    scale_shape_manual(name = '', values = 21:23) + 
    scale_x_continuous(breaks = seq(2009, year_current, 1)) + 
    geom_text(color = 'white') + 
    scale_fill_brewer(name = '', palette = 'Set1') + 
    labs(y = "Count", x = "Year", 
         title = "Productivity as a function of year and status") + 
    expand_limits(y = c(0, year_max + 5)) + 
    my_theme()
```

<img src="README_files/figure-gfm/year-1.png" width="768" />

# Google scholar data

## H-index stuff

``` r
# Include ID
my_id <- "6sd7cVAAAAAJ"
ms_id <- "GnYMTI8AAAAJ"

miguel <- "12mgD38AAAAJ"

ids <- c(my_id, ms_id)
compare_scholars(ids)
```

    ##              id year cites total               name
    ## 1  6sd7cVAAAAAJ 2011    12    12 Joseph V. Casillas
    ## 2  6sd7cVAAAAAJ 2012     3    15 Joseph V. Casillas
    ## 3  6sd7cVAAAAAJ 2013    20    35 Joseph V. Casillas
    ## 4  6sd7cVAAAAAJ 2014    20    55 Joseph V. Casillas
    ## 5  6sd7cVAAAAAJ 2015    38    93 Joseph V. Casillas
    ## 6  6sd7cVAAAAAJ 2016    44   137 Joseph V. Casillas
    ## 7  6sd7cVAAAAAJ 2017     1   138 Joseph V. Casillas
    ## 8  6sd7cVAAAAAJ 2018    38   176 Joseph V. Casillas
    ## 9  6sd7cVAAAAAJ 2020    47   223 Joseph V. Casillas
    ## 10 6sd7cVAAAAAJ 2021    10   233 Joseph V. Casillas
    ## 11 6sd7cVAAAAAJ 2022     1   234 Joseph V. Casillas
    ## 12 6sd7cVAAAAAJ 2023     0   234 Joseph V. Casillas
    ## 13 6sd7cVAAAAAJ   NA     0   234 Joseph V. Casillas
    ## 14 GnYMTI8AAAAJ 2005     5     5     Miquel Simonet
    ## 15 GnYMTI8AAAAJ 2006     9    14     Miquel Simonet
    ## 16 GnYMTI8AAAAJ 2007     0    14     Miquel Simonet
    ## 17 GnYMTI8AAAAJ 2008   169   183     Miquel Simonet
    ## 18 GnYMTI8AAAAJ 2009    49   232     Miquel Simonet
    ## 19 GnYMTI8AAAAJ 2010   202   434     Miquel Simonet
    ## 20 GnYMTI8AAAAJ 2011   308   742     Miquel Simonet
    ## 21 GnYMTI8AAAAJ 2012   186   928     Miquel Simonet
    ## 22 GnYMTI8AAAAJ 2013     0   928     Miquel Simonet
    ## 23 GnYMTI8AAAAJ 2014   162  1090     Miquel Simonet
    ## 24 GnYMTI8AAAAJ 2015    81  1171     Miquel Simonet
    ## 25 GnYMTI8AAAAJ 2016    32  1203     Miquel Simonet
    ## 26 GnYMTI8AAAAJ 2017     8  1211     Miquel Simonet
    ## 27 GnYMTI8AAAAJ 2018    46  1257     Miquel Simonet
    ## 28 GnYMTI8AAAAJ 2019    10  1267     Miquel Simonet
    ## 29 GnYMTI8AAAAJ 2020    36  1303     Miquel Simonet
    ## 30 GnYMTI8AAAAJ 2021     9  1312     Miquel Simonet
    ## 31 GnYMTI8AAAAJ 2022     6  1318     Miquel Simonet
    ## 32 GnYMTI8AAAAJ 2023     1  1319     Miquel Simonet
    ## 33 GnYMTI8AAAAJ   NA     0  1319     Miquel Simonet

``` r
compare_scholar_careers(ids)
```

    ##              id year cites career_year               name
    ## 1  6sd7cVAAAAAJ 2013     1           0 Joseph V. Casillas
    ## 2  6sd7cVAAAAAJ 2014     2           1 Joseph V. Casillas
    ## 3  6sd7cVAAAAAJ 2015     3           2 Joseph V. Casillas
    ## 4  6sd7cVAAAAAJ 2016     8           3 Joseph V. Casillas
    ## 5  6sd7cVAAAAAJ 2017    11           4 Joseph V. Casillas
    ## 6  6sd7cVAAAAAJ 2018    14           5 Joseph V. Casillas
    ## 7  6sd7cVAAAAAJ 2019    16           6 Joseph V. Casillas
    ## 8  6sd7cVAAAAAJ 2020    47           7 Joseph V. Casillas
    ## 9  6sd7cVAAAAAJ 2021    53           8 Joseph V. Casillas
    ## 10 6sd7cVAAAAAJ 2022    46           9 Joseph V. Casillas
    ## 11 6sd7cVAAAAAJ 2023    15          10 Joseph V. Casillas
    ## 12 GnYMTI8AAAAJ 2007     4           0     Miquel Simonet
    ## 13 GnYMTI8AAAAJ 2008     5           1     Miquel Simonet
    ## 14 GnYMTI8AAAAJ 2009     6           2     Miquel Simonet
    ## 15 GnYMTI8AAAAJ 2010    19           3     Miquel Simonet
    ## 16 GnYMTI8AAAAJ 2011    42           4     Miquel Simonet
    ## 17 GnYMTI8AAAAJ 2012    43           5     Miquel Simonet
    ## 18 GnYMTI8AAAAJ 2013    64           6     Miquel Simonet
    ## 19 GnYMTI8AAAAJ 2014    57           7     Miquel Simonet
    ## 20 GnYMTI8AAAAJ 2015    99           8     Miquel Simonet
    ## 21 GnYMTI8AAAAJ 2016   103           9     Miquel Simonet
    ## 22 GnYMTI8AAAAJ 2017   112          10     Miquel Simonet
    ## 23 GnYMTI8AAAAJ 2018    84          11     Miquel Simonet
    ## 24 GnYMTI8AAAAJ 2019   128          12     Miquel Simonet
    ## 25 GnYMTI8AAAAJ 2020   191          13     Miquel Simonet
    ## 26 GnYMTI8AAAAJ 2021   156          14     Miquel Simonet
    ## 27 GnYMTI8AAAAJ 2022   159          15     Miquel Simonet
    ## 28 GnYMTI8AAAAJ 2023    27          16     Miquel Simonet

``` r
# Get h-index and citation history
my_h <- predict_h_index(my_id) %>% mutate(author = "jvc")
my_c <- get_citation_history(my_id) %>% mutate(author = "jvc")

# Game same info for MS
ms_h <- predict_h_index(ms_id) %>% mutate(author = "ms")
ms_c <- get_citation_history(ms_id) %>% mutate(author = "ms")

mj_h <- predict_h_index(miguel) %>% mutate(author = "miguel")
mj_c <- get_citation_history(miguel) %>% mutate(author = "miguel")
```

My current h-index is 10. I don’t really know what this means (yet), but
I can predict how this will grow over the next ten years.

``` r
my_h %>% 
  ggplot(., aes(x = years_ahead, y = h_index)) + 
    geom_hline(yintercept = ms_h[1, 2], lty = 3) + 
    geom_path() + 
    geom_point(pch = 24, fill = "grey90", size = 3) + 
    ylim(0, max(my_h$h_index) + 5) + 
    my_theme()
```

<img src="README_files/figure-gfm/h-plot-1.png" width="768" />

``` r
ms_h %>% 
  ggplot(., aes(x = years_ahead, y = h_index)) + 
    geom_hline(yintercept = ms_h[1, 2], lty = 3) + 
    geom_path() + 
    geom_point(pch = 24, fill = "grey90", size = 3) + 
    ylim(0, max(mj_h$h_index) + 5) + 
    my_theme()
```

<img src="README_files/figure-gfm/h-plot-2.png" width="768" />

So it looks like I can plan on my h-index improving, but I have no
context (yet) for what this means. I would like to add a few influential
people to the plot to see where I currently fit in in relation to them.
This might be a useful metric for setting goals.

``` r
# Plot h-index side by side
h_index_1 <- bind_rows(my_h, ms_h, mj_h) %>% 
  ggplot(., aes(x = years_ahead, y = h_index, shape = author, color = author)) + 
    geom_path() + 
    geom_point(fill = "grey90", size = 3) + 
    scale_color_viridis_d(option = "C", end = 0.4) + 
    scale_shape_manual(values = 21:23) + 
    labs(title = "Comparison of h-index", 
         subtitle = "Predicted h-index values over 10 years.") + 
    my_theme() + 
    theme(legend.position = c(0.15, 0.88))

# Fit a model and plot trajectories
combined_h <- bind_rows(my_h, ms_h) %>% 
  spread(author, h_index)
coefs <- lm(ms ~ jvc, data = combined_h) %>% coef

h_index_2 <- combined_h %>% 
  ggplot(., aes(x = jvc, y = ms)) + 
    geom_abline(intercept = coefs[1], slope = coefs[2], lty = 3) + 
    geom_path() + 
    geom_point(pch = 24, fill = "grey90", size = 3) + 
    my_theme()

h_index_1 + h_index_2
```

<img src="README_files/figure-gfm/compare-h-1.png" width="768" />

So it looks like MS has an overall higher h-index and the function shows
us as having more or less similar growth over time.

## Citations

Now I will take a look at my citation history.

``` r
my_c %>% 
  ggplot(., aes(x = year, y = cites)) + 
    geom_path() + 
    geom_point(pch = 24, fill = "grey90", size = 3) + 
    scale_x_continuous(breaks = seq(min(my_c$year), max(my_c$year), 1)) + 
    coord_cartesian(ylim = c(0, 0.9 * sum(my_c$cites))) + 
    labs(y = "Citations", x = "Year") + 
    my_theme()
```

<img src="README_files/figure-gfm/citation-history-1.png" width="768" />

``` r
my_c %>% 
  mutate(cum = cumsum(cites)) %>% 
  ggplot(., aes(x = year, y = cum)) + 
    geom_path() + 
    geom_point(pch = 24, fill = "grey90", size = 3) + 
    scale_x_continuous(breaks = seq(min(my_c$year), max(my_c$year), 1)) + 
    coord_cartesian(ylim = c(0, sum(my_c$cites) + 10)) + 
    labs(y = "Cumulative citations", x = "Year") + 
    my_theme()
```

<img src="README_files/figure-gfm/citation-history-2.png" width="768" />

Note that these are cumulative citations by year.

Let’s plot this in comparison to MS.

``` r
bind_rows(my_c, ms_c, mj_c) %>% 
  ggplot(., aes(x = year, y = cites, shape = author, color = author)) + 
    geom_path() + 
    geom_point(aes(fill = author), size = 3.5, color = "white", stroke = 1) + 
    scale_shape_manual(name = NULL, values = 21:23) + 
    scale_color_viridis_d(name = NULL, option = "C", end = 0.4) + 
    scale_fill_viridis_d(name = NULL, option = "C", end = 0.4) + 
    scale_x_continuous(breaks = seq(min(ms_c$year), max(my_c$year), 2)) + 
    coord_cartesian(ylim = c(0, max(mj_c$cites) + 20)) + 
    labs(y = "Citations", x = "Year") + 
    my_theme()
```

<img src="README_files/figure-gfm/combined-citations-1.png" width="768" />

Both MS and I had bumps in year 5, but they aren’t even remotely
comparable. 😳

------------------------------------------------------------------------

# Journals

## Already published or accepted

- Journal of Second Language Studies
- Second Language Research
- Phonetica
- Journal of the Acoustical Society of America
- Journal of Phonetics
- Bilingualism: Language and Cognition
- Language and Speech
- Language Learning
- Studies in SLA
- Spanish in Context
- Languages
- Translation, Cognition & Behavior
- Advances in Methods and Practices in Psychological Science
- Frontiers in Psychology
- Frontiers in Communication

## Submitted

- Applied Psycholinguistics (10k)
- Frontiers in Psychology

## In prep

- JASA

## On deck

- Journal of Second Language Pronunciation
- International Journal of Bilingualism

## Wishlist

- Heritage Language Journal
- Linguistic approaches to bilingualism
- Laboratory phonology

------------------------------------------------------------------------

# My references (testing)

## Articles

``` r
# Printer function for refs
ref_printer <- function(pubs) {
  
  for (i in pubs) {
    print(bib[key = i]); cat("\n")
  }
  
}
```

``` r
# Filter citekey dataframe, convert to vector and use to subset bib
filter(cite_key_list, type == "Article") %>% 
  arrange(desc(year) )%>% 
  pull(citekey) %>% 
  ref_printer
```

Jiménez-Crespo, M. and J. V. Casillas (2021). “Literal is not always
easier: Literal and default translation, post-editing effort, and
comparable corpora”. In: *Translation, Cognition & Behavior* 4.1. DOI:
10.1075/tcb.00048.jim.

Casillas, J. V. (2021). “Interlingual Interactions Elicit Performance
Mismatches Not ‘Compromise’ Categories in Early Bilinguals: Evidence
from Meta-Analysis and Coronal Stops”. In: *Languages* 6.9, pp. 1-20.
DOI: 10.3390/languages6010009.

Lozano-Argüelles, C., L. F. Arroyo, N. Rodr'iguez, E. Durand, J. J. G.
Pozu, J. M. Rojas, J. Varela, N. de Rocafiguera, and J. V. Casillas
(2021). “Conceptually cued perceptual categorization in adult L2
learners”. In: *Studies in Second Language Acquisition* 43.1, pp.
204-219. DOI: 10.1017/S0272263120000273.

Casillas, J. V. (2020). “The longitudinal development of fine-phonetic
detail: Stop production in a domestic immersion program”. In: *Language
Learning* 70.3, pp. 768-806. DOI: <https://doi.org/10.1111/lang.12392>.

Casillas, J. V. (2020). “Phonetic category formation is perceptually
driven during the early stages of adult L2 development”. In: *Language
and Speech* 63.3, pp. 550-581. DOI: 10.1177/0023830919866225.

Lozano-Argüelles, C., N. Sagarra, and J. V. Casillas (2020). “Slowly but
surely: Interpreting facilitates L2 morphological anticipation based on
suprasegmental and segmental information”. In: *Bilingualism: Language
and Cognition* 23.4, pp. 752-762. DOI: 10.1017/S1366728919000634.

Casillas, J. V. and M. Simonet (2018). “Perceptual categorization and
bilingual language modes: Assessing the double phonemic boundary in
early and late bilinguals”. In: *Journal of Phonetics* 71, pp. 51-64.
DOI: 10.1016/j.wocn.2018.07.002.

Sagarra, N. and J. V. Casillas (2018). “Suprasegmental information cues
morphological anticipation during L1/L2 lexical access”. In: *Journal of
Second Language Studies* 1.1, pp. 31-59. DOI: 10.1075/jsls.17026.sag.

Bessett, R. M., J. V. Casillas, and M. Ramírez Martínez (2017).
“Language choice and accommodation: Casual encounters in San Ysidro and
Nogales”. In: *Spanish in Context* 14.1, pp. 78-98. DOI:
10.1075/sic.14.1.04bes.

Casillas, J. V. (2017). “Reseña de Lacorte, Manel. 2014. The Routledge
Handbook of Hispanic Applied Linguistics. New York: Routledge.” In:
*Infoling* 2.43. <http://infoling.org/informacion/Review230.htm>.

Llompart, M. and J. V. Casillas (2016). “Lexically driven selective
adaptation by ambiguous auditory stimuli occurs after limited exposure
to adaptors”. In: *Journal of the Acoustical Society of America* 139.5,
pp. EL172-EL177. DOI: 10.1121/1.4951704.

Casillas, J. V. and M. Simonet (2016). “Production and perception of the
English /æ/-/ɑ/ contrast in switched-dominance speakers”. In: *Second
Language Research* 32.2, pp. 171-195. DOI: 10.1177/0267658315608912.

Casillas, J. V. (2015). “Production and perception of the /i/-/ɪ/ vowel
contrast: The case of L2-dominant early learners of English”. In:
*Phonetica* 72.2-3, pp. 182-205. DOI: 10.1159/000431101.

Casillas, J. V. (2012). “La fricativización del africado /tʃ/ en el
habla de las mujeres del sur de Arizona”. In: *Divergencias: Revista de
estudios lingü'isticos y literarios* 10.1, pp. 56-70.

Casillas, J. V. (2010). “La vibrante múltiple intervocálica: los
ejercicios de canto como ayuda a su pronunciación en español”. In: *La
Gaceta Hispánica de Madrid* VIII. ISSN: 1886-1741.

Casillas, J. V. (2009). “El uso de los refranes en El Quijote”. In: *La
Gaceta Hispánica de Madrid* VIII. ISSN: 1886-1741.
