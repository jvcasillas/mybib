
# mybib

Version controlled .bib file for my articles, proceedings, 
presentations, posters, and workshops.

This is a 'clean' version. It only contains UTF-8 chars (i.e. 
no ```tipa```).

This is **NOT** synchronized with my online CV (for the TODO list).

----



```
## Last Updated: 2017-05-17 12:11:31
```

License: Public Domain (CC-0)




Here are some basic statistics on its contents:


```r
library("RefManageR")
library("ggplot2")
bib <- suppressWarnings(RefManageR::ReadBib("publicationsCVclean.bib", check = FALSE))
dat <- as.data.frame(bib)
dat$year <- as.numeric(dat$year)
# dat$journal[is.na(dat$journal)] <- dat$journaltitle[is.na(dat$journal)]
```
