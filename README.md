
# mybib

Version controlled .bib file for my articles, proceedings, 
presentations, posters, and workshops.

This is a 'clean' version. It only contains UTF-8 chars (i.e. 
no ```tipa```).

This is **NOT** synchronized with my online CV (for the TODO list).

----



```
## Last Updated: 2017-05-17 12:21:18
```

License: Public Domain (CC-0)




Here are some basic statistics on its contents:


```r
library("RefManageR")
library("ggplot2")
bib <- suppressWarnings(RefManageR::ReadBib("publicationsCVclean.bib", check = FALSE))
dat <- as.data.frame(bib)
dat$year <- as.numeric(dat$year)
dat$journal[is.na(dat$journal)] <- dat$journal[is.na(dat$journal)]
```


## Citation Types


```r
dat$bibtype <- factor(dat$bibtype, levels = names(sort(table(dat$bibtype))))
ggplot(dat, aes(x = bibtype)) + geom_bar() + 
  xlab("Count") + ylab("Citation Type") + coord_flip()
```

![](http://i.imgur.com/gHL2PfP.png)<!-- -->

## Journals


```r
datj <- aggregate(bibtype ~ journal, data = dat, FUN = length)
datj <- head(datj[order(datj$bibtype, decreasing = TRUE), ], 30)
datj$journal <- factor(datj$journal, levels = rev(datj$journal))
ggplot(datj, aes(x = journal, y = bibtype)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Journal") + coord_flip()
```

![](http://i.imgur.com/IqM0YDC.png)<!-- -->

## Authors


```r
aut <- unlist(lapply(unlist(lapply(bib, function(x) unclass(x$author)), recursive = FALSE), `[[`, "family"))
aut <- as.data.frame(head(sort(table(aut), decreasing = TRUE), 50))
aut$aut <- factor(aut$aut, levels = rev(aut$aut))
ggplot(aut, aes(x = aut, y = Freq)) + geom_bar(stat = "identity") + 
  ylab("Count") + xlab("Author Surname") + coord_flip()
```

![](http://i.imgur.com/W0ctoef.png)<!-- -->

## Publication Years


```r
ggplot(dat[dat$year > 1900, ], aes(x = year)) + geom_bar() +
  xlab("Publication Year") + ylab("Count")
```

```
## Warning: Removed 2 rows containing non-finite values (stat_count).
```

![](http://i.imgur.com/5anfpff.png)<!-- -->


