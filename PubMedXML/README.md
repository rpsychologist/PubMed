A [scraper for PubMed articles](http://rpsychologist.com/how-to-download-complete-xml-records-from-pubmed-and-extract-data).

# DEMO

```{S}
# how many?
pubmed_count("FCTC OR 'Framework Convention on Tobacco Control'")
# try out
pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", file = "fctc")
# journals table
pubmed_journals("pubmed_fctc")
# years table
pubmed_years("pubmed_fctc")
```

# HOWTO

The workhorse function is `pubmed_get`, to which you have to pass a Pubmed query and a filename. You might want to run it two consecutive times to fix any network error during the batch download:

```{S}
query = "FCTC OR 'Framework Convention on Tobacco Control'"
for(ii in 1:2)
  try(pubmed_get(query, file = "fctc"))
```

This will download XML records to the `pubmed_fctc` folder, along with the `pubmed_fctc.log` file that contains the initial PubMed query and various additional download details.

Adding the `list = TRUE` runs through some aggregation functions and returns a list of data details, which can take quite some time to complete if you are scraping hundreds of files:

* `date` — timestamp
* `search` — the initial search string
* `dir` — the folder containing the XML records
* `log` — the filename of the batch download log
* `ncid` — the NCID PubMed query identifier
* `count` — the total number of articles
* `years` — the number of articles per year
* `journals` — the number of articles per journal
* `authors` — the number of authors per article

There is one other aggregation function, `pubmed_timeline`, to get counts of articles by year and journal title. This graph shows top and selected journals from the FCTC example above:

![](example.png)

```{S}
FCTC = pubmed_timeline("pubmed_fctc", min = 0,
                       regex = "World Health Org|BMJ $|Lancet|Tob Control")
require(ggplot2)
qplot(data = FCTC[ order(FCTC$label), ], x = year, y = count, fill = label,
      stat = "identity", geom = "bar") +
  scale_fill_brewer("Journal", palette = "Set3", na.value = "grey") +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom")
```