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

The workhorse function is `pubmed_get`. You might want to run it twice in a single spell to fix any network error during the batch download:

```{S}
query = "FCTC OR 'Framework Convention on Tobacco Control'"
for(ii in 1:2)
  try(pubmed_get(query, file = "fctc"))
```

This will return the raw data to the `pubmed_fctc` folder, along with a download log `pubmed_fctc.log` that contains the full PubMed query.

Adding the `list = TRUE` runs through some aggregation functions and returns a list of data details, which can take some time to complete:

* `date` — timestamp
* `search` — the initial search string
* `dir` — the folder containing the XML records
* `log` — the filename of the batch download log
* `ncid` — the NCID PubMed query identifier
* `count` — the total number of articles
* `years` — the number of articles per year
* `journals` — the number of articles per journal
* `authors` — the number of authors per article

# TODO

* protect `pubmed_names` against problematic records
* journal-year count function
