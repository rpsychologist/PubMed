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
