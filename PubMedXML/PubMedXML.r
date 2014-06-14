library(RCurl)
library(XML)
library(plyr)
library(stringr)

#' Get a PubMed search index
#' @param query a PubMed search string
#' @example
#' # Which articles discuss the WHO FCTC?
#' pubmed_ask("FCTC OR 'Framework Convention on Tobacco Control'")
pubmed_ask <- function(query) {
  
  # change spaces to + and single-quotes to URL-friendly %22 in query
  query = gsub("'", "%22", gsub(" ", "+", query))
  
  query = paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=", 
                query, "&usehistory=y", sep = "")
  
  cat("Querying PubMed on", as.character(Sys.time()), ":\n", query, "\n\n")
  
  # perform search and save history, this will save PMIDS in history
  query = xmlTreeParse(getURL(query), asText = TRUE)
  
  cat(xmlValue(query[["doc"]][["eSearchResult"]][["QueryTranslation"]]), "\n\n")
  
  return(query)
  
}

#' Get PubMed number of articles
#' @param query a PubMed search string or PubMed search index
#' @param max optional cap for the number of articles
#' @example
#' # How many articles on the WHO FCTC?
#' pubmed_count("FCTC OR 'Framework Convention on Tobacco Control'")
pubmed_count <- function(query, max = 0) {
  
  if(is.character(query))
    query = pubmed_ask(query)
  
  stopifnot("XMLDocument" %in% class(query))

  n = as.numeric(xmlValue(query[["doc"]][["eSearchResult"]][["Count"]]))
  
  # cap number of articles (for testing purposes; off by default)
  if(max > 0 & max < n)
    n = max
  
  return(n)

}

#' Search and fetch XML from PubMed
#'
#' @param query a PubMed search string
#' @param file the batch file name
#' @param max optional cap for the number of articles
#' @param k how many articles per batch
#' @example
#' # Scrape approx. 230 articles on the WHO FCTC (not run)
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' @reference \url{http://rpsychologist.com/how-to-download-complete-xml-records-from-pubmed-and-extract-data}
pubmed_get <- function(query, file, max = 0, k = 10^3) {
  
  stopifnot(max >= 0)
  dir = paste0("pubmed_", file)
  
  # where to save the files (batch folder)
  if(!file.exists(dir))
    dir.create(dir, showWarnings = FALSE)
  
  # batch file paths and log file path
  file = file.path(dir, file)
  log = file.path(paste0(dir, ".log"))
  
  sink(log)
  
  # change spaces to + and single-quotes to URL-friendly %22 in query
  query = pubmed_ask(query)
    
  # count number of hits
  n = pubmed_count(query, max)
  
  # batch download counter
  j = 0
  
  # calculate how many iterations will be needed
  r = (n %/% k) + 1
  
  # print details on the download loop to log file
  cat("Downloading", n, "articles in", r, "batch(es) of", k, "entries\n\n")
  
  # save WebEnv-string, containing "links" to all articles in search
  query = xmlValue(query[["doc"]][["eSearchResult"]][["WebEnv"]])
  
  # batch download loop
  for(i in r:1) { 
    
    x = paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&WebEnv=",
              query,"&query_key=1&retmode=xml&retstart=", j, "&retmax=", k, sep = "")
    
    y = paste0(file, str_pad(i, nchar(n), pad = "0"), ".xml")
    
    if(!file.exists(y)) {
      
      # download XML based on hits saved in query (WebEnv)
      z = getURL(x)
      
      # write to batch data folder
      write(z, y)
      
    }
    
    # save file name and file size to log file
    cat(i, "\n")
    cat(x, "\nsaved to", y, as.character(file.info(y)$ctime), "\n\n")
    
    # increment to next batch
    j = j + k
    
  }
  
  sink()
  
  # announce final file size
  cat("Completed download from PubMed:", n, "articles",
      as.integer(sum(file.info(file.path(dir, dir(dir, "xml")))$size) / 10^6),
      "MB")  
  
}

#' Get journal titles
#'
#' @param min cut results at minimum number of articles
#' @return a table
#' @example
#' # Years of publication for articles on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # pubmed_journals("pubmed_fctc")
pubmed_journals <- function(dir, min = 3) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    pre = "//PubmedArticle/MedlineCitation/Article/"
    xpathSApply(pub, paste0(pre, "Journal/ISOAbbreviation"), xmlValue)
  })
  tbl = as.data.frame(table(tbl)[ table(tbl) > min ])
  return(tbl[ order(-tbl), ])
  
}

#' Get years of publication
#'
#' @param min cut results at minimum number of articles
#' @return a table
#' @example
#' # Years of publication for articles on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # pubmed_years("pubmed_fctc", 0)
pubmed_years <- function(dir, min = 3) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    pre = "//PubmedArticle/MedlineCitation/Article/"
    xpathSApply(pub, paste0(pre, "Journal/JournalIssue/PubDate/Year"), xmlValue)
  })
  tbl = table(tbl)[ table(tbl) > min ]
  return(tbl)
  
}
