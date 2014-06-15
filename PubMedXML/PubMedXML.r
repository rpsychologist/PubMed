library(RCurl)
library(XML)
library(plyr)
library(stringr)

#' Get a PubMed search index
#' @param query a PubMed search string
#' @return the XML declaration of the search
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

#' Search and fetch XML records from PubMed
#'
#' @param query a PubMed search string
#' @param file a string to name the batch files and folder (the "pubmed_" prefix will be appended)
#' @param list optionally returns a list of details on the data (off by default)
#' @param max optional cap for the number of articles (off by default)
#' @param k how many articles per batch (1,000 by default)
#' @examples
#' query = "FCTC OR 'Framework Convention on Tobacco Control'"
#' # Scrape approx. 230 articles on the WHO FCTC (not run).
#' # pubmed_get(query, "fctc")
#' # Scrape twice to fix possible network errors (not run).
#' # for(ii in 1:2) try(pubmed_get(query, "fctc"))
#' # Scrape safely and save data details (not run).
#' # for(ii in 1:2) FCTC = try(pubmed_get(query, "fctc", list = TRUE))
#' # Plot data details (not run).
#' # require(ggplot2)
#' # qplot(data = FCTC$years, x = year, y = count, stat = "identity", geom = "bar")
#' # qplot(data = FCTC$authors, x = authors, y = count, stat = "identity", geom = "bar")
#' # qplot(data = subset(FCTC$journals, count > 5),
#' #       x = reorder(journal, count), y = count, stat = "identity", geom = "bar") + 
#' #   labs(x = NULL) +
#' #   coord_flip()
#' @reference \url{http://rpsychologist.com/how-to-download-complete-xml-records-from-pubmed-and-extract-data}
pubmed_get <- function(query, file, list = FALSE, max = 0, k = 10^3) {
  
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
  q = pubmed_ask(query)
    
  # count number of hits
  n = pubmed_count(q, max)
  
  # batch download counter
  j = 0
  
  # calculate how many iterations will be needed
  r = (n %/% k) + 1
  
  # print details on the download loop to log file
  cat("Downloading", n, "articles in", r, "batch(es) of", k, "entries\n\n")
  
  # save WebEnv-string, containing "links" to all articles in search
  q = xmlValue(q[["doc"]][["eSearchResult"]][["WebEnv"]])
  
  # batch download loop
  for(i in r:1) { 
    
    x = paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&WebEnv=",
              q,"&query_key=1&retmode=xml&retstart=", j, "&retmax=", k, sep = "")
    
    y = paste0(file, str_pad(i, nchar(n), pad = "0"), ".xml")
    
    if(!file.exists(y)) {
      
      # download XML based on hits saved in query (WebEnv)
      z = try(getURL(x), silent = TRUE)
      
      if(!"try-error" %in% class(z)) {
        
        write(z, y) # save to batch data folder
        
      } else {
        
        warning("Error while downloading batch file #", i)
        Sys.sleep(60) # wait a minute before looping

      }
            
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
      "MB.\n")
  
  if(list)
    return(list(
      date = Sys.time(),
      search = query,
      dir = dir,
      log = log,
      ncid = q,
      count = n,
      years    = pubmed_years(dir, 0),
      journals = pubmed_journals(dir, 0),
      authors  = pubmed_authors(dir, 0)
    ))

}

#' Get journal titles
#'
#' @param min cut results at minimum number of articles
#' @return a data frame
#' @example
#' # Journal titles for articles on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # pubmed_journals("pubmed_fctc")
pubmed_journals <- function(dir, min = 0) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    pre = "//PubmedArticle/MedlineCitation/Article/"
    tbl = xpathSApply(pub, paste0(pre, "Journal/ISOAbbreviation"), xmlValue)
    tbl = table(tbl)
    data.frame(journal = names(tbl), count = as.vector(tbl))
  })
  tbl = rbind.fill(tbl)
  tbl = aggregate(count ~ journal, sum, data = tbl)
  tbl = subset(tbl, count >= min)
  return(tbl[ order(-tbl$count), ])
  
}

#' Get years of publication
#'
#' @param min cut results at minimum number of articles
#' @return a data frame, ordered by year
#' @example
#' # Years of publication for articles on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # pubmed_years("pubmed_fctc", 0)
pubmed_years <- function(dir, min = 0) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    pre = "//PubmedArticle/PubmedData/History/"
    tbl = xpathSApply(pub, paste0(pre, "PubMedPubDate[@PubStatus='medline']/Year"), xmlValue)
    tbl = table(tbl)
    data.frame(year = names(tbl), count = as.vector(tbl))
  })
  tbl = rbind.fill(tbl)
  tbl = aggregate(count ~ year, sum, data = tbl)
  tbl = subset(tbl, count >= min)
  tbl$year = as.numeric(as.character(tbl$year))
  tbl$year = factor(tbl$year, levels = min(tbl$year):max(tbl$year))
  return(tbl[ order(sort(-as.numeric(tbl$year))), ])
  
}

#' Get number of authors
#'
#' @param min cut results at minimum number of articles
#' @return a data frame
#' @example
#' # Number of authors for articles on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # pubmed_authors("pubmed_fctc", 0)
pubmed_authors <- function(dir, min = 0) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    tbl = xpathSApply(pub, "//PubmedArticle/MedlineCitation/Article")
    tbl = lapply(tbl, xpathApply, "AuthorList/Author/LastName")
    tbl = as.vector(sapply(tbl, length))
    tbl = table(tbl)
    data.frame(authors = names(tbl), count = as.vector(tbl), stringsAsFactors = FALSE)
  })
  tbl = rbind.fill(tbl)
  tbl = aggregate(count ~ authors, sum, data = tbl)
  tbl = subset(tbl, count >= min)
  tbl$authors = factor(tbl$authors, levels = 0:max(as.numeric(tbl$authors)))
  return(tbl[ order(as.numeric(tbl$authors)), ])
  
}

#' Get number of publications per journal and year
#'
#' @param min cut results at minimum number of articles
#' @param top label top n journals (5 by default)
#' @param regex regular expression to include additional journal titles
#' @return a data frame, ordered by journal-year
#' @example
#' # Years of publication for articles on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # FCTC = pubmed_timeline("pubmed_fctc", 0, regex = "World Health Org|BMJ $|Lancet|Tob Control")
#' # Plot the results (not run).
#' # require(ggplot2)
#' # qplot(data = FCTC[ order(FCTC$label), ], x = year, y = count, fill = label,
#' #       stat = "identity", geom = "bar") +
#' #   scale_fill_brewer("Journal", palette = "Set3", na.value = "grey") +
#' #   labs(x = NULL) +
#' #   theme(legend.position = "bottom")
pubmed_timeline <- function(dir, min = 0, top = 3, regex = NULL) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    jl = xpathSApply(pub, "//PubmedArticle/MedlineCitation/Article/Journal/ISOAbbreviation", xmlValue)
    yr = xpathSApply(pub, "//PubmedArticle/PubmedData/History/PubMedPubDate[@PubStatus='medline']/Year", xmlValue)
    tbl = table(paste(jl, yr))
    data.frame(journal = names(tbl), count = as.vector(tbl))
  })
  tbl = rbind.fill(tbl)
  tbl = aggregate(count ~ journal, sum, data = tbl)
  tbl = subset(tbl, count >= min)
  tbl = data.frame(journal = str_sub(tbl$journal, 1, -5),
                   year = str_sub(tbl$journal, -4, -1),
                   count = tbl$count)
  tbl$year = as.numeric(as.character(tbl$year))
  
  if(top > 0) {
    
    # find top journals
    label = aggregate(count ~ journal, sum, data = pt)
    label = as.character(label[ order(-label$count), 1 ])
    tbl$label = factor(tbl$journal, levels = label, ordered = TRUE)
    
    # trim and refactor
    label = head(levels(tbl$label), top)
    if(!is.null(regex))
      label = c(label, levels(tbl$label)[ grepl(regex, levels(tbl$label)) ])
    print(label)
    tbl$label[ !tbl$label %in% label ] = NA
    
  }
  return(tbl[ order(sort(-tbl$year)), ])
  
}
