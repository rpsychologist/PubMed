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
#' @return a data frame
#' @example
#' # Journal titles for articles on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # pubmed_journals("pubmed_fctc")
pubmed_journals <- function(dir, min = 3) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    pre = "//PubmedArticle/MedlineCitation/Article/"
    tbl = xpathSApply(pub, paste0(pre, "Journal/ISOAbbreviation"), xmlValue)
    tbl = table(tbl)[ table(tbl) >= min ]
    data.frame(journal = names(tbl), count = tbl)
  })
  print(tbl)
  tbl = rbind.fill(tbl)
  tbl = aggregate(count ~ journal, sum, data = tbl)
  return(tbl[ order(-tbl$count), ])
  
}

#' Get years of publication
#'
#' @param min cut results at minimum number of articles
#' @return a data frame
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
    tbl = table(tbl)[ table(tbl) >= min ]
    data.frame(year = names(tbl), count = tbl)
  })
  tbl = rbind.fill(tbl)
  return(aggregate(count ~ year, sum, data = tbl))
  
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
    tbl = table(tbl)[ table(tbl) >= min ]
    data.frame(authors = names(tbl), count = tbl, stringsAsFactors = FALSE)
  })
  tbl = rbind.fill(tbl)
  tbl = aggregate(count ~ authors, sum, data = tbl)
  tbl$authors = factor(tbl$authors, levels = 0:max(as.numeric(tbl$authors)))
  return(tbl[ order(tbl$authors), ])
  
}

#' Get counts of authors (by name)
#'
#' @param min cut results at minimum number of articles
#' @return a data frame
#' @example
#' # Names of authors for articles on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # pubmed_names("pubmed_fctc", 6)
pubmed_names <- function(dir, min = 0) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    tbl = xpathSApply(pub, "//PubmedArticle/MedlineCitation/Article")
    last = lapply(tbl, xpathSApply, "AuthorList/Author/LastName", xmlValue)
    init = lapply(tbl, xpathSApply, "AuthorList/Author/Initials", xmlValue)
    stopifnot(length(unlist(last)) == length(unlist(init)))
    tbl = paste(unlist(last), substr(unlist(init), 1, 1))
    tbl = table(tbl)[ table(tbl) >= min ]
    data.frame(author = names(tbl), count = tbl, stringsAsFactors = FALSE)
  })
  tbl = rbind.fill(tbl)
  tbl = aggregate(count ~ author, sum, data = tbl)
  return(tbl[ order(-tbl$count), ])
  
}

#' Get undirected edge list of coauthors
#'
#' The weights are Newman-Fowler (inversely proportional to number of coauthors).
#' @return a data frame with three columns (sender, receiver, weight)
#' @example
#' # # Network of authors on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # n = pubmed_edges("pubmed_fctc")
#' 
#' # # Plot with network package (install first; not run).
#' # require(network)
#' # plot(network(n[ 1:2 ], directed = FALSE))
#' 
#' # # Plot with ggnet (install devtools package first; not run).
#' 
#' # net = network(n[, 1:2])
#' # net %e% "w" = n[, 3] / max(n[ , 3])
#' # net %e% "q" = as.numeric(cut(net %e% "w", quantile(net %e% "w"), include.lowest = TRUE)) / 4
#' # require(devtools)
#' # source_url("https://raw.githubusercontent.com/briatte/ggnet/master/ggnet.R")
#' # ggnet(net, segment.alpha = net %e% "q",
#' #       segment.color = "black", size = 1) + 
#' #   geom_text(label = ifelse(degree(net) > quantile(degree(net), .9),
#' #                            network.vertex.names(net), NA), size = 4)
pubmed_edges <- function(dir) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    tbl = xpathSApply(pub, "//PubmedArticle/MedlineCitation")
    tbl = lapply(tbl, function(x) {
      y = paste(xpathSApply(x, "Article/AuthorList/Author/LastName", xmlValue),
                xpathSApply(x, "Article/AuthorList/Author/Initials", xmlValue))
      if(length(y) > 1) {
        y = expand.grid(y, y)
        y = subset(y, Var1 != Var2) # self-loops
        y = unique(apply(y, 1, function(x) paste(sort(x), collapse = ",")))
        y = ldply(strsplit(y, ","))
        y = data.frame(xpathApply(x, "PMID", xmlValue), y, 1 / nrow(y))
        names(y) = c("pmid", "i", "j", "w")
      } else {
        y = data.frame()
      }
      return(y)
    })
    tbl = rbind.fill(tbl)
    write.csv(tbl, gsub("xml", "csv", x))
    return(tbl)
  })
  tbl = rbind.fill(tbl)
  tbl$uid = apply(tbl[, 2:3], 1, function(x) paste(sort(x), collapse = ","))
  
  # Newman-Fowler weights
  tbl = merge(tbl, aggregate(w ~ uid, sum, data = tbl), by = "uid")
  tbl = unique(tbl[, c("i", "j", "w.y") ])
  names(tbl)[3] = "w"

  return(tbl)
  
}

