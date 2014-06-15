#' Get counts of authors (by name)
#'
#' @param min cut results at minimum number of articles
#' @param first use first initial only in authors identification
#' @return a data frame
#' @example
#' # Names of authors for articles on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # pubmed_names("pubmed_fctc", 6)
pubmed_names <- function(dir, min = 0, first = FALSE) {
  
  tbl = file.path(dir, dir(dir, ".xml"))
  tbl = lapply(tbl, function(x) {
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    tbl = xpathSApply(pub, "//PubmedArticle/MedlineCitation/Article")
    last = lapply(tbl, xpathSApply, "AuthorList/Author/LastName", xmlValue)
    init = lapply(tbl, xpathSApply, "AuthorList/Author/Initials", xmlValue)
    stopifnot(length(unlist(last)) == length(unlist(init)))
    if(first)
      tbl = paste(unlist(last), substr(unlist(init), 1, 1))
    else
      tbl = paste(unlist(last), unlist(init))
    tbl = table(tbl)
    data.frame(author = names(tbl), count = as.vector(tbl), stringsAsFactors = FALSE)
  })
  tbl = rbind.fill(tbl)
  tbl = aggregate(count ~ author, sum, data = tbl)
  tbl = subset(tbl, count > min)
  return(tbl[ order(-tbl$count), ])
  
}

#' Get undirected edge list of coauthors
#'
#' The weights are Newman-Fowler (inversely proportional to number of coauthors).
#' \textbf{Caution:} this function requires inordinate amounts of disk space to
#' complete on large corpora (something like 4GB is recommended for a corpus of
#' 200,000 articles).
#' @return a data frame with three columns (sender, receiver, weight)
#' @example
#' # # Network of authors on the WHO FCTC (not run).
#' # pubmed_get("FCTC OR 'Framework Convention on Tobacco Control'", "fctc")
#' # n = pubmed_edges("pubmed_fctc")
#' # # Plot with network package (install first; not run).
#' # require(network)
#' # plot(network(n[ 1:2 ], directed = FALSE))
#' # # Plot with ggnet (install devtools package first; not run).
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
  
  dir = file.path(dir, dir(dir, ".xml"))
  get = dir[ !file.exists(gsub("xml", "csv", dir)) ]

  cat("Converting", length(get), "files to edge lists ...")

  for(x in get) {
    
    pub = xmlTreeParse(x, useInternalNodes = TRUE)
    pub = xpathSApply(pub, "//PubmedArticle/MedlineCitation")
    
    pub = lapply(pub, function(x) {
      
      y = paste(xpathSApply(x, "Article/AuthorList/Author/LastName", xmlValue),
                xpathSApply(x, "Article/AuthorList/Author/Initials", xmlValue))
      y = unique(y)
      
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
    
    pub = rbind.fill(pub)
    write.csv(pub, gsub("xml", "csv", x))
        
  }
  
  dir = gsub("xml", "csv", dir)
  cat(" done:", sum(file.exists(dir)), "edge lists,",
      as.integer(sum(file.info(dir)$size) / 10^6),
      "MB.")

}
