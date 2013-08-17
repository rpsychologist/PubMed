require("RCurl")
require("XML")
require("plyr")

#######################
# Download PubMed XML #
#######################

# Search and fetch XML from PubMed
searchPubmed <- function(query.term) {
  # change spaces to + in query
  query.gsub <- gsub(" ", "+", query.term)
  # change single-quotes to URL-friendly %22
  query.gsub <- gsub("'","%22", query.gsub)
  # Perform search and save history, this will save PMIDS in history
  pub.esearch <- getURL(paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=", 
                              query.gsub, "&usehistory=y", sep = ""))
  # Parse esearch XML
  pub.esearch <- xmlTreeParse(pub.esearch, asText = TRUE)
  # Count number of hits (super assign)
  pub.count <<- as.numeric(xmlValue(pub.esearch[["doc"]][["eSearchResult"]][["Count"]]))
  # Save WebEnv-string, it contains "links" to all articles in my search
  pub.esearch <- xmlValue(pub.esearch[["doc"]][["eSearchResult"]][["WebEnv"]])
  # Show how many articles that's being downloaded
  cat("Searching (downloading", pub.count, "articles)\n")
  
  ## We need to batch download, since efetch will cap at 10k articles ##
  # Start at 0
  RetStart <- 0
  # End at 10k
  RetMax <- 10000
  # Calculate how many itterations will be needed
  Runs <- (pub.count %/% 10000) + 1
  # Create empty object
  pub.efetch <- list(NULL)
  # Loop to batch download
  for (i in 1:Runs) { 
    # Download XML based on hits saved in pub.esearch (WebEnv)
    tmp <- getURL(paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&WebEnv=",
                      pub.esearch,"&query_key=1&retmode=xml&retstart=", RetStart, "&retmax=", RetMax, sep = ""))
    pub.efetch[i] <- tmp
    RetStart <- RetStart + 10000
    RetMax <- RetMax + 10000
  }

  # Print that download is completed
  cat("Completed download from PubMed.\n")
  # Return XML
  return(pub.efetch)
}

# Function to extract journal name from individual article
extractJournal <- function(query.term = query) {
  tmp <- lapply(1:length(pub.efetch), function(i) {
    # Parse XML into XML Tree
    xml.data <- xmlTreeParse(pub.efetch[[i]], useInternalNodes = TRUE)
    # Use xpathSApply to extract Journal name
    xpathSApply(xml.data, "//PubmedArticle/MedlineCitation/MedlineJournalInfo/MedlineTA", xmlValue)
   })
  journal <- unlist(tmp)
  # Show how many journals that were extracted
  cat("Extracted ", length(journal), " hits (",(length(journal)/pub.count)*100," %) from a total of ",
      pub.count," hits. For query named: ", query.term,"\n", sep="")
  # Create data frame with journal counts
  journal <- data.frame(count(journal))
  # Calculcate percent
  journal$percent <- journal$freq / pub.count
  # return data
  return(journal)
}