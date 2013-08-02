# create total_table --------------------------------------------------
total_table <- readHTMLTable("http://www.nlm.nih.gov/bsd/medline_cit_counts_yr_pub.html")
total_table <- total_table[1]
total_table <- as.data.frame(total_table)
total_table <- total_table[c(1,2)]
total_table <- total_table[-(67:69),]
# change variable names
names(total_table) <- c("year","total_count")
# remove asterisk
total_table$year <- gsub("\\*","",total_table$year)
total_table$total_count <- gsub(",","",total_table$total_count)
total_table$total_count <- as.numeric(total_table$total_count)
save(total_table, file="total_table")


# Get updated total values by quering PubMed API --------------------------------------------------

getTotalUpdated <- function(yrStart=1950, yrMax=2012) {
  # add progressbar
  pb <- txtProgressBar(min = yrStart, max = yrMax, style = 3)
  # create empty data frame
  df <- data.frame(NULL)
  cat("Getting updated yearly totals\n")
  
  # Start retrieval loop
  for(i in yrStart:yrMax) {
    # tell progressbar how it's going
    setTxtProgressBar(pb, i)
    # create query
    query.parsed <- paste(i, "%5Bppdat%5D", sep="")
    # Get XML
    pub.esearch <- getURL(paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&rettype=count&term=", 
                                query.parsed, sep = ""))
    # Parse XML
    pub.esearch <- xmlTreeParse(pub.esearch, asText = TRUE)
    # Get number of hits from XML
    pub.count <- as.numeric(xmlValue(pub.esearch[["doc"]][["eSearchResult"]][["Count"]]))
    # Don't add anything if count is 0
    if (pub.count != 0) df <- rbind(df, data.frame("year" = i, "total_count" = pub.count))
    # Wait 0.5 sec
    Sys.sleep(0.5)
  }
  # close progressbar
  close(pb)
  # We want year as numeric. 
  df$year <- as.numeric(df$year)
  return(df)
}
total_table_updated <- getTotalUpdated(1947,2012)
save(file="total_table_updated", total_table_updated)


# compare counts from table with counts from my script -----------------------------------------
load(file="total_table")
load(file="total_table_updated")
df <- rbind(total_table, total_table_updated)
df$id <- gl(2,nrow(total_table), labels=c("original", "updated"))
df$year <- as.numeric(df$year)

# plot comparison
ggplot(df, aes(year, total_count, group=id, color=id)) + 
  geom_line() +
  geom_point() + 
  labs(title = "Pubmed total count (yearly)",
       x = "Publication year",
       y = "Total hits") +
  scale_x_continuous(breaks=seq(1900,2012, 5))

