library(XML)
library(plyr)

# A function that will take a string (pattern) and search for where it occurs in
# another string (to.search)
find.starting.char <- function(pattern, to.search, what = "first"){
  np <- nchar(pattern)
  if(np > (nts <- nchar(to.search))) return(NA)
  all.subs <- as.character(mapply(substr, to.search, c(1:(nts - np + 1)), c(np:nts)))
  if(what == "first") return(which(all.subs == pattern)[1])
  which(all.subs == pattern)
}

# A funciton to trim the whitespace off the end of a character vector
trim.ws<- function(x) gsub("[ ]*$", "", x)

# A function that will get all filings for all companies within a given year and quarter
get.quarter.index <- function(year, qtr){
  url.base <- c("ftp://ftp.sec.gov/edgar/full-index/")
  url <- paste(url.base, year,"/QTR", qtr, "/company.idx", sep = "")
  header.line <- as.character(read.table(url, skip = 8, nrows = 1, sep = "\t")[[1]])
  headers <- c("Company Name", "Form Type", "CIK", "Date Filed", "File Name")
  begin <- mapply(find.starting.char, headers, header.line)
  all.data <- read.fwf(url, skip = 10, widths = c(diff(as.vector(begin)), 100), comment.char = "")
  names(all.data) <- headers
  apply(all.data, 2, trim.ws)
}


