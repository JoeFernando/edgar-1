library(XML)
library(XBRL)
library(plyr)
library(dplyr)
library(reshape2)
library(lubridate)
cik.lookup <- function(company){
  company <- gsub(" ","+", company)
  cik.table <- readHTMLTable(paste0("http://www.sec.gov/cgi-bin/browse-edgar?company=", company, "&owner=include&action=getcompany"))
  if(length(cik.table) == 0){ return(invisible())}
  cik.table[[1]]
}

grab.filing.locs.at <- function(url){
  doc <- htmlParse(url)
  table <- readHTMLTable(doc)[[3]]
  if(is.null(table)) return(NA)
  root <- xmlRoot(doc)
  table$links <- grep("/Archives/",ldply(getNodeSet(root, "//a[@href]"), function(x) xmlAttrs(x)["href"])$href, value = TRUE)
  table[,-which(names(table) == "Format")]
}

gen.url <- function(cik, type, dateb, owner, start, count){
  paste0("http://www.sec.gov/cgi-bin/browse-edgar?action=getcompany",
         "&CIK=",    cik,
         "&type=",   type,
         "&dateb=",  dateb,
         "&owner=",  owner,
         "&start=",  start,
         "&count=",  count)
}


get.filing.locs <- function(cik, type = "", dateb = "", owner = "exclude",start = 0, count = 100, n.tot = -1){
  url <- gen.url(cik, type, dateb, owner, start, count)
  filing.list <- list()
  while(TRUE){
    filings <- grab.filing.locs.at(url)
    if(!is.data.frame(filings)) break
    filing.list[[length(filing.list) + 1]] <- filings
    start <- start + count
    url <- gen.url(cik, type, dateb, owner, start, count)
  }
  rbind.fill(filing.list)
}

get.files <- function(link){
  url <- paste0("http://www.sec.gov",link)
  doc <- htmlTreeParse(url, useInternalNodes = T)
  table <- rbind.fill(readHTMLTable(doc))
  table$link = grep("/Archives/",xpathApply(doc, "//a[@href]", function(x) xmlAttrs(x)["href"]), value = T)
  table
}

xbrl.inst.doc.loc <- function(files) paste0("http://www.sec.gov",subset(files, Description == "XBRL INSTANCE DOCUMENT")$link)

xbrl.schema.doc.loc <- function(files) paste0("http://www.sec.gov",subset(files, Description == "XBRL TAXONOMY EXTENSION SCHEMA DOCUMENT")$link)


getXbrlData <- function(xbrl.inst){
  out <- list()
  doc <- xbrlParse(xbrl.inst)
  out$fct <- xbrlProcessFacts(doc)
  out$cts <- xbrlProcessContexts(doc)
  out$unt <- xbrlProcessUnits(doc)
  out$sch <- xbrlGetSchemaName(doc)
  xbrlFree(doc)
  out
}

get.long.var.name <- function(elementId) gsub("^ | $","",gsub("^.*_|([A-Z][a-z]*)", "\\1 ", as.character(elementId)))

get.var.type <- function(elementId) gsub("(.+)_.+","\\1", as.character(data$fct$elementId))

get.ticker <- function(data) as.character(data$fct$fact[data$fct$elementId == "dei_TradingSymbol"])
