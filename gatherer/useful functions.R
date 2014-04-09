library(XML, quietly = FALSE, warn.conflicts = FALSE)
library(XBRL, quietly = FALSE, warn.conflicts = FALSE)
library(plyr, quietly = FALSE, warn.conflicts = FALSE)
library(dplyr, quietly = FALSE, warn.conflicts = FALSE)
library(reshape2, quietly = FALSE, warn.conflicts = FALSE)
library(lubridate, quietly = FALSE, warn.conflicts = FALSE)

## ---- cik.lookup ----
cik.lookup <- function(company, quietly = TRUE){
  company <- gsub(" ","+", company)
  cik.table <- readHTMLTable(paste0("http://www.sec.gov/cgi-bin/browse-edgar?company=", company, "&owner=include&action=getcompany"))
  if(length(cik.table) == 0){
    if(!quietly) warning("No results found.")
    return(invisible())
  }
  cik.table[[1]]
}

## ---- get.filing.locs ----
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

## ---- get.files ----
get.files <- function(link){
  url <- paste0("http://www.sec.gov",link)
  doc <- htmlTreeParse(url, useInternalNodes = T)
  table <- rbind.fill(readHTMLTable(doc))
  table$link = grep("/Archives/",xpathApply(doc, "//a[@href]", function(x) xmlAttrs(x)["href"]), value = T)
  table
}

xbrl.inst.doc.loc <- function(files) paste0("http://www.sec.gov",subset(files, Description == "XBRL INSTANCE DOCUMENT")$link)

## ---- getXbrlData ----

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


## ---- other.funs ----
create.megaframe <- function(xbrlData) Reduce(f = function(...) merge(..., all = T), x = xbrlData[-4])

niceify <- function(useful.frame){
  store <- vector("list", nrow(useful.frame))
  for(i in 1:length(store)){
    x <- useful.frame[i,]
    if(is.na(x$startDate)){
      store[[i]] <- data.frame(variable = x$variable, fact = x$fact, date = x$endDate)
    }else{
      store[[i]] <- data.frame(variable = x$variable, fact = x$fact, date = x$startDate + days(0:(x$endDate - x$startDate)))
    }
  }
  useful <- rbind.fill(store)
  dcast(useful, date ~ variable, value.var = "fact")
}

get.long.var.name <- function(elementId) gsub("^ | $","",gsub("^.*_|([A-Z][a-z]*)", "\\1 ", as.character(elementId)))

get.var.type <- function(elementId) gsub("(.+)_.+","\\1", as.character(data$fct$elementId))

get.ticker <- function(data) as.character(data$fct$fact[data$fct$elementId == "dei_TradingSymbol"])

xbrl.schema.doc.loc <- function(files) paste0("http://www.sec.gov",subset(files, Description == "XBRL TAXONOMY EXTENSION SCHEMA DOCUMENT")$link)
