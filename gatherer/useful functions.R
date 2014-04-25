library(XML, quietly = TRUE, warn.conflicts = FALSE)
library(XBRL, quietly = TRUE, warn.conflicts = FALSE)
library(plyr, quietly = TRUE, warn.conflicts = FALSE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(reshape2, quietly = TRUE, warn.conflicts = FALSE)
library(lubridate, quietly = TRUE, warn.conflicts = FALSE)
library(parallel, quietly = TRUE, warn.conflicts = FALSE)
suppressMessages(library(quantmod, quietly = TRUE, warn.conflicts = FALSE))

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

xbrl.inst.doc.loc <- function(files){
  link <- subset(files, Description == "XBRL INSTANCE DOCUMENT")
  if(nrow(link) == 0) return(NA)
  paste0("http://www.sec.gov",link$link)
} 

## ---- getXbrlData ----

getXbrlData <- function(xbrl.inst){
  to.be.merged <- list()
  doc <- xbrlParse(xbrl.inst)
  to.be.merged $fct <- xbrlProcessFacts(doc)
  to.be.merged $cts <- xbrlProcessContexts(doc)
  to.be.merged $unt <- xbrlProcessUnits(doc)
  xbrlFree(doc)
  mf <- merge.list(to.be.merged)
  mf$startDate <- ymd(mf$startDate)
  mf$endDate <- ymd(mf$endDate)
  mf$ticker <- get.ticker(to.be.merged)
  mf
}

## ---- other.funs ----
# Deprecated. See merge.list
#create.megaframe <- function(x) Reduce(f = function(...) merge(..., all = T), x = x)

merge.list <- function(x){
  if(length(x) < 2) return(x[[1]])
  Reduce(f = function(...) merge(..., all = T), x)
} 

# niceify.row <- function(x){
#   name <- x$variable
#   df <- data.frame(variable = x$variable, fact = x$fact, date = if(is.na(x$startDate)){x$startDate}else{x$startDate + days(0:(x$endDate - x$startDate))})
#   names(df)[1] <- x$variable
#   df
# }
# 
# niceify <- function(useful.frame){
#   store <- list()
#   for(i in 1:nrow(useful.frame)){
#     store[[i]] <- niceify.row(useful.frame[i,])
#   }
#   useful <- rbind.fill(store)
#   dcast(useful, date ~ variable, value.var = 'fact', fun.aggregate = function(x) x[1])
# }


# This function sucks. Replace it with something less awful asap.
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
  # Note: If there are more than 1 obs, then you can use it to check for consistency. Perhaps this can be used as a mechanism to validate data?
  dcast(useful, date ~ variable, value.var = "fact", fun.aggregate = function(x) x[1])
}

produce.frame <- function(xbrlData, variables = "."){
  out <- list()
  xbrlData.t <- xbrlData[grep(variables, xbrlData$elementId),]
  ticker <- xbrlData$ticker
  xbrlData <- xbrlData.t[,-which(names(xbrlData) == "ticker")]
  out$keyframe <- unique(xbrlData.t[, which(names(xbrlData) %in% c("elementId", "fact", "decimals", "startDate", "endDate", "factId"))])
  out$keyframe$context.key <- paste0(c(1L:nrow(out$keyframe)),ticker[1])
  mid.frame <- merge(xbrlData.t, out$keyframe)
  mid.frame$variable <- paste0(mid.frame$elementId,"-",mid.frame$context.key)
  out$data <- niceify(useful.frame = mid.frame[,c("variable", "fact", "startDate", "endDate")])
  out
}

pull.10K <- function(cik, from = as.Date("1990-01-01"), to = today()){
  fls <- get.filing.locs(cik = cik, type = "10-K")
  dates <- as.Date(fls$`Filing Date`)
  used.rows <- (dates > from) & (dates < to)
  fls <- fls[used.rows,]
  xbrlData <- list()
  for(i in 1:nrow(fls)){
    files <- get.files(fls$links[i])
    xbrl.inst <- xbrl.inst.doc.loc(files)
    if(is.na(xbrl.inst)) break
    xbrlData[[i]] <- getXbrlData(xbrl.inst)
  }
  rbind.fill(xbrlData)
}


xbrl.cn <- function(xbrlDataList, variables){
  if(is.null(xbrlDataList)) return(NULL)
  if(length(xbrlDataList) == 0) return(NULL)
  post.nice.data <- list()
  post.nice.keyframe <- list()
  out <- list()
  for(i in 1:length(xbrlDataList)){
    this <- produce.frame(xbrlDataList[[i]], variables)
    post.nice.data[[i]] <- this$data
    post.nice.keyframe[[i]] <- this$keyframe
  }
  out$data <- merge.list(post.nice.data)
  out$keyframe <- merge.list(post.nice.keyframe)
  out
}

fix.getSymbols <- function(x) data.frame(coredata(x), date = ymd(index(x)))

get.long.var.name <- function(elementId) gsub("^ | $","",gsub("^.*_|([A-Z][a-z]*)", "\\1 ", as.character(elementId)))

get.var.type <- function(elementId) gsub("(.+)_.+","\\1", as.character(data$fct$elementId))

get.ticker <- function(data) as.character(data$fct$fact[data$fct$elementId == "dei_TradingSymbol"])

xbrl.schema.doc.loc <- function(files) paste0("http://www.sec.gov",subset(files, Description == "XBRL TAXONOMY EXTENSION SCHEMA DOCUMENT")$link)
