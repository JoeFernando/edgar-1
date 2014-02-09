url.base <- "ftp://ftp.sec.gov/"
tail <- "edgar/data/1084869/0001437749-14-001715.txt"

strip.documents <- function(url){
  lines <- readLines(url)
  doc.start = grep("^<DOCUMENT>", lines)
  doc.end = grep("</DOCUMENT>", lines)
  if((n.docs <- length(doc.start)) != length(doc.end)) stop("Num of doc.starts != Num of doc.ends")
  docs.list <- vector(mode = "list", length = n.docs)
  for(i in 1:n.docs){
    docs.list[[i]] <- lines[doc.start[i] : doc.end[i]]
  }
  docs.list
}


grab.doc.item <- function(item){
  item <- toupper(item)
  tag = paste0("<",item,">")
  out <- function(document){
    string = grep(tag, document, value = T)
    if(identical(character(0), string)) return(NA)
    gsub(tag,"", string)
  } 
}

get.doc.info <- function(document){
  values <- c("type", "sequence", "filename","description")
  doc.info <- data.frame()
  for(i in 1:4){
    doc.info[1,i] <- grab.doc.item(values[i])(document)
  }
  names(doc.info) <- values
  doc.info
}

get.doc.text <- function(document){
  begin.text <- grep("<TEXT>",document)
  end.text <- grep("</TEXT>", document)
  doc.text <- document[(begin.text + 1):(end.text - 1)]
  doc.text
}


extract.doc <- function(document){
  doc.vals <- vector(mode = "list", 2)
  doc.vals[[1]] <- get.doc.info(document)
  doc.vals[[2]] <- get.doc.text(document)
  names(doc.vals) <- c("info", "text")
  doc.vals
}