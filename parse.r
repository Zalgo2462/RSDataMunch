library(XML)
library(RCurl)
library(zoo)
library(jsonlite)
library(parallel)

getRSDataByName <- function(name) {
  html <- getURL(paste('http://runescape.wikia.com/wiki/Exchange:',name, sep=''));
  html <- htmlTreeParse(html, asText = TRUE, useInternal = TRUE);
  text <- unlist(xpathApply(html, '//div[@class="GEdataprices"]', function (el) xmlGetAttr(el, 'data-data')));
  text <- unlist(strsplit(text, '|', fixed = TRUE));
  
  parseRSWikiGE <- function(text) {
    components <- unlist(strsplit(text, ':', fixed = TRUE));
    c(as.numeric(components[1]), as.integer(components[2]), as.integer(components[3]));
  }
  text <- sapply(text, parseRSWikiGE);
  data <- matrix(text, ncol = 3, byrow = TRUE);
  colnames(data) <- c("Date", "Price", "Volume");
  data <- data[!duplicated(data[, "Date"]), ];
  
  data <- zoo(data[, c("Price", "Volume")], sapply(data[, "Date"], function (ts) as.Date(as.POSIXct(ts, origin="1970-1-1"))));
  colnames(data) <- c("Price", "Volume");
  data <- na.approx(data)
  data <- na.locf(data, na.rm = FALSE);
  data <- na.locf(data, na.rm = FALSE, fromLast = TRUE);
  comment(data) <- name;
  data;
}


getRSItemNames <- function(file) {
  json <- paste(readLines(file), collapse=" ");
  data <- fromJSON(json);get
  gsub(" ", "_", data$name);
}

writeRSData <- function(itemZoo, destFolder) {
  write.zoo(itemZoo, paste(destFolder, comment(itemZoo), sep="/"));
}

loadRSData <- function(itemName, folder) {
  r <- read.table(paste(folder, itemName, sep="/"), header = TRUE);
  r <- read.zoo(r);
  comment(r) <- itemName;
  r;
}

cacheCurrentData <- function(itemsFile, folder) {
  items <- getRSItemNames(itemsFile);
  cached <- list.files(folder);
  
  #don't pull already cached items
  items = items[!(items %in% cached)]
  
  #duplicate item names need to be fixed up remove them for now
  items = items[!duplicated(items)]
  
  # Calculate the number of cores
  no_cores <- detectCores();
  # Initiate cluster
  cl <- makeCluster(no_cores, type="FORK");
  fun <- function(name) {
    tryCatch({writeRSData(getRSDataByName(name), folder);}, warning = function(w) print(w), error = function(e) print(e));
  }
  clusterApply(cl, items, fun);
  stopCluster(cl);
}

loadCachedData <- function(folder) {
  files = list.files(folder);
  lapply(files, function (file) loadRSData(file, folder));
}