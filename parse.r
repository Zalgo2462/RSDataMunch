library(XML)
library(RCurl)
library(jsonlite)
library(tseries)

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
  
  #convert to matrix
  data <- matrix(text, ncol = 3, byrow = TRUE);
  colnames(data) <- c("Date", "Price", "Volume");
  
  #Get rid of same day measurements
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
  data <- fromJSON(json);
  gsub(" ", "_", data$name);
}

writeRSData <- function(itemZoo, destFolder) {
  write.zoo(itemZoo, paste(destFolder, comment(itemZoo), sep="/"));
}

cacheCurrentRSData <- function(itemsFile, folder) {
  items <- getRSItemNames(itemsFile);
  cached <- list.files(folder);
  
  #don't pull already cached items
  items = items[!(items %in% cached)]
  
  #duplicate item names need to be fixed up remove them for now
  items = items[!duplicated(items)]
  
  fun <- function(name) {
    tryCatch(
      {writeRSData(getRSDataByName(name), folder);},
      warning = function(w) print(paste(name, w)),
      error = function(e) print(paste(name, e))
    );
  }
  
  for (item in items) {
    fun(item);
  }
}

loadAllCachedRSData <- function(folder) {
  files = list.files(folder);
  lapply(files, function (file) loadCachedRSData(folder, file));
}


loadCachedRSData <- function(folder, itemName) {
  r <- read.table(paste(folder, itemName, sep="/"), header = TRUE);
  r <- read.zoo(r);
  comment(r) <- itemName;
  r;
}

RSPriceOnly <- function(itemZoo) {
  itemZoo[,1];
}

zooToTsLocf <- function(itemZoo) {
  t <- as.ts(itemZoo);
  nas <- which(is.na(t));
  for(na in nas) {
    begin <- na;
    while(is.na(t[begin - 1])) {
      begin <- begin - 1;
    }
    if (begin < 0) {
      next;
    }
    while(is.na(t[begin])) {
      t[begin] = t[begin -1];
      begin <- begin + 1;
    }
  }
  t;
}

prepItemZoo <- function(itemZoo) {
  t <- diff(zooToTsLocf(RSPriceOnly(itemZoo)));
  comment(t) <- comment(itemZoo);
  t;
}

getCrossCorrelatedRSItems <- function(cacheFolder) {
  print("Analyzing cross correlations");
  items <- lapply(loadAllCachedRSData(cacheFolder), prepItemZoo);
  print(paste("Loaded", length(items), "items"));
  print(paste("Checking", choose(length(items), 2), "item pairs"))
  bestCrossCorrelation <- function(pair) {
    tryCatch(
      {
        x <- ccf(pair[[1]], pair[[2]], plot=FALSE);
        idx <- which.max(abs(x$acf));
        lag <- x$lag[[idx]];
        r <- x$acf[[idx]];
        if (abs(r) > .7) {
          print(paste(comment(pair[[1]]), comment(pair[[2]]), lag, r));
        }
        list(Item1 = comment(pair[[1]]), Item2 = comment(pair[[2]]), Lag = lag, R = r);
      }, 
      warning = function(w) {list(Item1 = comment(pair[[1]]), Item2 = comment(pair[[2]]), Lag = 0, R = -2)},
      error = function(e) {list(Item1 = comment(pair[[1]]), Item2 = comment(pair[[2]]), Lag = 0, R = -2)}
    );
  }
  pairs <- combn(items, 2, bestCrossCorrelation, simplify=FALSE);
  pairs <- pairs[sapply(pairs, function(cx) abs(cx$R) > .7 && cx$R != -2)]
  data.frame(
    Item1 = sapply(pairs, function(pair) pair$Item1),
    Item2 = sapply(pairs, function(pair) pair$Item2),
    Lag = sapply(pairs, function(pair) pair$Lag),
    R = sapply(pairs, function(pair) pair$R));
}

ZooDir <- "itemZoos"