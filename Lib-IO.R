DownloadDataFile <- function(baseURL, filename) {
  download.file(paste(baseURL,filename,sep = ""), filename)
}

CachedWebGet <- function(baseURL, filename, varName) {
  url <- paste(baseURL,filename,sep = "")
  if (!file.exists(filename)) {
    print(paste("Downloading file: ", url))
    download.file(url, filename)
  }
  print(paste("Loading file: ", filename))
  srcData <- fread(filename, na.strings=c("#DIV/0!", "", "NA", " ","NULL"))
  assign(varName,srcData, envir = globalenv())
}

