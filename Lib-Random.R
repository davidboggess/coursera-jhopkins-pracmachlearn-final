InitializeRepeatableSeed <- function() {
  set.seed(5138271)	
}

CleanSession <- function() {
  rm(list = ls(all.names = TRUE), envir = globalenv())
}