GenerateKnitHTML <- function(RmdFile, HtmlFile) {
  knit2html(RmdFile, HtmlFile)
}

GenerateKnitHTML("project.Rmd", "index.html")
