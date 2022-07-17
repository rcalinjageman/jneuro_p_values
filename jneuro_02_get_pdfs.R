# Libraries

# Settings / Options
jneurosci_url <- "https://www.jneurosci.org"
articles_file <- "jneuro_articles.csv"
volume_suffix <- "/content/"
paper_suffix <- ".full.pdf"

# Initialization
articles <- read.csv(articles_file)


# Get each pdf, unless it already exists
x <- 1
for (x in 1:nrow(articles)) {
  article_url <- paste(
    jneurosci_url,
    volume_suffix,
    articles[[x, "url"]],
    paper_suffix,
    sep = ""
  )

  destination <- paste(
    getwd(),
    "/pdf/",
    articles[[x, "file_stem"]],
    ".pdf",
    sep = ""
  )

  if (!file.exists(destination)) {
    download.file(
      url = article_url,
      destfile = destination,
      cacheOK = FALSE,
      method = "wininet"
    )
  }
}
