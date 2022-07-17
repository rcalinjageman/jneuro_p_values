# Libraries

# Settings / Options
jneurosci_url <- "https://www.jneurosci.org/content/jneuro/"
articles_file <- "jneuro_articles.csv"
paper_suffix <- ".full.pdf"

# Initialization
articles <- read.csv(articles_file)

articles$full_url <- paste(
  jneurosci_url,
  articles$url,
  paper_suffix,
  sep = ""
)

topaste <- data.frame(url = articles$full_url)
write.csv(topaste, "justurl.csv")

# Get each pdf, unless it already exists
x <- 1
for (x in 1:nrow(articles)) {
  article_url <- paste(
    jneurosci_url,
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
      cacheOK = FALSE
    )
  }
}
