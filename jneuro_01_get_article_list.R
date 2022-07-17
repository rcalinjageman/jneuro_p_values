# Libraries
library(rvest)
library(xml2)

# Volumes to retrieve
volumes <- c(1, 40)


# Settings / Options
jneurosci_url <- "https://www.jneurosci.org"
volume_suffix <- "/content/"
paper_suffix <- ".full.pdf"
issue_class <- ".hw-issue-meta-data"
research_paper_class <- ".issue-toc-section-research-articles"
toc_class <- ".issue-toc"
paper_class <- ".highwire-citation-type-highwire-article"
paper_id <- "data-pisa"
paper_id_replace <- "jneuro;"
articles_file <- "jneuro_articles.csv"


# Initialization
issues <- NULL
articles <- NULL


# Get issues for each volume
for (v in 1:length(volumes)) {
  volume_url <- paste(
    jneurosci_url,
    volume_suffix,
    volumes[[v]],
    sep = ""
  )

  this_volume <- rvest::read_html(volume_url) %>%
    html_nodes(issue_class) %>%
    xml_attr("href")

  issues <- c(
    issues,
    this_volume
  )

}

# Get research papers in each issue
x <- 1
for (x in 1:length(issues)) {
  issue_url <- paste(
    jneurosci_url,
    issues[[x]],
    sep = ""
  )

  issue_list <- rvest::read_html(issue_url)

  research_articles <- issue_list %>%
    html_nodes(research_paper_class) %>%
    html_nodes(paper_class) %>%
    xml_attr(paper_id)

  if (length(research_articles) == 0) {
    research_articles <- issue_list %>%
      html_nodes(toc_class) %>%
      html_nodes(paper_class) %>%
      xml_attr(paper_id)

  }

  articles <- c(
    articles,
    research_articles
  )
}

# Clean up article data
articles <- as.data.frame(articles)
articles$url <- gsub(
  pattern = paper_id_replace,
  replacement = "",
  x = articles$articles
)
articles$file_stem <- gsub(
  pattern = "/",
  replacement = "_",
  x = articles$url
)
for (x in 1:nrow(articles)) {
  articles[x, c("volume", "issue", "page")] <- strsplit(articles[x, "url"], "/")[[1]]
}

write.csv(articles, articles_file)




