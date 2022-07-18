library(ggplot2)
library(ggdist)

# Options/Settings
articles_file <- "jneuro_articles.csv"
p_value_file <- "jneuro_p_values.csv"


articles <- read.csv(articles_file)
res <- read.csv(p_value_file)

# Aggregate number of tests per article
  article_counts <- aggregate(
    res$article_key,
    by = list(res$article_key),
    drop = FALSE,
    FUN = length
  )

  # Join that to list of articles
  colnames(article_counts) <- c("file_stem", "p_values_reported")

  articles <- merge(
    x=articles,
    y = article_counts,
    by = "file_stem",
    all.x = TRUE
  )

# Aggregate number of significant tests per article

  just_sig <- res[res$p_value < 0.05, ]
  sig_counts <- aggregate(
    just_sig$article_key,
    by = list(just_sig$article_key),
    drop = FALSE,
    FUN = length
  )

  # Join that to list of articles
  colnames(sig_counts) <- c("file_stem", "p_values_reported_less_05")

  articles <- merge(
    x=articles,
    y = sig_counts,
    by = "file_stem",
    all.x = TRUE
  )


# Clean up
  articles[is.na(articles$p_values_reported), ]$p_values_reported <- 0
  articles[is.na(articles$p_values_reported_less_05), ]$p_values_reported_less_05 <- 0

# Print
  volumes <- c(1, 40)
  for (v in volumes) {
    print(paste("Results for volume", v))
    n_articles <- nrow(articles[articles$volume == v, ])
    n_reporting_p <- nrow(articles[articles$volume == v & articles$p_values_reported > 0, ])
    total_tests <- sum(articles[articles$volume == v, ]$p_values_reported)
    total_significance <- sum(articles[articles$volume == v, ]$p_values_reported_less_05)
    print(
      paste(n_reporting_p, "of", n_articles, " reported p values, or ", n_reporting_p/n_articles*100, "%")
    )
    print(
      paste(total_tests, " p values reported, or ", total_tests/n_articles, "per total article, or ", total_tests/n_reporting_p, "per article that had tests")
    )
    print(
      paste(total_significance, "of", total_tests, "were significant at alpha = 0.05, or", total_significance/total_tests*100, "percent")
    )
  }

write.csv(articles, articles_file)

# Histograms
  articles$volume_factor <- as.factor(articles$volume)
  myplot <- ggplot2::ggplot(data = articles, aes(x = p_values_reported, fill = volume_factor))
  myplot <- myplot + ggdist::geom_dots(alpha=0.6, position = 'identity')
  myplot <- myplot + scale_fill_manual(values=c("#69b3a2", "#404080"))
  myplot <- myplot + ggplot2::theme_classic()
  myplot

  res$file_stem <- res$article_key
  res_extend <- merge(x = res, y = articles, by = "file_stem")
  myplot <- ggplot2::ggplot(data = res_extend, aes(x = log(p_value, base = 10), fill = p_value < 0.05))
  myplot <- myplot + ggdist::geom_dots(alpha=0.6)
  myplot <- myplot + scale_fill_manual(values=c("#69b3a2", "#404080"))
  myplot <- myplot + ggplot2::theme_classic()
  myplot
