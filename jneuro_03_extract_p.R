# Libraries
library("pdftools")
library("stringr")

# Options/Settings
# Definitions for regexp - lifted from statcheck - https://github.com/MicheleNuijten/statcheck/blob/master/R/regex.R
RGX_P <- "(p\\s?[<>=,]\\s?\\d?\\.\\d+e?-?\\d*)"
RGX_COMP <- "[<>=,]"
RGX_DEC <- "\\.\\d+"

articles_file <- "jneuro_articles.csv"
p_value_file <- "jneuro_p_values.csv"


# Data - stores each p value found
res <- data.frame(
  article_key = "article_key",
  test_text = "p < test",
  p_value = 100,
  p_comp = "test",
  section = -1,
  start = -1,
  end = -1,
  one_previous = "*"
)
crow <- 1

articles <- read.csv(articles_file)


# Process each article
z <- 813
for (z in 1:nrow(articles)) {
  # Article properties
  article_key <- articles[z, "file_stem"]


  destination <- paste(
    getwd(),
    "/pdf/",
    article_key,
    ".pdf",
    sep = ""
  )

  # Get text and unlist it
  pdf_text <- pdftools::pdf_text(destination)
  pdf_text<-unlist(pdf_text)
  pdf_text <- gsub(
    pattern = "p ⬍ 0",
    replacement = "p < 0",
    x = pdf_text
  )
  pdf_text <- gsub(
    pattern = "p ⫽ 0",
    replacement = "p = 0",
    x = pdf_text
  )



  # Find each reported p value result in the form: (p </>/= decimal)
  nhst_raw <- gregexpr(
    pattern = RGX_P,
    text = pdf_text,
    ignore.case = FALSE,
    perl = TRUE
  )

  # Article text is in sections (due to character length restriction?); cycle through
  for (x in 1:length(nhst_raw)) {
    # Check to see if there are any matches in this section
    if (nhst_raw[[x]][1] != -1) {
      # Section number
      section <- pdf_text[[x]]

      # Retrieve actual text of each p value result in this section
      section_tests <- substring(
        text = section,
        first = nhst_raw[[x]],
        last = nhst_raw[[x]] + attr(nhst_raw[[x]], "match.length")-1
      )
      one_previous <- substring(
        text = section,
        first = nhst_raw[[x]]-1,
        last = nhst_raw[[x]]-1
      )
      section_tests_broad <- substring(
        text = section,
        first = nhst_raw[[x]]-10,
        last = nhst_raw[[x]] + attr(nhst_raw[[x]], "match.length")-1+10
      )


      # Now cycle through each p value result
      for (y in 1:length(section_tests)) {
        # Get just the text for this result
        the_test <- section_tests[[y]]

        # Avoid test if it is eta2partial (eta2p)
        if (one_previous[[y]] != "2") {
          # Get the comparison value (<, >, or =)
          p_comp <- gregexpr(
            text = the_test,
            pattern = RGX_COMP,
            ignore.case= FALSE,
            perl = TRUE
          )[[1]]

          p_comp <- substring(
            text = the_test,
            first = p_comp,
            last = p_comp + attr(p_comp, "match.length")-1
          )

          # Get the p value and convert it to a numeric
          p_value <- strsplit(the_test, RGX_COMP)[[1]][2]
          p_value <- as.numeric(trimws(p_value, which = "both"))

          # Store this result in the results table
          res[crow, ] <- c(
            article_key,
            section_tests_broad[[y]],
            p_value,
            p_comp,
            x,
            nhst_raw[[x]][y],
            nhst_raw[[x]][y] + nchar(the_test),
            one_previous[[y]]
          )
          crow <- crow + 1
        }

      }

    }

  }

}


# Manual coding -- Freeman wasn't parsing correctly due to an encoding error; easier to hand code these
res[nrow(res)+ 1, ] <- c(
  "41_1_89",
  "This result, according to a two-sample Kolmogorov-Smirnov test, is highly significant (n ¼ 180 per direction; p , 2:2  1016)",
  2.2*10^-16,
  "<",
  1,
  -1,
  -1,
  "Manual"
)

res[nrow(res)+ 1, ] <- c(
  "41_1_89",
  "The correlation coefficient in this figure is 0.75, which is highly significant (n ¼ 3721; p , 2:2  1016)",
  2.2*10^-16,
  "<",
  1,
  -1,
  -1,
  "Manual"
)

res[nrow(res)+ 1, ] <- c(
  "41_1_89",
  "A one-sided sign test (sign ¼ 2728; n ¼ 3721; p , 2:2  1016) indicates that this result is highly significant.",
  2.2*10^-16,
  "<",
  1,
  -1,
  -1,
  "Manual"
)

res[nrow(res)+ 1, ] <- c(
  "41_1_89",
  "Indeed, the correlation between the two maps was positive (correlation coefficient ¼ 0:61) and highly significant (n ¼ 3721; p , 2:2  1016)",
  2.2*10^-16,
  "<",
  1,
  -1,
  -1,
  "Manual"
)

res[nrow(res)+ 1, ] <- c(
  "41_1_89",
  "Offset > 0:   p <   2.2 x 10–16",
  2.2*10^-16,
  "<",
  1,
  -1,
  -1,
  "Manual"
)


write.csv(res, p_value_file, row.names = FALSE)

