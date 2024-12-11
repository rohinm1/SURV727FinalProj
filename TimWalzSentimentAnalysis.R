library(reticulate)
use_python("C:/Users/qdogs/AppData/Local/Programs/Python/Python311/python.exe", required = TRUE)
py_config()

library(tidyverse)
library(tidytext)
library(textdata)
library(ggplot2)
library(rvest)
library(httr)

# Updated date range and domains
dRangeSeq <- function(start_date, end_date, by = 1) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  sdates <- seq.Date(start_date, end_date, by = by)
  
  result <- data.frame(
    "start_date" = sdates[-length(sdates)], 
    "end_date" = sdates[2:length(sdates)]
  )
  return(result)
}
date_ranges <- dRangeSeq("2024-08-06", "2024-11-05", by = 1)
domains <- c('CNN.com', 'NPR.org', 'Foxnews.com', 'Breitbart.com', 'Forbes.com', 'Newsweek.com')

articles_list <- list()

# Scrape articles
for (i in 1:nrow(date_ranges)) {
  f = py$Filters(start_date = as.character(date_ranges$start_date[i]), 
                 end_date = as.character(date_ranges$end_date[i]), 
                 keyword = 'Tim Walz', 
                 country = 'US', 
                 domain_exact = domains)
  gd = py$GdeltDoc()
  articles = gd$article_search(f)
  articles_list[[i]] <- articles
  print(i)
}

# Combine articles into a data frame
articles_frame <- do.call('rbind', articles_list)
articles_frame$date <- as.Date(strptime(articles_frame$seendate, format = '%Y%m%dT%H%M%SZ'))

# Function to check the status of the URL (HTTP status code check)
check_status <- function(url) {
  response <- tryCatch({
    GET(url)  # Check the URL response status
  }, error = function(e) {
    message(paste("Error fetching URL:", url, "Error:", e))
    return(NULL)
  })
  
  if (!is.null(response)) {
    if (status_code(response) == 200) {
      return(TRUE)  # URL is reachable
    } else {
      message(paste("Failed to fetch:", url, "Status Code:", status_code(response)))
      return(FALSE)  # URL is not reachable
    }
  }
  return(FALSE)  # Return FALSE if there's an issue with the request
}

# Function to scrape article content with enhanced error handling
extract_content <- function(url) {
  print(paste("Fetching content for URL:", url))  # Print the URL being processed
  page <- tryCatch({
    # Attempt to read HTML with extended timeout
    read_html(url, timeout(60))  # Timeout set to 60 seconds
  }, error = function(e) { 
    message(paste("Error with URL:", url, "Error:", e))  # Log the error
    return(NULL)  # Return NULL on error
  })
  
  if (!is.null(page)) {
    # Extract article content (assuming it's within <p> tags)
    content <- page %>%
      html_nodes("p") %>%  # Extract all <p> text
      html_text() %>%
      paste(collapse = " ")  # Combine all paragraphs into one text
    return(content)
  } else {
    return(NA)  # Return NA if there's an issue fetching the page
  }
}

# Process URLs with error handling and status check
articles_frame$content <- sapply(articles_frame$url, function(url) {
  if (check_status(url)) {  # Check if the page can be fetched
    content <- tryCatch({
      print(paste("Processing:", url))  # Print each URL being processed
      content <- extract_content(url)  # Fetch content
      Sys.sleep(2)  # Optional delay to prevent rate-limiting
      return(content)
    }, error = function(e) {
      message(paste("Error processing URL:", url, "Error:", e))  # Log any errors
      return(NA)  # Return NA on error
    })
  } else {
    return(NA)  # Return NA if status check fails
  }
})

# Check the new column with content
head(articles_frame$content)

# Convert to tidy format for text processing
articles_tidy <- articles_frame %>%
  select(domain, title, content) %>%
  unnest_tokens(word, content)

# Load sentiment lexicons
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")

# Perform sentiment analysis using Bing
bing_sentiment <- articles_tidy %>%
  inner_join(bing, by = "word") %>%
  count(domain, sentiment, sort = TRUE) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)

# Visualize Bing sentiment scores by domain
ggplot(bing_sentiment, aes(x = domain, y = sentiment_score, fill = domain)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Scores by Domain",
       x = "Domain",
       y = "Sentiment Score") +
  theme_minimal()

# Optionally, analyze and visualize NRC emotions
nrc_emotion <- articles_tidy %>%
  inner_join(nrc, by = "word") %>%
  count(domain, sentiment, sort = TRUE)

nrc_emotion %>%
  filter(sentiment != "positive" & sentiment != "negative") %>%
  ggplot(aes(x = domain, y = n, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Emotion Analysis by Domain",
       x = "Domain",
       y = "Count of Words") +
  theme_minimal()
