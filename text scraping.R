library(tidyverse)
library(rvest)
library(xml2)

read_html("https://www.amazon.com/product-reviews/B01AHB9CN2/ref=cm_cr_dp_d_cmps_btm?ie=UTF8&reviewerType=all_reviews")


scrape_amazon <- function(ASIN, page_num){
  
  url_reviews <- paste0("https://www.amazon.com/product-reviews/",ASIN,"/?pageNumber=",page_num)
  
  doc <- read_html(url_reviews) # Assign results to `doc`
  
  # Review Title
  doc %>% 
    html_nodes(".a-text-bold span ") %>%
    html_text() -> review_title
  
  # Review Text
  doc %>% 
    html_nodes(".review-text-content span") %>%
    html_text() -> review_text
  
  # Number of stars in review
  doc %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() -> review_star
  
  # Return a tibble
  tibble(review_title,
         review_text,
         review_star,
         page = page_num) %>% return()
}

scrape_amazon(ASIN = "B01AHB9CN2", page_num = 1) 

ASIN <- "B01AHB9CN2" # Specify ASIN
page_range <- 1:10 # Let's say we want to scrape pages 1 to 10

# Create a table that scrambles page numbers using `sample()`
# For randomising page reads!
match_key <- tibble(n = page_range,
                    key = sample(page_range,length(page_range)))

lapply(page_range, function(i){
  j <- match_key[match_key$n==i,]$key

  message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j) # Progress bar

  Sys.sleep(3) # Take a three second break

  if((i %% 3) == 0){ # After every three scrapes... take another two second break
    
    message("Taking a break...") # Prints a 'taking a break' message on your console
    
    Sys.sleep(2) # Take an additional two second break
  }
  scrape_amazon(ASIN = ASIN, page_num = j) # Scrape
}) -> output_list






