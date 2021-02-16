library(quanteda)
library(tidyverse)
library(readtext)
library(rvest)
library(corpus)


# get the ASIN of the product. ASIN is the unique id that Amazon gives for every product
get_asin <- function(url) {
  first_attempt <- xml2::read_html(url) %>% rvest::html_node("[id='ASIN']") %>% rvest::html_attr("value")
  if (is.na(first_attempt)) {
    xml2::read_html(url) %>% rvest::html_node("[name='ASIN']") %>% rvest::html_attr("value") %>% return()
  }
  else {
    return(first_attempt)
  }
}

# get the number of reviews for a given product.
number_of_reviews <- function(asin){
  paste0("https://www.amazon.it/product-reviews/", asin, "/ref=cm_cr_arp_d_viewopt_sr?filterByStar=critical&pageNumber=1") %>% read_html() %>%
    html_node("[data-hook='cr-filter-info-review-count']") %>% html_text() %>%
    str_extract("recensioni su (?s)(.*)") %>% substring(15,) %>% gsub('\\.','', .) %>% as.numeric() %>% return()
}

# calculate number of review-pages for a given number of reviews
number_of_review_pages <- function(number_of_reviews){
  number_of_reviews %>% `/`(10) %>% ceiling() %>% return()
}

# This functions will scrape the review page of a given product
# and will put the review title, text and given stars in a tibble
acquire_reviews <- function(asin, page_num){
  paste0("https://www.amazon.it/product-reviews/",asin,"/ref=cm_cr_arp_d_viewopt_sr?filterByStar=critical&pageNumber=",page_num) %>% read_html() -> doc
  
  doc %>% # Review Title
    html_nodes("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text() -> review_title
  
  doc %>% # Review Text
    html_nodes("[class='a-size-base review-text review-text-content']") %>%
    html_text() -> review_text
  
  doc %>% # Number of stars in review
    html_nodes("[data-hook='review-star-rating']") %>%
    html_text() -> review_star
  
  data.frame(title = (review_title %>% substring(19,nchar(review_title)-4)),
             text = (review_text %>% substring(19,nchar(review_text)-4)),
             star = (review_star %>% substring(1, 1))) %>% return()
}


###################################################################################
#urls <- c("https://www.amazon.it/product-reviews/B01CT6W0U0/")
###################################################################################
asins <- c("B01N562A19")
###################################################################################

setwd("/home/mehdi/sentiment_project/mega_corpus/new")

for (asin in asins){
  #asin <- get_asin(url)
  # Create a data frame and put review data there
  reviews.df <- data.frame(title=character(), text=character(), star=integer())
  page_range <- 1:number_of_review_pages(number_of_reviews(asin))
  j <- sample(page_range)
  for(i in page_range){
    reviews.df <- rbind(reviews.df, acquire_reviews(asin = asin, page_num = j[i]))
  }
write.csv(reviews.df, paste0("critical_reviews_", asin, ".csv"), row.names=TRUE)
}


#message("Getting page ",i, " of ",length(page_range), "; Actual: page ",j)
#Sys.sleep(abs(rnorm(1, mean=1, sd=1)))
#if((i %% 5) == 0){
#  message("Taking a break...")
#  Sys.sleep(abs(rnorm(1, mean=1, sd=1)))
#}