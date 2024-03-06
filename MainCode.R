library(rvest)
library(dplyr)
library(stringr)
library(xlsx)


 ExtractInformation<- function(year) {
  
  #Loading the Journal Link
  Link <- "https://mobilednajournal.biomedcentral.com/articles"
  page <- read_html(Link)
  
  # Total years and Respective volume numbers
  volumes <- page %>% html_nodes("#volume-from") %>% html_elements("option") %>% as.character()
  years <- as.numeric(str_extract_all(volumes, '\\b\\d{4}\\b'))
  volume_numbers <- as.numeric(str_extract_all(volumes, '(?<=Volume )\\d+'))
  
  # Matrix of volume numbers and years
  volume_years_m <- matrix(c(volume_numbers[-1], years[-1]), ncol = 2) 
  
  # Input Year validation
  if (!(year %in% volume_years_m)) {
    stop("Not a valid year.")
  }
  
  # Adding correct volume numbers to the URL
  volume_url <- paste0("https://mobilednajournal.biomedcentral.com/articles?query=&volume=", volume_years_m[volume_years_m[, 2] == year, 1])
  
  volume_page <- read_html(volume_url)
  article_links <- volume_page %>% html_nodes('.c-listing__title a') %>% html_attr('href')
  article_links <- paste0("https://mobilednajournal.biomedcentral.com", article_links)
  
  # Initialize variables
  titles <- c()
  author_names <- c()
  pub_dates <- c()
  full_texts <- c()
  affils <- c()
  corr_authors <- c()
  corr_emails <- c()
  abstracts <- c()
  
  for (link in article_links) {
    article_page <- read_html(link)
    
    # Extracting required information 
    article_title <- article_page %>% html_nodes(".c-article-title") %>% html_text()
    titles <- append(titles, article_title)
    
  
    article_authors <- article_page %>% html_elements(xpath = "//*[@data-test = 'author-name']") %>% html_text() %>% paste(collapse = ', ')
    author_names <- append(author_names, article_authors)
    
  
    article_affil <- article_page %>% html_elements(xpath = "//*[@class='c-article-author-affiliation__list']") %>% html_text()
    affils <- append(affils, article_affil)
    
    
    article_corr_author <- article_page %>% html_nodes("#corresponding-author-list a") %>% html_text() %>% paste(collapse = ', ')
    corr_authors <- append(corr_authors, article_corr_author)
    
    
    article_corr_email <- article_page %>% html_nodes("#corresponding-author-list a") %>% html_attr("href") %>% paste(collapse = ', ')
    corr_emails <- append(corr_emails, article_corr_email)
    
    
    article_pub_date <- article_page %>% html_node("time") %>% html_text()
    pub_dates <- append(pub_dates, article_pub_date)
    
  
    article_abstract <- article_page %>% html_nodes("#Abs1-content p") %>% html_text() %>% paste(collapse = ' ')
    abstracts <- append(abstracts, article_abstract)
    
}
    # Creating a dataframe and inserting all extracted info within
    df <- data.frame("Title" = titles, "Authors" = author_names, "Affiliations" = affils, "Corresponding_Authors" = corr_authors, "Corresponding_Authors_email" = corr_emails, "Publish_Date" = pub_dates, "Abstract" = abstracts)
    
    #Converting then to a Excel File
    if (nrow(df) == 0) {
      print("No Information From the provided year")
    } else {
      write.xlsx(df,'ExtractedInfoGrp5.xlsx')
    }
}

ExtractInformation(2021)

