library(rvest)
library(glue)
library(tidyverse)

pag <- 1
city <- 'milano'


url <- glue::glue('https://www.immobiliare.it/affitto-appartamenti/{city}/?pag={pag}')

html_page <- read_html(url)

body_content <- html_page %>% html_nodes('.listing-item_body--content')
html_snippet <- body_content[[22]]

df <- seq_len(175) %>% map_df(function(pag){
  
  cat('scraping pag', pag, '\t')
  
  url <- glue::glue('https://www.immobiliare.it/affitto-appartamenti/{city}/?pag={pag}')
  html_page <- read_html(url)
  body_content <- html_page %>% html_nodes('.listing-item_body--content')
  
  
  df <- body_content %>% map_df(function(html_snippet){
  # body_content[22] %>% map_df(function(html_snippet){
    
    # browser()
    
    tibble(
      name =
        html_snippet %>% html_nodes('.text-primary a') %>% html_text() %>% trimws() %>% ifelse(length(.)==1, ., NA)
      ,price =
        html_snippet %>% html_nodes('.lif__item:nth-child(1)') %>% html_text() %>% gsub(x = ., pattern = '[^0-9]', replacement = "") %>% ifelse(length(.)==1, ., "") %>% as.numeric() 
      ,rooms =
        html_snippet %>% html_nodes('.lif__item:nth-child(2)') %>% html_text() %>% gsub(x = ., pattern = '[^0-9]', replacement = "") %>% ifelse(length(.)==1, ., "") %>% as.numeric()
      ,sqm =
        html_snippet %>% html_nodes('.lif__item:nth-child(3)') %>% html_text() %>% gsub(x = ., pattern = '[^0-9]', replacement = "") %>% substr(start = 1, stop = nchar(.)-1) %>% ifelse(length(.)==1, ., "") %>% as.numeric()
      ,bathrooms =
        html_snippet %>% html_nodes('.lif__item:nth-child(4)') %>% html_text() %>% gsub(x = ., pattern = '[^0-9]', replacement = "") %>% ifelse(length(.)==1, ., NA) %>% as.numeric()
      ,storey =
        html_snippet %>% html_nodes('.lif__item:nth-child(5)') %>% html_text() %>% gsub(x = ., pattern = '[^0-9 A R]', replacement = "") %>% trimws() %>% factor(levels = c('R', 'A', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15')) %>% ifelse(length(.)==1, ., NA)
      ,description_short =
        html_snippet %>% html_nodes('.descrizione__titolo') %>% html_text() %>% ifelse(length(.)==1, ., "")
      ,description_truncate =
        html_snippet %>% html_nodes('.descrizione__truncate') %>% html_text() %>% ifelse(length(.)==1, ., "")
    )
    
  })
  
  # df %>% length
  
  # df[[7]]
  
  # df %>% bind_rows()
  
  cat('scraped', nrow(df), 'offers', '\n')
  
  return(df)
  
})
# storey <- html_page %>% html_nodes('.lif__item:nth-child(5)') %>% html_text() %>% gsub(x = ., pattern = '[^0-9]|A', replacement = "")

df %>% write_csv('E:/dataScienceProjects/houseprice_scraping/test.csv')

df %>% mutate(price_sqm = price/sqm) %>% filter(price < 1500, sqm < 500) %>%
  ggplot(aes(y = price_sqm, x = sqm)) + geom_point(alpha = 0.3)

df %>% mutate(price_sqm = price/sqm) %>% filter(price < 10000, sqm < 500) %>%
  ggplot(aes(y = price, x = sqm)) + geom_point(alpha = 0.3)


df %>% filter(str_detect(tolower(description_short), tolower('piola')))