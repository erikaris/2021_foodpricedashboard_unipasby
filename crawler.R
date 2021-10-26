library(rvest)
library(stringr)
library(httr)
library(dplyr)

# Read the whole html
html <- read_html("https://hargapangan.id/")

# Crawl commodities from select options
id <- html %>% 
  html_elements(xpath='//*[@id="commodity_id"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()

label <- html %>% 
  html_elements(xpath='//*[@id="commodity_id"]/option') %>% 
  html_text() %>% 
  str_trim()

commodities <- as.data.frame(list(id=id, label=label))

# Crawl type of markets from select options
id <- html %>% 
  html_elements(xpath='//*[@id="price_type_id"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()

label <- html %>% 
  html_elements(xpath='//*[@id="price_type_id"]/option') %>% 
  html_text() %>% 
  str_trim()

market_types <- as.data.frame(list(id=id, label=label))

# Crawl information types from select options
id <- html %>% 
  html_elements(xpath='//*[@id="data_type"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()

label <- html %>% 
  html_elements(xpath='//*[@id="data_type"]/option') %>% 
  html_text() %>% 
  str_trim()

information_types <- as.data.frame(list(id=id, label=label))

# Crawl comparison periods from select options
id <- html %>% 
  html_elements(xpath='//*[@id="layout"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()

label <- html %>% 
  html_elements(xpath='//*[@id="layout"]/option') %>% 
  html_text() %>% 
  str_trim()

comparison_periods <- as.data.frame(list(id=id, label=label))

# Crawl provinces from select options
html <- read_html("https://hargapangan.id/tabel-harga/pasar-tradisional/daerah")

id <- html %>% 
  html_elements(xpath='//*[@id="filter_province_ids"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()

label <- html %>% 
  html_elements(xpath='//*[@id="filter_province_ids"]/option') %>% 
  html_text() %>% 
  str_trim()

provinces <- as.data.frame(list(id=id, label=label))

# Crawl regencies from select options
regencies_prov = c()
regencies_id = c()
regencies_label = c()

for (provid in provinces$id) {
  resp <- GET(paste0(
    "https://hargapangan.id/?option=com_gtpihps&task=stats_province.loadRegencies&filter_province_ids%5B%5D=", provid, "&price_type_id=1"
  )) %>% 
    content("text")
  
  # -- coz resp from above is not valid html (without html, body, and select tags), we need to concat it
  resp <- paste0("<html><body><select id='kabupaten'>", resp, "</select></body></html>")
  
  tmp_regencies_id <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="kabupaten"]/option') %>% 
    html_attr("value") %>% 
    str_trim()
  
  regencies_id <- c(regencies_id, tmp_regencies_id)
  
  tmp_regencies_label <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="kabupaten"]/option') %>% 
    html_text() %>% 
    str_trim()
  
  regencies_label <- c(regencies_label, tmp_regencies_label)
  
  regencies_prov <- c(regencies_prov, rep(provid, length(tmp_regencies_label)))
}

regencies <- as.data.frame(list(province=regencies_prov, id=regencies_id, label=regencies_label))

# Crawl traditional markets from select options
markets_prov = c()
markets_reg = c()
markets_id = c()
markets_label = c()

for (regid in regencies$id) {
  resp <- GET(paste0(
    "https://hargapangan.id/?option=com_gtpihps&task=stats_province.loadMarkets&filter_regency_ids%5B%5D=", regid, "&price_type_id=1"
  )) %>% 
    content("text")
  
  # -- coz resp from above is not valid html (without html, body, and select tags), we need to concat it
  resp <- paste0("<html><body><select id='market'>", resp, "</select></body></html>")
  
  tmp_markets_id <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="market"]/option') %>% 
    html_attr("value") %>% 
    str_trim()
  
  markets_id <- c(markets_id, tmp_markets_id)
  
  tmp_markets_label <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="market"]/option') %>% 
    html_text() %>% 
    str_trim()
  
  markets_label <- c(markets_label, tmp_markets_label)
  
  markets_reg <- c(markets_reg, rep(regid, length(tmp_markets_label)))
  
  reg <- regencies %>% dplyr::filter(id == regid)
  markets_prov <- c(markets_prov, rep(reg$province, length(tmp_markets_label)))
}

markets <- as.data.frame(list(province=markets_prov, regency=markets_reg, 
                              id=markets_id, label=markets_label))



json_body <- jsonlite::toJSON(list(a = 1, b = NULL), auto_unbox = TRUE)
POST(b2, body = json_body, encode = "raw")