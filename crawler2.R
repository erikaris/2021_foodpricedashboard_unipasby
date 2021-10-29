library(rvest)  # https://rvest.tidyverse.org/articles/rvest.html
library(stringr) # https://stringr.tidyverse.org/
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# step 1: Read the whole html
html <- read_html("https://hargapangan.id/tabel-harga/")

# step 2: get the list of commodities
id <- html %>% 
  html_elements(xpath='//*[@id="filter_commodity_ids"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()    # library(stringr)

label <- html %>% 
  html_elements(xpath='//*[@id="filter_commodity_ids"]/option') %>% 
  html_text() %>% 
  str_trim()

# commodities <- as.data.frame(list(idkom=idkom, label=idlabel))
commodities <- data.frame(id = id, label = label)

# step 3: get the list of provinces
id <- html %>% 
  html_elements(xpath='//*[@id="filter_province_ids"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()

label <- html %>% 
  html_elements(xpath='//*[@id="filter_province_ids"]/option') %>% 
  html_text() %>% 
  str_trim()

# provinces <- as.data.frame(list(id=id, label=label))
provinces <- data.frame(id = id, label = label)

# step 4: get the list of regencies
# bit challenging coz it involves lazy loading of regencies based on selected provinces. 
# explore the page to see the method that is used to obtain the data. 
# then, iterate to get the context using API. 

regencies = NULL

for (provid in provinces$id) {
  # get the kabupaten list
  resp <- GET(paste0(
    "https://hargapangan.id/?option=com_gtpihps&task=stats_province.loadRegencies&filter_province_ids%5B%5D=", provid, "&price_type_id=1"
  )) %>% 
    content("text")
  
  # the above resp is not valid html (without html, body, and select tags), we need to concat it
  resp <- paste0("<html><body><select id='kabupaten'>", resp, "</select></body></html>")
  
  # scrape the kabupaten id
  tmp_regencies_id <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="kabupaten"]/option') %>% 
    html_attr("value") %>% 
    str_trim()
  
  # scrape the kabupaten label
  tmp_regencies_label <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="kabupaten"]/option') %>% 
    html_text() %>% 
    str_trim()
  
  tmp_regencies <- data.frame(province = provid, id = tmp_regencies_id, label = tmp_regencies_label)
  
  if (is.null(regencies)) regencies = tmp_regencies
  else regencies = rbind(regencies, tmp_regencies)
}

# step 5: get the list of markets
# same technique as step 4

markets = NULL

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
  
  tmp_markets_label <- read_html(resp) %>% 
    html_elements(xpath='//*[@id="market"]/option') %>% 
    html_text() %>% 
    str_trim()
  
  reg <- regencies %>% dplyr::filter(id == regid)
  markets_prov <- rep(reg$province, length(tmp_markets_label))
  
  tmp_markets <- data.frame(province = markets_prov, regency = regid, id = tmp_markets_id, label = tmp_markets_label)
  
  if (is.null(markets)) markets = tmp_markets
  else markets = rbind(markets, tmp_markets)
}

# step 6: get the commodity prices
# take a look at the POST request sent when clicking "Lihat Laporan"

commodity_prices <- NULL
for (prov in provinces$id[1:1]) {
  for (reg in regencies %>% filter(province == prov) %>% .$id[1:1]) {
    json_body <- toJSON(list(
      "task" = "",
      "filter_commodity_ids[]" = "0",
      "filter_province_ids[]" = "0",
      "filter_province_ids[]" = prov,
      "filter_regency_ids[]" = "0",
      "filter_regency_ids[]" = reg,
      "filter_market_ids[]" = "0",
      "filter_all_commodities" = "0",
      "format" = "html",
      "price_type_id" = "1",
      "filter_layout" = "default",
      "filter_start_date" = "29-08-2021",
      "filter_end_date" = "29-10-2021"
    ), auto_unbox = TRUE)
    
    resp <- POST("https://hargapangan.id/tabel-harga/pasar-tradisional/daerah", body = json_body, encode = "json") %>% 
      content("text")
    
    tmp_prices <- read_html(resp) %>% 
      html_elements('#report') %>%
      html_table() %>% 
      .[[1]] %>% 
      pivot_longer(!c(No., `Komoditas (Rp)`), names_to = "tanggal", values_to = "harga")
    
    tmp_prices$province <- prov
    tmp_prices$regency <- reg
    
    if (is.null(commodity_prices)) commodity_prices <- tmp_prices
    else commodity_prices <- union(commodity_prices, tmp_prices)
  }
}

if (!is.null(provinces)) write.csv2(provinces, 'provinces.csv')
if (!is.null(regencies)) write.csv2(regencies, 'regencies.csv')
if (!is.null(markets)) write.csv2(markets, 'markets.csv')
if (!is.null(commodities)) write.csv2(commodities, 'commodities.csv')
if (!is.null(commodity_prices)) write.csv2(commodity_prices, 'commodity_prices.csv') 
