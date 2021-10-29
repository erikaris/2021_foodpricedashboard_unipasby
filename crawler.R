library(rvest)
library(stringr)
library(httr)
library(dplyr)
library(tidyr)

# Read the whole html
html <- read_html("https://hargapangan.id/tabel-harga/pasar-tradisional/daerah")

# Crawl commodities from select options
id <- html %>% 
  html_elements(xpath='//*[@id="filter_commodity_ids"]/option') %>% 
  html_attr("value")  %>% 
  str_trim()

label <- html %>% 
  html_elements(xpath='//*[@id="filter_commodity_ids"]/option') %>% 
  html_text() %>% 
  str_trim()

commodities <- as.data.frame(list(id=id, label=label))

# # Crawl type of markets from select options
# id <- html %>% 
#   html_elements(xpath='//*[@id="price_type_id"]/option') %>% 
#   html_attr("value")  %>% 
#   str_trim()
# 
# label <- html %>% 
#   html_elements(xpath='//*[@id="price_type_id"]/option') %>% 
#   html_text() %>% 
#   str_trim()
# 
# market_types <- as.data.frame(list(id=id, label=label))

# # Crawl information types from select options
# id <- html %>% 
#   html_elements(xpath='//*[@id="data_type"]/option') %>% 
#   html_attr("value")  %>% 
#   str_trim()
# 
# label <- html %>% 
#   html_elements(xpath='//*[@id="data_type"]/option') %>% 
#   html_text() %>% 
#   str_trim()
# 
# information_types <- as.data.frame(list(id=id, label=label))

# # Crawl comparison periods from select options
# id <- html %>% 
#   html_elements(xpath='//*[@id="layout"]/option') %>% 
#   html_attr("value")  %>% 
#   str_trim()
# 
# label <- html %>% 
#   html_elements(xpath='//*[@id="layout"]/option') %>% 
#   html_text() %>% 
#   str_trim()
# 
# comparison_periods <- as.data.frame(list(id=id, label=label))

# Crawl provinces from select options
# html <- read_html("https://hargapangan.id/tabel-harga/pasar-tradisional/daerah")

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

commodity_prices <- NULL
for (prov in provinces$id) {
  for (reg in regencies %>% filter(province == prov) %>% .$id) {
    resp <- POST(
      "https://hargapangan.id/tabel-harga/pasar-tradisional/daerah", 
      content_type("application/x-www-form-urlencoded"), 
      accept("text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"),
      body = paste0(list(
        "task=", 
        "filter_commodity_ids%5B%5D=0", 
        "filter_regency_ids%5B%5D=0", 
        "filter_province_ids%5B%5D=0", 
        "filter_market_ids%5B%5D=0", 
        "filter_all_commodities=0", 
        "format=html", 
        "price_type_id=1", 
        "1b5708ee366b3fcb44c12566d8112508=1", 
        paste0("filter_province_ids%5B%5D=", prov), 
        paste0("filter_regency_ids%5B%5D=", reg), 
        "filter_layout=default", 
        "filter_start_date=01-01-2021", 
        "filter_end_date=30-10-2021"
      ), collapse = "&")
    ) %>% 
      content("text")
    
    tmp_table <- read_html(resp) %>% 
      html_element('#report')
    
    if (! is.null(tmp_table) && ! is.na(tmp_table)) {
      tmp_prices <- tmp_table %>%
        html_table(convert=FALSE) %>%   # convert=FALSE untuk mencegah konversi yang salah untuk separator ribuan
        pivot_longer(!c(No., `Komoditas (Rp)`), names_to = "tanggal", values_to = "harga")
      
      tmp_prices$harga <- gsub("\\.","", as.character(tmp_prices$harga)) # hapus dot pada separator ribuan
      tmp_prices$harga <- gsub("\\-","", as.character(tmp_prices$harga)) # hapus dash pada harga
      tmp_prices$harga <- tmp_prices$harga %>% as.numeric()
      
      tmp_prices$province <- prov
      tmp_prices$regency <- reg
      
      if (is.null(commodity_prices)) {
        commodity_prices <- tmp_prices
      } else {
        commodity_prices <- rbind(commodity_prices, tmp_prices) %>% distinct()
      }
    }
  }
}

if (!is.null(provinces)) write.csv2(provinces, 'provinces.csv')
if (!is.null(regencies)) write.csv2(regencies, 'regencies.csv')
if (!is.null(markets)) write.csv2(markets, 'markets.csv')
if (!is.null(commodities)) write.csv2(commodities, 'commodities.csv')
if (!is.null(commodity_prices)) write.csv2(commodity_prices, 'commodity_prices.csv')