library(RSelenium)
library(rvest)
library(tidyverse)
library(netstat)
rD <- rsDriver(verbose = TRUE,
               port= free_port(), 
               browser = "chrome", 
               chromever = "94.0.4606.41",
               check = TRUE)
remDr <- rD$client
remDr$navigate("https://solsea.io/explore")

data <- data.frame()
for(i in 1:300){
  remDr$executeScript(paste("scroll(0,",i*10000,");"))
  cat('no_scroll =', i, '\n')
  Sys.sleep(3)
    html <- remDr$getPageSource()[[1]]
    url <- read_html(html) |>
  html_nodes('div.img-holder__NftItem-module_2iKWZ') |>
  html_element('a') |>                                                    
  html_attr("href") %>% paste0("https://solsea.io", .)
    data <- c(data, url)
}

# remove duplicate data
unique(data) -> data2

n <- nrow(data2)

scrap_title <- list()
for (i in data2[1:n]){
  url <- i
  html <- read_html(url)
  
  title <- html %>%
    html_node("div.title__Nft-module_2NRa7") %>%
    html_text()
  
  scrap_title[[length(scrap_title) + 1]] <- title

}


scrap_price <- list()
for (i in data2[1:n]){
  url <- i
  html <- read_html(url)
  
  price <- html %>%
    html_node("span.price__Nft-module_3_W6L") %>%
    html_text()
  
  scrap_price[[length(scrap_price) + 1]] <- price
}


scrap_currency <- list()
for (i in data2[1:n]){
  url <- i
  html <- read_html(url)
  
  currency <- html %>%
    html_node("span.currency__Nft-module_3VVEm") %>%
    html_text()
  
  scrap_currency[[length(scrap_currency) + 1]] <- currency
}
unique(scrap_currency) -> c # same currency


scrap_secondsale <- list()
for (i in data2[59:n]){
  url <- i
  html <- read_html(url)
  
  secondary_sale <- html %>%
    html_node("span.royalties__Nft-module_26M49") %>%
    html_element('strong') %>%
    html_text()
  
  scrap_secondsale[[length(scrap_secondsale) + 1]] <- secondary_sale
}

urls_all <- map(data2[1:n], read_html)
text <- lapply(urls_all, function(x) html_node(x, "div.views__Nft-module_1jJ-z"))
view <- xml_contents(text[[7]])


library(tm.plugin.webmining)
for (i in text){
  extractHTMLStrip(" ")
}


scrap_fav <- list()
for (i in data2[1:n]){
  url <- i
  html <- read_html(url)
  
  fav <- html %>% 
    html_node("div.like__Nft-module_h2bUI.no-cursor__Nft-module_5MkRK") %>% 
    html_element('p') %>%
    html_text()
  
  scrap_fav[[length(scrap_fav) + 1]] <- fav
}


scrap_propname <- list()
for (i in data2[1:n]){
  url <- i
  html <- read_html(url)
  
  property_name <- html %>% 
    html_nodes("p.trait-name__Nft-module_34-uj") %>% 
    html_text()
  
  scrap_propname[[length(scrap_propname) + 1]] <- property_name
}

unique(scrap_propname) -> propname


scrap_propvalue <- list()
for (i in data2[1:n]){
  url <- i
  html <- read_html(url)
  
  property_value <- html %>% 
    html_nodes("p.trait-value__Nft-module_1sXUa") %>% 
    html_text()
  
  scrap_propvalue[[length(scrap_propvalue) + 1]] <- property_value
}


scrap_tag <- list()
for (i in data2[1:n]){
  url <- i
  html <- read_html(url)
  
  tag <- html %>%
    html_nodes("div.tag-holder__Nft-module_3VUxe") %>%
    html_elements('p') %>%
    html_text()
  
  scrap_tag[[length(scrap_tag) + 1]] <- tag
}


scrap_rarity <- list()
for (i in data2[1:n]){
  url <- i
  html <- read_html(url)
  
  rarity <- html %>%
    #html_nodes("div.rarity-score__Nft-module_BvPlY") %>%
    html_nodes("div.counter__Nft-module_3vElb") %>%
    html_element('p') %>%
    html_text()
  
  scrap_rarity[[length(scrap_rarity) + 1]] <- rarity
}



# sample size
library(pwr)
pwr.f2.test(u = 9, f2 = 0.15, sig.level = 0.05, power = 0.90)
pwr.f2.test(u = 17, f2 = 0.15, sig.level = 0.05, power = 0.90)
pwr.f2.test(u = 30, f2 = 0.35, sig.level = 0.05, power = 0.90)

scrap_price <- list(scrap_price)
summary(scrap_price)
lst2 <- unlist(scrap_price, use.names = FALSE)

hist(lst2)

boxplot(scrap_price)

write.csv(scrap_propname2, "property.csv")

data.table::data.table(scrap_propname) -> scrap_propname2
df <- apply(scrap_propname2, 2, as.character)

library(readr)
write.csv(data2, "data_499.csv")
write.csv(scrap_title, "title.csv")
write.csv(scrap_price, "price.csv")
write.csv(scrap_currency, "currency.csv")
write.csv(scrap_secondsale, "second.csv")


data.table::data.table(scrap_tag) -> scrap_tag2
df <- apply(scrap_tag2, 2, as.character)
write.csv(df, "tag.csv", fileEncoding = 'UTF-8')


data.table::data.table(scrap_rarity) -> scrap_rarity2
df <- apply(scrap_rarity2, 2, as.character)
write.csv(df, "rarity.csv", fileEncoding = 'UTF-8')


data.table::data.table(scrap_propname) -> scrap_propname2
df <- apply(scrap_propname2, 2, as.character)
write.csv(df, "newproperty.csv", fileEncoding = 'UTF-8')


