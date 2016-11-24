pacman::p_load(rvest, dplyr)
source("C:/Users/rjsai/Dropbox/Work/Logitech/scripts/measure_functions.R")

logi_page <- read_html("https://en.wikipedia.org/wiki/List_of_Logitech_products")
logi_prod <- html_nodes(logi_page, "th:nth-child(1)") %>% html_text() %>% subset(!grepl("Product Title|Product title", .)) %>% gsub('\"', "", .) %>% .[1:(length(.)-6)]


#need to scrape diff skus from amazon?




cutoff = .1
s = logi_prod
dist <- array(0, c(length(s), length(s)))

for(j in 1:length(s)) {
  for(k in j:length(s)) {  #computing only half the matrix to remove redundant calculations
    dist[j,k] <- wosa(prod[j], s[k], type = "item", weight = "linear", sum.right = T)$score
  }
}
dist[lower.tri(dist)] <- t(dist)[lower.tri(dist)]   #fill lower triangle with upper triangle

dist %>%
  as.dist() %>%
  hclust() %>%
  cutree(h = cutoff) -> 
  group


