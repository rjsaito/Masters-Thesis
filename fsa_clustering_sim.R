#load packages
pacman::p_load(dplyr, tidyr, foreach, qualV, stringdist, dbscan)

#source some functions
source("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/src_func.R")

#first, load data
sku <- read.csv("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/data/sku_list.csv", stringsAsFactors = F)
word_list <- read.csv("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/data/word_list.csv", stringsAsFactors = F)

#list of 1) desc words, 2) sku words
words = list(desc = unique(subset(word_list, Type == 1)$Word), sku = unique(subset(word_list, Type == 2)$Word))


## simulate
out = list(NULL, NULL, NULL)
reps = 1e4; iter = 1

while(iter < reps){

  prods = data_sim(words)
  class = prods$Class

  dist = wosa.mat(prods$SKU, type =  "item", weight = "linear", sum.right = T) %>% as.dist()

  # DBSCAN
  db <- dbscan(dist, eps = .4, minPts = 2)
  db.grps = db$cluster; if(all(db.grps == 0)) db.grps = rep(1, length(db.grps))

  # Hiererchical Clustering
  cutoff = .2
  hs.grps = hclust(dist, method = "single") %>% cutree(h = cutoff)
  hc.grps = hclust(dist, method = "complete") %>% cutree(h = cutoff)

  out[[1]] = rbind(out[[1]], clustEval(db.grps, class))
  out[[2]] = rbind(out[[2]], clustEval(hs.grps, class))
  out[[3]] = rbind(out[[3]], clustEval(hc.grps, class))
    
  iter = iter + 1
}
  
lapply(out, function(x) colMeans(x))

