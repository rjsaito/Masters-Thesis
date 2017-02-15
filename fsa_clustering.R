#load packages
pacman::p_load(dplyr, tidyr, foreach, qualV, stringdist, dbscan, tm, foreach)

#source some functions
source("https://raw.githubusercontent.com/rjsaito/Masters-Thesis/master/src_func.R")

#load data
sku <- read.csv("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/data/sku_list.csv", stringsAsFactors = F)
word_list <- read.csv("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/data/word_list.csv", stringsAsFactors = F)

#build list
sku.list <- sku %>% 
  group_by(Brand, Category) %>% 
  do(grp=data.frame(.)) %>% 
  lapply(function(x) {(x)})


## fsa clust
out_osa = out_wosa = list(NULL, NULL, NULL)


for(i in 1:length(sku.list$grp)){
  
  dist = wosa.mat(sku.list$grp[[i]]$SKU, type =  "item", weight = "linear", sum.right = T) %>% as.dist()
  class = sku.list$grp[[i]]$Product.Line.Class
  
  # DBSCAN
  db <- dbscan(dist, eps = .3, minPts = 2)
  db.grps = db$cluster; if(all(db.grps == 0)) db.grps = rep(1, length(db.grps))
  
  # Hiererchical Clustering
  cutoff = .2
  hs.grps = hclust(dist, method = "single") %>% cutree(h = cutoff)
  hc.grps = hclust(dist, method = "complete") %>% cutree(h = cutoff)
  
  out_raw[[1]] = rbind(out_raw[[1]], clustEval(db.grps, class))
  out_raw[[2]] = rbind(out_raw[[2]], clustEval(hs.grps, class))
  out_raw[[3]] = rbind(out_raw[[3]], clustEval(hc.grps, class))
  
}

results_raw = lapply(out_raw, function(x) colMeans(x))
results_raw


## simluation

#list of 1) desc words, 2) sku words
words = list(desc = unique(subset(word_list, Type == 1)$Word), sku = unique(subset(word_list, Type == 2)$Word))

out_osa = out_wosa = list(NULL, NULL, NULL)
reps = 1e2; iter = 0

while(iter < reps){
  
  prods = data_sim(words)
  class = prods$Class
  
  #dist = wosa.mat(prods$SKU, type =  "item", weight = "linear", sum.right = T) %>% as.dist()
  dist.osa = wosa.mat(prods$SKU, type =  "item", weight = "constant", sum.right = T) %>% as.dist()
  dist.wosa = wosa.mat(prods$SKU, type =  "item", weight = "linear", sum.right = T) %>% as.dist()
  
  # DBSCAN
  #db <- dbscan(dist, eps = .3, minPts = 2)
  #db.grps = db$cluster; if(all(db.grps == 0)) db.grps = rep(1, length(db.grps))
  db.osa <- dbscan(dist.osa, eps = .3, minPts = 2)
  db.osa.grps = db.osa$cluster; if(all(db.osa.grps == 0)) db.osa.grps = rep(1, length(db.osa.grps))
  db.wosa <- dbscan(dist.wosa, eps = .3, minPts = 2)
  db.wosa.grps = db.wosa$cluster; if(all(db.wosa.grps == 0)) db.wosa.grps = rep(1, length(db.wosa.grps))
  
  # Hiererchical Clustering
  hs.osa.grps = hclust(dist.osa, method = "single") %>% cutree(h = .3)
  hc.osa.grps = hclust(dist.osa, method = "complete") %>% cutree(h = .3)
  hs.wosa.grps = hclust(dist.wosa, method = "single") %>% cutree(h = .2)
  hc.wosa.grps = hclust(dist.wosa, method = "complete") %>% cutree(h = .2)
  
  out_osa[[1]] = rbind(out_osa[[1]], clustEval(db.osa.grps, class))
  out_osa[[2]] = rbind(out_osa[[2]], clustEval(hs.osa.grps, class))
  out_osa[[3]] = rbind(out_osa[[3]], clustEval(hc.osa.grps, class))
  out_wosa[[1]] = rbind(out_wosa[[1]], clustEval(db.wosa.grps, class))
  out_wosa[[2]] = rbind(out_wosa[[2]], clustEval(hs.wosa.grps, class))
  out_wosa[[3]] = rbind(out_wosa[[3]], clustEval(hc.wosa.grps, class))
  
  iter = iter + 1
}
(results_osa = lapply(out_osa, function(x) colMeans(x)))
(results_wosa = lapply(out_wosa, function(x) colMeans(x)))







# Raw datas

#load packages
pacman::p_load(dplyr, tidyr, foreach, qualV, stringdist, dbscan, tm, foreach, png, grid)

#build list
sku.list <- sku %>% 
  group_by(Brand, Category) %>% 
  do(grp=data.frame(.)) %>% 
  lapply(function(x) {(x)})


## fsa clust
out_osa = out_wosa = list(NULL, NULL, NULL)

for(i in 1:length(sku.list$grp)){
  
  dist.osa = wosa.mat(sku.list$grp[[i]]$SKU, type =  "item", weight = "constant", sum.right = T) %>% as.dist()
  dist.wosa = wosa.mat(sku.list$grp[[i]]$SKU, type =  "item", weight = "linear", sum.right = T) %>% as.dist()
  class = sku.list$grp[[i]]$Product.Line.Class
  
  # DBSCAN
  db.osa <- dbscan(dist.osa, eps = .3, minPts = 2)
  db.osa.grps = db.osa$cluster; if(all(db.osa.grps == 0)) db.osa.grps = rep(1, length(db.osa.grps))
  db.wosa <- dbscan(dist.wosa, eps = .3, minPts = 2)
  db.wosa.grps = db.wosa$cluster; if(all(db.wosa.grps == 0)) db.wosa.grps = rep(1, length(db.wosa.grps))
  
  # Hiererchical Clustering
  hs.osa.grps = hclust(dist.osa, method = "single") %>% cutree(h = .3)
  hc.osa.grps = hclust(dist.osa, method = "complete") %>% cutree(h = .3)
  hs.wosa.grps = hclust(dist.wosa, method = "single") %>% cutree(h = .2)
  hc.wosa.grps = hclust(dist.wosa, method = "complete") %>% cutree(h = .2)
  
  out_osa[[1]] = rbind(out_osa[[1]], clustEval(db.osa.grps, class))
  out_osa[[2]] = rbind(out_osa[[2]], clustEval(hs.osa.grps, class))
  out_osa[[3]] = rbind(out_osa[[3]], clustEval(hc.osa.grps, class))
  out_wosa[[1]] = rbind(out_wosa[[1]], clustEval(db.wosa.grps, class))
  out_wosa[[2]] = rbind(out_wosa[[2]], clustEval(hs.wosa.grps, class))
  out_wosa[[3]] = rbind(out_wosa[[3]], clustEval(hc.wosa.grps, class))
  
}
(results_osa = lapply(out_osa, function(x) colMeans(x)))
(results_wosa = lapply(out_wosa, function(x) colMeans(x)))
