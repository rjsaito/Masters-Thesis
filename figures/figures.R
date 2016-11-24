pacman::p_load(dpyr, stringr, stringdist)

#source some functions
source("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/src_func.R")

sku <- read.csv("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/data/sku_list.csv", stringsAsFactors = F)
word_list <- read.csv("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/data/word_list.csv", stringsAsFactors = F)

words = list(desc = unique(subset(word_list, Type == 1)$Word), sku = unique(subset(word_list, Type == 2)$Word))
prods = data_sim(words)
prods %>% mutate(SKU = paste0(str_sub(SKU,1, 60), "...")) %>% slice(1:10)


#Example
list = c("Product A Bluetooth Mobile Speaker Black",
         "Product A Bluetooth Mobile Speaker Green",
         "Product A Bluetooth Mobile Speaker Gray",
         "Product B Bluetooth Mobile Speaker Black",
         "Product B Bluetooth Mobile Speaker Red",
         "Product C Tablet Case for iPad Black",
         "Product C Tablet Case for iPad Gray"
)
dist.mat.1 = round(wosa.mat(list, type = "character", weight = "linear", sum.right = F), 2)
dist.mat.2 = round(wosa.mat(list, type = "item", weight = "linear", sum.right = T), 2)
row.names(dist.mat.1) = row.names(dist.mat.2) = list


hc.single <- hclust(as.dist(dist.mat.1), "single")
png('C:/Users/rjsai/Dropbox/UMN Courses/Plan B/figures/hc_single_osa.png')
plot(hc.single, main = "Single-Link Hierarchical Clustering, OSA", xlab = "SKU")
dev.off()

hs.single <- hclust(as.dist(dist.mat.2), "single")
png('C:/Users/rjsai/Dropbox/UMN Courses/Plan B/figures/hc_single_wosa.png')
plot(hs.single, main = "Single-Link Hierarchical Clustering, WOSA", xlab = "SKU")
dev.off()


png('C:/Users/rjsai/Dropbox/UMN Courses/Plan B/figures/hc_single.png')
par(mfrow = c(1,2))
plot(hc.single, main = "Cluster Dendrogram, OSA", xlab = "SKU")
plot(hs.single, main = "Cluster Dendrogram, WOSA", xlab = "SKU")
dev.off()

hc.comp <- hclust(as.dist(dist.mat.1), "complete")
hs.comp <- hclust(as.dist(dist.mat.2), "complete")

png('C:/Users/rjsai/Dropbox/UMN Courses/Plan B/figures/hc_complete.png')
par(mfrow = c(1,2))
plot(hc.comp, main = "Cluster Dendrogram, OSA", xlab = "SKU")
plot(hs.comp, main = "Cluster Dendrogram, WOSA", xlab = "SKU")
dev.off()



# cmd1 <- cmdscale(as.dist(dist.mat.1),eig=TRUE, k=2)
# eigs1 = eigen(as.dist(dist.mat.1))
# db1 <- dbscan(as.dist(dist.mat.1), eps = .07, minPts = 1)
# plot(cmd1$points[,1], cmd1$points[,2])
# hullplot(cmd1$points[,1:2], db1, main = "DBSCAN")
# 
# cmd2 <- cmdscale(as.dist(dist.mat.2),eig=TRUE, k=2)
# db2 <- dbscan(as.dist(dist.mat.2), eps = .07, minPts = 1)
# hullplot(cmd2$points[,1:2], db2, main = "DBSCAN")

