### Plan B Simulation Scheme
#Bayesian simulation scheme (not enough data for frequentist scheme)

#load packages
pacman::p_load(dplyr, tidyr, foreach, qualV, stringdist, dbscan)

#first, load data
sku <- read.csv("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/data/sku_list.csv", stringsAsFactors = F)
word_list <- read.csv("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/data/word_list.csv", stringsAsFactors = F)


#list of 1) desc words, 2) sku words
words = list(desc = unique(subset(word_list, Type == 1)$Word), sku = unique(subset(word_list, Type == 2)$Word))


## Appendix
#1) class size
classes <- sku %>% 
  group_by(Brand, Category, Product.Line.Class) %>% 
  group_indices()
cfreq = table(classes)

cdist = fitdistr(cfreq, "gamma")$estimate
#kolmogorov smirnov test
ks.test(cfreq, "pgamma", cdist[1], cdist[2]) #use gamma dist

x = 0:max(cfreq)
y = sum(x)*dgamma(x, cdist[1], cdist[2])

#write png
png('C:/Users/rjsai/Dropbox/UMN Courses/Plan B/figures/class_size_dist.png')
hist(cfreq, ylim = c(0,20), main = "Histogram of Class Size", xlab = "Class Size")
lines(x, y, col = "blue", lty = 2 , lwd = 2)
legend(.6*sum(range(x)), .9*22, expression("~"*Gamma(k == 2.92, theta == 0.61)), lty=2, lwd=2, col  ="blue", cex = 1, bty = "n")
dev.off()

paste0("~", expression(Gamma(k == 2.92, theta = 0.61)))

#2) SKU description length
word_list <- read.csv("C:/Users/rjsai/Dropbox/UMN Courses/Plan B/data/word_list.csv", stringsAsFactors = F)

wfreq = table(word_list$Product)
wdist = fitdistr(wfreq, "gamma")$estimate
ks.test(wfreq, "pgamma", wdist[1], wdist[2]) #use gamma dist

x = 0:max(wfreq)
y = sum(x)*dgamma(x, shape = wdist[1], rate = wdist[2])

#write png
png('C:/Users/rjsai/Dropbox/UMN Courses/Plan B/figures/desc_length_dist.png')
hist(wfreq, breaks = 15, main = "Histogram of Description Length (# of words)", xlab = "Description Length (# of words)")
lines(x, y, col = "blue" , lty = 2 , lwd = 2)
legend(.6*sum(range(x)), .9*55, expression("~"*Gamma(k == 4.07, theta == 0.32)), lty=2, lwd=2, col  ="blue", cex = 1, bty = "n")
dev.off()  


#3) probability of PLdesc vs SKU word by position
PLdist = SKUdist = numeric(1000)
prods = unique(word_list$Product)  
for(i in prods){
  temp = subset(word_list, Product == i)
  type = temp$Type
  n = nrow(temp)
  breaks = c(0, round(seq_len(n)/n, 3))
  #fill dist  
  for(j in 1:n){
    if(type[j] == 1) PLdist[(1000*breaks[j]+1):(1000*breaks[j+1])] = PLdist[(1000*breaks[j]+1):(1000*breaks[j+1])] + 1 else
    if(type[j] == 2) SKUdist[(1000*breaks[j]+1):(1000*breaks[j+1])] = SKUdist[(1000*breaks[j]+1):(1000*breaks[j+1])] + 1 
  }
}
PLdist = PLdist/length(prods)
SKUdist = SKUdist/length(prods)


x = seq(.001, 1, by = .001)

png('C:/Users/rjsai/Dropbox/UMN Courses/Plan B/figures/word_type_prob.png')
plot(x, PLdist, ylim = c(0,1), type = "l", main = "Binomial Probability of Word Type by Location")
lines(x, SKUdist, col = "red")
lines(x, y = 1 - exp(-7.5*(1-x)) , col = "blue" , lty = 2 , lwd = 2)
legend(0, .95, c("P(Type = Desc)", "P(Type = SKU)", expression("~"*1-exp(-7.5(1-x)))), lty=c(1,1,2), lwd=c(1,1,1), col  = c("black", "red", "blue"), cex = 1, bty = "n")
dev.off()



