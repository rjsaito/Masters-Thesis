#Riki Saito rsaito@logitech.com/rjsaito@gmail.com
#Distance Measure Functions


#Remove all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#Clean strings (lower case, remove punctation, remove all white space)
str_clean <- function(strings) {
  require(dplyr); require(tm)
  strings %>%
    tolower() %>%
    removePunctuation() %>%
    stripWhitespace() %>%
    trim()  
}


#Convert roman numerals to integers (except for roman numeral I)
roman.to.int <- function(x){
  xs <- strsplit(x, " ")[[1]]
  ind <- which(suppressWarnings(!is.na(as.roman(xs))) & tolower(xs) != "i")
  newx <- paste(replace(xs, ind, as.roman(xs[ind])), collapse = " ")
  return(newx)
}


#Weighted Optimal String Alignment
#Modified method from https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance#Optimal_string_alignment_distance
#ranges from 0-1
### Parameters
### a : first string
### b : second string
### type : edit on character vs item (separated by space)
### weight : weight of character location, linear (default) vs quadratic
### sum.right 
wosa <- function(a, b, type = "character", weight = "linear", sum.right = F){
  if(type == "character"){
    A <- strsplit(a, '')[[1]]
    B <- strsplit(b, '')[[1]]
    d <- array(0, c(nchar(a)+1, nchar(b)+1))
  } else if(type == "item"){
    A <- strsplit(a, ' ')[[1]]
    B <- strsplit(b, ' ')[[1]]
    d <- array(0, c(length(A)+1, length(B)+1))
  }
  for(i in seq_len(length(A))+1){
    for(j in seq_len(length(B))+1){
      if(A[i-1] == B[j-1]) cost <- 0 else cost <- 1
      d[i,j] <- min(c(  d[i-1,j] + 1,  		#deletion
                        d[i, j-1] + 1, 		#insertion
                        d[i-1, j-1] + cost)) 	#substitution
      if(i > 2 & j > 2) if(A[i-1] == B[j-2] & A[i-2] == B[j-1]) 
        d[i,j] <- min(d[i,j], d[i-2, j-2] + cost) #transposition
    }
  }
  osa <- d[length(A)+1, length(B)+1]
  #want to weight score based on where character/item edits occured
  if(ncol(d) == nrow(d)) edits <- diff(diag(d)) else {
    short_len <- min(length(A), length(B))+1
    edits <- diff(c(diag(d[1:short_len, 1:short_len]), d[short_len:(length(A)+1), short_len:(length(B)+1)][-1]))
  }
  edit.at <- which(edits != 0)
  long_len <- max(length(A), length(B))
  if(weight == "constant") w <- rep(1, long_len) else
    if(weight == "linear") w <- long_len:1 else
      if(weight == "quadratic") w <- (long_len:1)^2 else 
        if(weight == "root") w <- sqrt(long_len:1) else stop("Define Weight: linear, quadratic, or root")
  wosa <- NULL
  if(length(edit.at) >0) {
    if(sum.right) wosa$score <-  sum(w[min(edit.at):long_len])/sum(w) else wosa$score <- sum(w[edit.at])/sum(w) 
  } else wosa$score <- 0
  wosa$weights <- w/sum(w)
  wosa$edit.loc <- edit.at
  return(wosa)
}


## create distnace matrix from wosa
wosa.mat <- function(s, type = "item", weight = "constant", sum.right = T){
  require(stringdist)
  dist <- array(0, c(length(s), length(s)))
  for(j in 1:length(s)) {
    for(k in j:length(s)) {  #computing only half the matrix to remove redundant calculations
      dist[j,k] <- wosa(s[j], s[k], type = type, weight = weight, sum.right = sum.right)$score    
    }
  }
  #fill lower triangle
  dist[lower.tri(dist)] <- t(dist)[lower.tri(dist)]   #fill lower triangle with upper triangle
  return(dist)
}


### Data Simulation
#Parameters:
# @words: 
data_sim = function(words){
  
  #group
  group_df = NULL
  
  #number of classes
  num_class =  sample(2:8, 1)
  
  #truncated poisson(dont want size 0 or 1)
  class_sizes = replicate(num_class, {x = 0; while(x < 2) x = rgamma(1, shape = 2.92, rate = .61); round(x, 0)})
  
  #then for each class
  for(i in 1:num_class){
    size = class_sizes[i]
    
    #truncated gamma
    w_len = 0; while(w_len < 3 | w_len > 30) w_len = rgamma(1, shape = 4.07, rate = .32); w_len = round(w_len, 0)
    
    at = round(seq_len(w_len)/w_len, 3)
    p = 1 - dexp(10 - at*10, rate = .75)/.75
    isPLword = rbinom(w_len, 1, p)
    
    #sample PL words and SKU words
    word_df = NULL
    for(j in 1:w_len){
      if(isPLword[j] == 1) {
        w = sample(words[[1]], 1)
        word_df = cbind(word_df, rep(w, size))
      } else if (isPLword[j] == 0){
        w = sample(words[[2]], size)
        word_df = cbind(word_df, w)
      }
    }
    
    #collapse by column to construct new words
    new_words = apply(word_df, 1, function(x) paste(x, collapse = " "))
    c
    group_df = rbind(group_df, cbind(SKU = new_words, Class = i))
  }
  
  return(data.frame(group_df, stringsAsFactors = F))
  
}


#evaluation
## Precision, Recall, Fmeasure
clustEval = function(clust, group){
  class = unique(group)
  
  confusionMatrix = table(group, clust)
  groupMarginal = rowSums(table(clust, group))
  
  #Purity http://stackoverflow.com/questions/9253843/r-clustering-purity-metric
  purity = sum(apply(confusionMatrix, 2, max))/length(clust)
  #High purity is easy to achieve when the number of clusters is large - in particular, purity is 1 if each document gets its own cluster. Thus, we cannot use purity to trade off the quality of the clustering against the number of clusters
  
  ## Precision
  allPos = sum(sapply(groupMarginal, function(x) choose(x, 2)))
  #True positives are only the pairs that are of same type
  truePos = sum(sapply(confusionMatrix, function(x) choose(x, 2)))
  precision = ifelse(allPos == 0, 0, truePos/allPos)
  
  ## Recall
  #false negatives can be found by looking at pairs that should be grouped together, but are not
  if(length(unique(clust)) == 1) falseNeg = 0 else{
    falseNeg = sum(apply(confusionMatrix, 1, function(row) 
      sum(sapply(1:(length(row)-1), function(i) row[i]*sum(row[(i+1):length(row)])))
    ))
  }
  recall = truePos/(truePos + falseNeg)
  
  #Accuracy
  allNeg = choose(length(clust),2) - allPos
  trueNeg = allNeg - falseNeg
  accuracy = (truePos + trueNeg)/(allPos + allNeg)
  
  ##F measure
  Fm = ifelse(precision+recall == 0, 0, 2*precision*recall/(precision+recall))
  
  return(c(accuracy = accuracy, purity = purity, precision = precision, recall = recall, Fmeasure = Fm))
}

