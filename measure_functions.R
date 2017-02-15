#Riki Saito rsaito@logitech.com/rjsaito@gmail.com
#Distance Measure Functions

#Install/Load Packages all at once
ipak <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}

#Remove all white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#Clean strings (lower case, remove punctation, remove all white space)
str_clean <- function(strings) {
  require(dplyr)
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


#String to be used for denominator first
fuzzyq <- function(a, b, q = 2){
  require(stringdist)
  qa <- qgrams(tolower(a), q = q)
  qb <- qgrams(tolower(b), q = q)
  qab <- merge(data.frame(t(qa)), data.frame(t(qb)), by = "row.names", all.x = T)
  qab[is.na(qab)] <- 0
  qmatch <- sum(as.numeric(apply(qab, 1, function(x) min(x[2:3]))))
  qdenom <- min(sum(qa), sum(qb))
  score <- 1 - qmatch/qdenom
  return(score)
}
#a = "surfacetouchcover2" 
#b = "touchcover3keyboardformicrosoftsurface3"
#fuzzyq(a,b)


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
#Examples
#wosa("folio case for ipad mini 2 black", "folio case for ipad pro 2 black", type = "item", weight = "root", sum.right = T)
#wosa("7Button Sensei Laser Scroll Mouse USB Glossy Black", "7Button Sensei Laser Scroll Mouse USB Gray", type = "item", weight = "quadratic", sum.right = T)
#----------------------------------------------------------------------------------------------------------------------------


#Longest Common Items
#find longest matching items b/w two strings (NOT CONSECUTIVE)
#by item or character
lci <- function(a, b, type = "item"){
  require(qualV)
  if(type == "item"){
    asplit <- strsplit(a, split=" ")[[1]]
    bsplit <- strsplit(b, split=" ")[[1]]
    string <- paste(intersect(asplit, bsplit), collapse = " ")
  } else if(type =="character"){
    asplit <- strsplit(a, split="")[[1]]
    bsplit <- strsplit(b, split="")[[1]]
    string <- paste(LCS(asplit, bsplit)$LCS, collapse = "")
  }
return(string)
}
#a = "939000680 folio case for ipad mini pink"
#b = "939000876 folio case for apple ipad mini black"
#lci(a,b)
#--------------------------------------------------------------------------------------------------------------------

lci_vec <- function(strings, type = "item"){
  ui <- strings[1]
  for(l in strings[-1]) ui <- lci(ui, l, type)
  return(ui)
}
#s <- c("abcd red","abcd","abcd black","abcd redblue")
#lci_vec(s)
#--------------------------------------------------------------------------------------------------------------------
