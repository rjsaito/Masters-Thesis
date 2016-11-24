#Process Live Demonstration
source.path <- "//us01f01/amr market analytics/AMR_Intern Riki/scripts/source/"
source(paste(source.path, "workingTempFile.R", sep=""))
source(paste(source.path, "measure_functions.R", sep=""))
ipak(c("dplyr","tm","foreach"))




#Phase 1 ---------------------------------------------------
#Lets say in a particular Family-Type-Brand, we have...

boom <- scan(what = "")
"BOOM 2 Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Black"
"BOOM 2 Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Blue"
"BOOM 2 Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Orange/Purple"
"BOOM 2 Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Red/Pink"
"BOOM 2 Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Blue/Green"
"BOOM 2 Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet White"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Black"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet White/Gray"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Green/Blue"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Blue/White"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Red/White"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Pink"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Multicolor"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Green/Yellow"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Green/White"
"BOOM Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Purple/White"
"Megaboom Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Blue"
"Megaboom Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Red"
"Megaboom Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Purple"
"Mini Boom Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Purple/Green"
"Mini Boom Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Black/Red"
"Mini Boom Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Orange/White"
"Mini Boom Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Black/Green"
"Mini Boom Portable Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Black"
"Mobile Boombox Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Gray/Blue"
"Mobile Boombox Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Red/Black"
"Mobile Boombox Bluetooth Speaker for MP3 Player/Cell Phone/Tablet White/Gray"
"Mobile Boombox Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Yellow/Black"
"Mobile Boombox Bluetooth Speaker for MP3 Player/Cell Phone/Tablet Black"

#clean string
boom_clean <- str_clean(boom)

#set cutoff
cutoff = .1

#create a distance matrix using measure score function
dist <- array(0, c(length(boom_clean), length(boom_clean)))
for(j in 1:length(boom_clean)) {
  for(k in j:length(boom_clean)) {  #computing only half the matrix to remove redundant calculations
    dist[j,k] <- wosa(boom_clean[j], boom_clean[k], type = "item", weight = "linear", sum.right = T)$score
  }
}

#fill lower triangle
dist[lower.tri(dist)] <- t(dist)[lower.tri(dist)]   

#groups by clustering
dist %>%
as.dist() %>%
hclust() %>%
cutree(h = cutoff) -> 
group

#create unique identifier for each group
ui <- rep(0, length(boom))
for(g in unique(group)){
  ind <- which(group == g)
  sg <- boom[ind]
  ui[ind] <- if(length(sg) > 1) rep(lci_vec(sg), length(sg)) else sg
}

#output
wtf(cbind(boom, group, ui))


#If we also want to consider score on the referece value...
surface <- scan(what = list("",""))
"D5S-00001" "Surface Touch Cover Keyboard for Tablet Black"
"D5S-00002" "Surface Touch Cover Keyboard for Tablet White"
"D5S-00003" "Surface Touch Cover Keyboard for Tablet Red"
"D5S-00005" "Surface Touch Cover Keyboard for Tablet Pink"
"D5S-00022" "Surface Touch Cover Keyboard for Tablet Black"
"N7W-00004" "Surface Type Cover 2 Keyboard for Microsoft Surface Magenta"
"N7W-00033" "Surface Type Cover 2 Keyboard for Microsoft Surface Purple"
"N7W-00001" "Surface Type Cover 2 Keyboard for Microsoft Surface Pro 2 Black"
"N7W-00002" "Surface Type Cover 2 Keyboard for Microsoft Surface Pro 2 Cyan"
"T4L-00001" "Mobile Keyboard 5000 Bluetooth Keyboard for Tablet Black"
"RD2-00079" "Type Cover for Microsoft Surface Pro 3 Blue"
"RD2-00080" "Type Cover for Microsoft Surface Pro 3 Black"
"RD2-00107" "Type Cover for Microsoft Surface Pro 3 Cyan"
"A7Z-00001" "Type Cover Keyboard for Microsoft Surface 3 Black"
"A7Z-00004" "Type Cover Keyboard for Microsoft Surface 3 Bright Red"
"A7Z-00005" "Type Cover Keyboard for Microsoft Surface 3 Red"
"A7Z00003" "Type Cover Keyboard for Microsoft Surface 3 Blue"
"A7Z00002" "Type Cover Keyboard for Microsoft Surface 3 Bright Blue"
"RD2-00077" "Type Cover Keyboard for Microsoft Surface Pro 3 Red"
"RD2-00078" "Type Cover Keyboard for Microsoft Surface Pro 3 Purple"
"QC7-00001" "Type Cover Keyboard for Microsoft Surface Pro 4 Black"
"QC7-00002" "Type Cover Keyboard for Microsoft Surface Pro 4 Bright Blue"
"QC7-00003" "Type Cover Keyboard for Microsoft Surface Pro 4 Blue"
"QC7-00005" "Type Cover Keyboard for Microsoft Surface Pro 4 Red"
"QC7-00006" "Type Cover Keyboard for Microsoft Surface Pro 4 Teal"

#clean string
rv = str_clean(surface[[1]])
desc = str_clean(surface[[2]])

#set parameters
cutoff = .07
id.weight = .2

#create a distance matrix using measure score function
idDist <- descDist <- dist <- array(0, c(length(desc), length(desc)))
for(j in 1:length(desc)) {
  for(k in j:length(desc)) {  #computing only half the matrix to remove redundant calculations
    idDist[j,k] <- wosa(rv[j], rv[k], type = "character", weight = "quadratic", sum.right = F)$score
    descDist[j,k] <- wosa(desc[j], desc[k], type = "item", weight = "linear", sum.right = T)$score
    dist[j,k] <- id.weight*idDist[j,k] + (1-id.weight)*descDist[j,k]
  }
}

#fill lower triangle
dist[lower.tri(dist)] <- t(dist)[lower.tri(dist)]   

#groups by clustering
dist %>%
as.dist() %>%
hclust() %>%
cutree(h = cutoff) -> 
group

#create unique identifier for each group
ui.1 <- rep(0, length(desc))
for(g in unique(group)){
  ind <- which(group == g)
  sg <- surface[[2]][ind]
  ui.1[ind] <- if(length(sg) > 1) rep(lci_vec(sg), length(sg)) else sg
}

#output
wtf(cbind(rv, desc, group, ui.1))
#-----------------------------------------------------------------------------------------------------





#Phase 2
map <- rbind(
  data.frame(ui = unique(ui), SOURCE_SYSTEM = "NPD"),
  data.frame(ui = c("UE BOOM", "UE BOOM 2", "UE MINI BOOM", "MEGABOOM"), SOURCE_SYSTEM = "GFK")
) %>%
mutate(ui = str_clean(ui))

gfk <- subset(map, SOURCE_SYSTEM == "GFK")
s.gfk <- gfk$ui
uniqs.gfk <- unique(s.gfk)
nch.gfk <- nchar(uniqs.gfk)

npd <- subset(map, SOURCE_SYSTEM == "NPD")
s.npd <- npd$ui
uniqs.npd <- unique(s.npd)

#create a distance matrix using measure score function
dist <- group <- array(0, c(length(uniqs.gfk), length(uniqs.npd)), dimnames = list(uniqs.gfk, uniqs.npd))
for(j in 1:length(uniqs.gfk)) {
  for(k in 1:length(uniqs.npd)) {  #computing only half the matrix to remove redundant calculations
    dist[j,k] <- fuzzyq(uniqs.gfk[j], uniqs.npd[k], q = 2)      
  }
} 
odr <- order(nch.gfk, decreasing = T)
dist <- dist[odr, , drop = F]; group <- group[odr, , drop = F]
dist <- replace(dist, is.na(dist), 1)

ig.col = NULL
for(l in 1:nrow(dist)){
  if(any(dist[l, setdiff(1:ncol(dist), ig.col)] < .1)){
    wch <- which(dist[l,] < .1  & !1:ncol(dist) %in% ig.col) 
    group[l, wch] <- 1
    ig.col <- c(ig.col, wch)
  }
}

g.row <- which(apply(group, 1, function(x) any(x == 1)))
gs <- apply(group[g.row, , drop = F], 1, function(x) names(x)[x == 1]) 
map$ui2 = map$ui

#unique identifier
if(is.null(gs)) map <- cbind(map, "m" = NA) else {
  gs.mat <- foreach(m = 1:length(gs), .combine = rbind) %do% cbind(c(names(gs)[m], gs[[m]]), m) %>%
    data.frame() %>%
    distinct()
  map <- merge(map, gs.mat, by = 1, all.x = T)
  for(n in na.omit(unique(map$m))) {
    wch <- which(map$m == n)
    ns <- map[wch, "ui"]
    map$ui2[wch] <- rep(ns[min(which(nchar(ns) == min(nchar(ns))))], length(ns))
  }
}
map %>%
arrange(m) %>%
wtf()
#need to fix







