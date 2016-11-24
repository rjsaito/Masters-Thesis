library(stringdist)

a = "M315 Wireless Optical Scroll Mouse Black Victorian"
b = "M315 Wireless Optical Scroll Mouse Brick Red"
c = "M325 Wireless Optical Scroll Mouse Black Victorian"

stringdist(a, b, method = "osa")/max(nchar(a), nchar(b))
stringdist(a, c, method = "osa")/max(nchar(a), nchar(c))


stringdist(a, b, method = "jaccard", q = 3)