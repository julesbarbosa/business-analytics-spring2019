url <- "https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/groceries-vertical.csv"
data <- read.csv(url, header = TRUE, stringsAsFactors = FALSE)

top = as.data.frame(head(table(products)[order(table(products), decreasing = TRUE)]))

# Function that takes a row (transaction) and a parameter n 
## and generates combinations of n products in a transaction.

create_itemsets <- function(row, n) {  
  products <- row[2:33]     #products are in the range from column 2 to 33
  products <- products[products != ""]   #remove empty strings
  if (length(products) >= n)            # number of products should be equal or bigger than ##
    combinations <- t(combn(products, n))  ## number of combinations. 
  else
    combinations = c(); # empty vector
  return(combinations)
}

itemsets2 <- apply(data, 1, create_itemsets, 2)
itemsets2 <- do.call(rbind, itemsets2)
itemsets2 <- as.data.frame(itemsets2)
itemsets_frequency2 <- aggregate(itemsets2[,1], by=list(itemsets2[,1], itemsets2[,2]), FUN=length)

## Combine 3 products
itemsets <- apply(data, 1, create_itemsets, 3)
itemsets <- do.call(rbind, itemsets)
itemsets <- as.data.frame(itemsets)
itemsets_frequency <- aggregate(itemsets[,1], by=list(itemsets[,1], itemsets[,2], itemsets[,3]), FUN=length)

# Combine 4 products
itemsets4 <- apply(data, 1, create_itemsets, 4)
itemsets4 <- do.call(rbind, itemsets4)
itemsets4 <- as.data.frame(itemsets4)
itemsets_frequency4 <- aggregate(itemsets4[,1], by=list(itemsets4[,1], itemsets4[,2], itemsets4[,3], itemsets4[,4]), FUN=length)

# Combine 5 products
itemsets5 <- apply(data, 1, create_itemsets, 5)
itemsets5 <- do.call(rbind, itemsets5)
itemsets5 <- as.data.frame(itemsets5)
itemsets_frequency5 <- aggregate(itemsets5[,1], by=list(itemsets5[,1], itemsets5[,2], itemsets5[,3], itemsets5[,4]),itemsets5[,5]) FUN=length)


