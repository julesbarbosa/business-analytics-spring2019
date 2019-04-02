url <- "https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/Progresso_Soup.csv"
data <- read.csv(url, header = TRUE)

str(data)
Winter <- function(month) {
  if (month == 10 | month == 11 | month == 12 | month == 1 | month == 2) {
    month <- 1
} else {
    month <- 0
}
  return (month)
}
data$Winter <- apply(as.matrix(data$Month), 1, Winter)

plot(data$Winter, data$Low_Income)
dataWinter <- data[data$Winter== 1,]
dataNoWinter <- data[data$Winter == 0,]

table(dataNoWinter$Region)
table(dataWinter$Region)

x <- lm(data$Sales.Progresso~ data$Price.Progresso, data=data)
summary(x)
y <- lm(data$Sales.Progresso ~ data$Region)
summary(y)
plot(y)

plot(data$Region, data$Sales.Progresso)
