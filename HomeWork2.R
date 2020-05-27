url <- "https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/zagat.CSV"
dataz <- read.csv(url, header = TRUE, stringsAsFactors = FALSE)

### Analytics Questions:
### 1. What can you say about the central tendency of the ratings?

tendency <- summary(dataz)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
v <- c(dataz[,2])
z <- c(dataz[,3])
w <- c(dataz[,4])
x <- c(dataz[,5])

getmode(v)
getmode(z)
getmode(w)
getmode(x)

sd(v)
sd(z)
sd(w)
sd(x)


###  2. What can you say about the spread and dispersion of the ratings?
##A measure of spread gives us an idea of how well the mean, for example,
##represents the 
var(v)
var(z)
var(w)
var(x)
par(mfrow=c(2,1)) 
boxplot(v, main= "Food")
boxplot(z, main= "Decor")
par(mfrow=c(2,1)) 
boxplot(w, main = "Service")
boxplot(x, main = "Price")
## "https://statistics.laerd.com/statistical-guides/measures-of-spread-range-quartiles.php" 

#### 3. What are the correlations between rating dimensions?

correlation <- cor(dataz[,unlist(lapply(dataz, is.numeric))])


### 4. Using the information in 1-3, design a weighted average (index) that computes
### scores for each restaurant. Your index needs to reflect which ratings 
# (decor vs food vs service vs price) you wish to amplify with loads/weights
 
# food= 35% decor = 20% service = 15% price = 30%

dataz$weighted <- 0.35*dataz$Food+ 0.2*dataz$Decor + 0.15*dataz$Service + 0.2*dataz$Price
summary(dataz$weighted)





str(dataz)
lm(Price~Food, data=dataz)
model <- lm(Price~Food, data=dataz)
summary(model)

lm(Price~Food + Decor, data=dataz)
model1 <- lm(Price~Food +Decor, data=dataz)
summary(model1)

lm(Price~Food + Decor + Service, data=dataz)
model1 <- lm(Price~Food +Decor+Service, data=dataz)
summary(model1)

lm(Price~Decor + Service, data=dataz)
model1 <- lm(Price~Decor+Service, data=dataz)
summary(model1)
 
#Food is negative so can be put out of the model because does not have statistics siginifcance 
