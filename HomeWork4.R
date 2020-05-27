###First Question

Question1$Price <- Question1$`Price ($)`   ## Changing name
Question1$Weight <- Question1$`Weight (lbs)`
plot(Question1$Weight, Question1$Price)
abline(model, col = "blue")
str(Question1)
model <- lm(Price ~ Weight, data = Question1)
summary(model)

###Second question

plot(Question2$LineSpeed, Question2$Number_of_defective_Parts)
abline(Model2, col = "blue")
str(Question2)
Model2 <- lm(Number_of_defective_Parts ~ LineSpeed, data=Question2)
summary(Model2)
plot(Model2)

## Third Question

plot(Question3$Weekly_Usage, Question3$Annual_Maintenance)
str(Question3)
Model3 <- lm(Annual_Maintenance ~ Weekly_Usage, data= Question3)
summary(Model3)
abline(Model3, col = "blue")

## 4th Question
plot(Question4$Miles, Question4$Price)
str(Question4)
Model4 <- lm(Price ~ Miles, data = Question4)
summary(Model4)
abline(Model4, col="blue")


### 5th question

url <- "https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/dodgers.csv"
data <- read.csv(url, header = TRUE, stringsAsFactors = FALSE)

str(data)
promotions <- data[data$cap =="YES"| data$shirt =="YES"| 
                     data$fireworks == "YES" | data$bobblehead == "YES",]
count(promotions)

NoPromotions <- data[data$cap =="NO" & data$shirt =="NO" & 
                       data$fireworks == "NO" & data$bobblehead == "NO",]
count(NoPromotions)

mean(promotions$attend)
mean(NoPromotions$attend)
mean(data$attend)

weekday <- data[data$day_of_week == "Monday"|data$day_of_week =="Tuesday"|
                  data$day_of_week =="Wednesday"|data$day_of_week =="Thursday"|
                  data$day_of_week =="Friday",]
Weekend <- data[data$day_of_week == "Saturday"| data$day_of_week == "Sunday",]
mean(weekday$attend)
mean(Weekend$attend)
table(weekday$day_of_week)
table(Weekend$day_of_week)
mean(table(Weekend$day_of_week))
mean(table(weekday$day_of_week))
table(data$month)
Summer <- data[data$month == "JUN" | data$month == "JUL"| data$month == "AUG",]
OtherMonths <- data[data$month == "APR" | data$month == "MAY"| data$month == "SEP" |
                      data$month == "OCT",]
mean(OtherMonths$attend)
mean(Summer$attend)
SummerWeekend <- Summer[Summer$day_of_week == "Saturday"| Summer$day_of_week == "Sunday",]
mean(SummerWeekend$attend)
SummerDay <- Summer[Summer$day_of_week == "Monday"|Summer$day_of_week =="Tuesday"|Summer$day_of_week =="Wednesday"|Summer$day_of_week =="Thursday"|Summer$day_of_week =="Friday",]
mean(SummerDay$attend)

OtherMonthsWeekend <- OtherMonths[OtherMonths$day_of_week == "Saturday"| OtherMonths$day_of_week == "Sunday",]
mean(OtherMonthsWeekend$attend)
OtherMonthsDay <- OtherMonths[OtherMonths$day_of_week == "Monday"|OtherMonths$day_of_week =="Tuesday"|OtherMonths$day_of_week =="Wednesday"|OtherMonths$day_of_week =="Thursday"|OtherMonths$day_of_week =="Friday",]
mean(OtherMonthsDay$attend)

####
summary(data$temp)
hightT <- data[data$temp >= 73.15,]
mean(hightT$attend)
LowT <-  data[data$temp<73.15,]
mean(LowT$attend)

summary(Summer$temp)
summary(OtherMonths$temp)
hightTemp <- Summer[Summer$temp >= 74.25,]
lowTemp <- Summer[Summer$temp <74.25,]
mean(hightTemp$attend)
mean(lowTemp$attend)
##summer
promotionsSummer <- promotions[promotions$month == "JUN" |
                                 promotions$month == "JUL"| promotions$month == "AUG",]

mean(promotionsSummer$attend)

NopromotionSummer <- NoPromotions[NoPromotions$month == "JUN" |
                                    NoPromotions$month == "JUL"| NoPromotions$month == "AUG",]
mean(NopromotionSummer$attend)
mean(NoPromotions$attend)

##otherMonths
promotionOther <- promotions[promotions$month == "APR" | promotions$month == "MAY"|
                               promotions$month == "SEP" | promotions$month == "OCT",]
mean(promotionOther$attend)

NopromotionOther <- NoPromotions[NoPromotions$month == "APR" | NoPromotions$month == "MAY"|
                                   NoPromotions$month == "SEP" |
                                   NoPromotions$month == "OCT",]
mean(NopromotionOther$attend)

## Teams
summary(data$attend)
AttAbove <- data[data$attend > 41040,]
table(AttAbove$opponent)
table(data$opponent)


### Second Question
lm(data$attend ~ data$bobblehead, data=data)


withb <- data[data$bobblehead == "YES",]
mean(withb$attend)
table(withb$month)

Promo <- function(binary) {
  if (binary == "YES") 
    binary = 1
     else if (binary == "NO")
    binary = -1
  return (binary)
}

### Cap promotion
capPromo <- as.data.frame(data$cap)
promocap <- as.matrix(apply(capPromo, 1, Promo))

data$promocap <- promocap
y <- lm(data$attend ~ data$promocap, data= data)
summary(y)
abline(plot(data$promocap, data$attend))
abline(y)
## Shirt promotion
shirtPromo <- as.data.frame(data$shirt)
shirtPromo <- as.matrix(apply(shirtPromo, 1, Promo))

data$shirtPromo <- shirtPromo
z <- lm(data$attend ~ data$shirtPromo, data= data)
summary(z)
plot(data$shirtPromo, data$attend)
abline(z)


## Firework promotion
fireworkPromo <- as.data.frame(data$fireworks)
fireworkPromo <- as.matrix(apply(fireworkPromo, 1, Promo))


data$fireworkPromo <- fireworkPromo
w <- lm(data$attend ~ data$fireworkPromo, data= data)
summary(w)
plot(data$fireworkPromo, data$attend)
abline(w)

##Bobblehead promotion
bobblehead <- as.data.frame(data$bobblehead)
bobbleheadPromo <- as.matrix(apply(bobblehead, 1, Promo))

data$bobbleheadPromo <- bobbleheadPromo
k <- lm(data$attend ~ data$bobbleheadPromo, data= data)
summary(k)
plot(data$bobbleheadPromo, data$attend)
abline(k)

x <- lm(data$attend~data$bobbleheadPromo + data$shirtPromo)
summary(x)
plot(x)

plot(data$attend, data$cap)
summary(glm(data$attend~data$cap, family = poisson()))
plot(glm(data$attend~data$cap, family = poisson()))
