
############################################################################
# Question 6 (Exercise 8 of Chapter 3 of ISLR)

#(a)

Auto <- read.table("Auto.data", header = TRUE, na.strings = "?")

# Alternatively, use the package ISLR
# library(ISLR) (loads all datasets mentioned in the book. Notice: naughty move, they removed the cars with missing entries from their dataset)
# View(Auto) (R Studio tool: shows dataset)
# ?Auto (dataset documentation)

auto_model <- lm(mpg ~ horsepower, data = Auto)
summary(auto_model)

predict(auto_model, data.frame(horsepower = 98), interval = "confidence")
predict(auto_model, data.frame(horsepower = 98), interval = "prediction")

#(b)

x11()
plot(Auto$horsepower, Auto$mpg)
abline(auto_model)

#(c)

plot(auto_model)

#Complement: let's remove 117 and 94

Auto_simpler <- Auto[-c(117, 94), ]
auto_model2 <- lm(mpg ~ horsepower, data = Auto_simpler)
summary(auto_model2)

summary(auto_model)

x11()
plot(Auto$horsepower, Auto$mpg)
abline(auto_model)
abline(auto_model2) # Barely any difference

############################################################################
# Question 7 (Exercise 9 of Chapter 3 of ISLR)

#(a)

x11()
plot(Auto)

#(b)

cor(Auto) # Will not work
Auto2 <- Auto[, -9]
cor(Auto2) # If any value is NA, correlation is undefined
           # Annoyingly, "cor" does not allow for an easy removal of missing daa points
bad_rows <- which(rowSums(is.na(Auto2)) > 0)
cor(Auto2[-bad_rows, ])

#(c)

auto_model3 <- lm(mpg ~ ., data = Auto2)
summary(auto_model3)

# Flag a point: a not-too-convenient, not-too-hard way of doing that without
# a nice package readily available

point_choice <- 14
var_choice <- 4 # Try a different variable, like year (= 7)
x11()
plot(Auto2[, var_choice], Auto2$mpg, xlab = names(Auto2)[var_choice], ylab = "mpg", main = "Scatterplot")
points(Auto2[point_choice, var_choice], Auto2$mpg[point_choice], pch = 19,, lwd = 5, col = "red")

#(d)

x11()
plot(auto_model3)

#(e)

?Auto #Might be worthwhile remembering what the variables are

auto_model4 <- lm(mpg ~ acceleration * weight, data = Auto2)
summary(auto_model4)

auto_model5 <- lm(mpg ~ horsepower * acceleration * weight, data = Auto2) # 3-way interactions
summary(auto_model5)

auto_model6 <- lm(mpg ~ horsepower * acceleration  + acceleration * weight, data = Auto2) # 2-way interactions
summary(auto_model6)

#(f)

# I will do it for horsepower only

auto_model6 <- lm(mpg ~ horsepower + I(horsepower^2) + log(horsepower), data = Auto2)
summary(auto_model6)
x11(); plot(auto_model6)

auto_model7 <- lm(log(mpg) ~ horsepower + I(horsepower^2) + log(horsepower), data = Auto2)
summary(auto_model7)
x11(); plot(auto_model7)

############################################################################
# Question 8 (Exercise 10 of Chapter 3 of ISLR)

View(Carseats)
?Carseats

#(a)

seat_model <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(seat_model)

x11()
max_v <- max(Carseats$Sales)
qqplot(Carseats$Sales[which(Carseats$Urban == "Yes")], Carseats$Sales[which(Carseats$Urban == "No")], xlim = c(0, max_v), ylim = c(0, max_v), xlab = "Urban = Yes", ylab = "Urban = No")
lines(c(0, max_v), c(0, max_v))

#(e)

seat_model2 <- lm(Sales ~ Price + US, data = Carseats)
summary(seat_model2)

#(h)

x11()
plot(seat_model2)
