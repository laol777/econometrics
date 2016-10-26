library("dplyr")
library("ggplot2")
library("GGally")
library("psych")

#1. Посторойте поле корреляции и сформулируйте гипотезу о форме связи.
#2. Рассчитайте параметры уравнения линейной, показательной и гиперболической парной регрессии.
#3.Оцените тесноту связи с помощью показателей корреляции и детерминации
#4. Дайте с помощью общего коэффициента эластичности сравнительную оценку силы связи фактора с результатом.
#5. Оцените с помощью средней ошибки аппроксимации качество уравнений.



#modelCoef[1] - intercept
#modelCoef[2] - coeff

X = data.frame(meanSalary = c(240,226,221,226,220,250,237,232,215,220,222,231,229), #target
           minSalary = c(178,202,197,201,189,302,215,166,199,180,181,186,250)) #train



nd <- data.frame(minSalary = sort(X$minSalary))

plotNewData <- function(x, predictedY) {
    ggplot(data = X, aes(x = X$minSalary, y = X$meanSalary)) + 
    geom_point(aes(x = X$minSalary, y = X$meanSalary)) + 
    geom_line(aes(x = x, y = predictedY ))

}

getStd <- function(predicted, y){
  sum = 0
  for (i in 1:13)
  {
     sum = sum + (predicted[i] - y[i]) * (predicted[i] - y[i])
  }
  sum = sqrt(sum)
  return(sum / 13)
}


#_________________linear_________________________________
linearModel <- lm(meanSalary~minSalary, data = X)
linearCoef <- coef(linearModel)
#summary(linearModel)


x <- nd$minSalary
y <- linearCoef[1] + linearCoef[2] * x

png(filename= "res1.png"); plotNewData(x, y); dev.off()

linearStd = getStd(y, X$meanSalary)

#_________________polynomial_____________________________
linearizedDataX <- log(X$minSalary)
linearizedDataY <- log(X$meanSalary)

polynomialModel <- lm(linearizedDataY~linearizedDataX)
polynomialCoef <- coef(polynomialModel)
#summary(polynomialModel)


x <- nd$minSalary
#y = b0 * x ^ b1
y <- exp(polynomialCoef[1]) * (x ^ polynomialCoef[2])
png(filename= "res2.png"); plotNewData(x, y); dev.off()

polynomialStd = getStd(y, X$meanSalary)

#__________________exponential________________________________
linearizedDataX <- exp(X$minSalary * 0.005)
linearizedDataY <- X$meanSalary

exponentialModel <- lm(linearizedDataY~linearizedDataX)
exponentialCoef <- coef(exponentialModel)
#summary(exponentialModel)


x <- nd$minSalary
#y = b0 + e ^ (x * 0.005)
y <- exponentialCoef[1] +  exponentialCoef[2] * exp(x * 0.005)
png(filename= "res3.png"); plotNewData(x, y); dev.off()
exponentialStd = getStd(y, X$meanSalary)

#_________________inverse_____________________________________

linearizedDataX <- 1000 / X$minSalary
linearizedDataY <- X$meanSalary

inverseModel <- lm(linearizedDataY~linearizedDataX)
inverseCoef <- coef(inverseModel)
summary(inverseModel)


x <- nd$minSalary
#y = b0 + b1 * (1000 / x)
y <- inverseCoef[1] + inverseCoef[2] * (1000 / x)
png(filename= "res4.png"); plotNewData(x, y); dev.off()
inverseStd = getStd(y, X$meanSalary)

#_________________hyperbolic____________________________
linearizedDataX <- X$minSalary
linearizedDataY <- 1/X$meanSalary

hyperbolicModel <- lm(linearizedDataY~linearizedDataX)
hyperbolicCoef <- coef(hyperbolicModel)
#summary(hyperbolicModel)


x <- nd$minSalary
#y = 1 / (b0 + b1 * x)
y <- 1 / (hyperbolicCoef[1] + hyperbolicCoef[2] *  x) 
png(filename= "res5.png"); plotNewData(x, y); dev.off()

hyperbolicStd = getStd(y, X$meanSalary)

#linearModel, polynomialModel, exponentialModel, inverseModel, hyperbolicModel


result = data.frame(modelName = c("linear", "polynomial", "exponential", "inverse", "hyperbolic"),
          correlation = cor(X$minSalary, X$meanSalary),
          determincation = c(summary(linearModel)$r.squared, summary(polynomialModel)$r.squared, summary(exponentialModel)$r.squared,
                           summary(inverseModel)$r.squared, summary(hyperbolicModel)$r.squared),
          stdError = c(linearStd, polynomialStd, exponentialStd, inverseStd, hyperbolicStd), 
          F = c(summary(linearModel)$fstatistic[1], summary(polynomialModel)$fstatistic[1], summary(exponentialModel)$fstatistic[1],
                summary(inverseModel)$fstatistic[1], summary(hyperbolicModel)$fstatistic[1])
          )
                     

sd = sd(X$meanSalary)
mean = mean(X$meanSalary)

