### Basit Regresyon ### 

summary(pressure)
sd(pressure$temperature)
sd(pressure$pressure)

regres1 <- lm(pressure~temperature, data = pressure)
regres1

#`summary()` fonksiyonu ile katsayıya bağlı standart hata, 
#*t* değeri,  olasılığı, $R^2$ gibi istatistikleri öğrenebiliriz. 
summary(regres1)

fitted(regres1)
resid(regres1)

hist(regres1$residuals)
plot(regres1)


