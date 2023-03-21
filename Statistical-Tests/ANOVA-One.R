### Tek Yönlü ANOVA ### 

library(xtable)
data(tli) #tli verisini oturuma ekliyoruz.
dim(tli)
head(tli)
boxplot(tlimth ~ ethnicty, data = tli)

library(dplyr)
veri <- tli %>% 
    filter(!ethnicty == "OTHER") 
veri$ethnicty <- factor(veri$ethnicty, labels = c("BLACK", "HISPANIC", "WHITE"))

veri %>%
    group_by(ethnicty) %>%
    summarise(ort= mean(tlimth),
              sapma = sd(tlimth),
              büyüklük = n())

#aov() fonksiyonu
model1 = aov(tlimth ~ ethnicty, data = veri)
summary(model1)

#`emmeans` kütüphanesinden `lsmeans()` fonksiyonu 
# en küçük kareler ortalaması (least squares means) 
# ve bu ortalamaların güven aralığını hesaplar. 
install.packages("emmeans")
library(emmeans)
lsmEth = lsmeans(model1, "ethnicty") #ortalamaları ve güven aralığını hesaplar
levels(veri$ethnicty)
lsmEth


###################################### Ortalamaların çoklu karşılaştırılması ############################## 

#Bonferroni karşılaştırma yöntemi
summary(contrast(lsmEth, method="pairwise", adjust="bonferroni"),
        infer=c(T,T), level=0.99, side="two-sided")
# `ìnfer=c(T, T)` hem güven aralığı hem de hipotez testi sonuçlarının çıkması için.
# `adjust` komutu ile hengi yöntem kullanılacağını belirtiriz.
# Tek ve yukarı yönlü güven aralığı için `side="<"` belirtilebilir.


#Scheffè karşılaştırma yöntemi
summary(contrast(lsmEth, method="pairwise", adjust="scheffe"),
        infer=c(T,T), level=0.99, side="two-sided")

# Tukey karşılaştırma yöntemi
summary(contrast(lsmEth, method="pairwise", adjust="tukey"),
        infer=c(T,T), level=0.95, side="two-sided")

#Dunnett karşılaştırma yöntemi
#`ref=3` bağımsız değişkenin hangi grubunun kontrol grubu olduğunu belirtir.
summary(contrast(lsmEth, method="trt.vs.ctrl", adjust="mvt", ref=3), #
        infer=c(T,T), level=0.99, side="two-sided")


###################################### Örneklem büyüklüğü ############################## 
#install.packages("pwr")
library(pwr)

v = 3 #bağımsız değişken altındaki grup sayısı
del = 0.25 #istenilen ortalamalar arası fark 
sig2 = 0.007 #varsayılan en büyük hata varyansı  
alpha = 0.05 #anlamlılık düzeyi
pwr = 0.90 #istenen güç düzeyi

pwr.anova.test(k = v, sig.level = alpha, power = pwr, f = sqrt(del^2/(2*v*sig2)))


###################################### Varsayımlar ############################## 
#Bağımlı değişken aralık ya da oran ölçeğinde olmalı. 
#Bağımsız değişkenin seviyelerinin oluşturduğu örneklemler 
#    i) bağımsız evrenlerden gelmeli, 
#    ii) bağımlı değişken normal dağılım göstermeli ve bu dağılımın varyansı eşit olmalıdır.

#model1 = aov(tlimth ~ ethnicty, data = veri)

# Compute predicted values, residuals, standardized residuals, normal scores 
veri = within(veri, {
    # Compute predicted, residual, and standardized residual values 
    ypred = fitted(model1); e = resid(model1); z = e/sd(e);
    # Compute Blom’s normal scores
    n = length(e); q = rank(e); nscore = qnorm((q-0.375)/(n+0.25)) })

print(head(veri, 3), digits=4)


plot(z ~ ethnicty, data = veri, ylab = "Standardized Residuals", las=1)
abline(h=0) # Horizontal line at zero
plot(z ~ ypred, data = veri, ylab = "Standardized Residuals", las=1)
abline(h=0) # Horizontal line at zero
plot(z ~ nscore, data = veri, ylab = "Standardized Residuals", las=1)
qqline(veri$z) # Line through 1st and 3rd quantile points # A simpler way to generate the normal probability plot

#Q-Q plot, normal probability plot
qqnorm(veri$z); qqline(veri$z)

 
