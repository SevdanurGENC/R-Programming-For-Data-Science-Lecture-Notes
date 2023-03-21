### Bağımsız Örneklemler *t* Testi ### 
library(xtable)
data(tli) #tli verisini oturuma ekliyoruz.
head(tli)
dim(tli)
summary(tli)


tli %>% 
    group_by(sex) %>%
    summarise(ort= mean(tlimth),
              ortanca = median(tlimth),
              sd = sd(tlimth),
              büyüklük = n())

#Normalden sapan dağılım örneklerinin Q-Q plot ile gösterimi

qqnorm(tli[tli$sex=="F", "tlimth"], ylab = "Female")
qqline(tli[tli$sex=="F", "tlimth"])

qqnorm(tli[tli$sex=="M", "tlimth"], ylab = "Male")
qqline(tli[tli$sex=="M", "tlimth"])

#varyansının eşitliği
var.test(tli$tlimth~tli$sex)
 
t.test(tlimth~sex, data = tli, var.equal=T)
t.test(tlimth~sex, data = tli)
 
#################################### Bağımlı Örneklemler *t* Testi ############################## 
summary(sleep) 

sleep %>% 
    group_by(group) %>%
    summarise(ort= mean(extra), 
              ortanca = median(extra),
              sd = sd(extra),
              büyüklük = n())

boxplot(extra ~ group, data = sleep)

qqnorm(sleep[sleep$group=="1", "extra"], ylab = "Group = 1")
qqline(sleep[sleep$group=="1", "extra"])

#örneklemlerin bağımlı olduğunu `paired=T` komutunu fonksiyona dahil ederek berlirtmemiz gerekiyor. 
t.test(extra~group, data = sleep, paired=T)

#Eşleşmiş gözlemlerin farkını kullanarak da aynı sonuçlara ulaşırız.
extra_diff <- sleep[sleep$group=="1", "extra"] - sleep[sleep$group=="2", "extra"]
t.test(extra_diff)

