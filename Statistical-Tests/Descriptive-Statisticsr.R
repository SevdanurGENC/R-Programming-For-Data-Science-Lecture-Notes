rm(list = ls())
## Veriyi İçe Aktarma ####

veri <- read.table( file = 'yas15_ustu_BatiK_0819.txt', 
                    header = TRUE, #verinin ilk satırı değişken isimleri.
                    sep = '\t') #veriler arasinda tab tuşu ile oluşturulmuş boşluk var.

#fonksiyonun içerdiği değişkenleri tanıma
#??read.table


#excel dosyaları
# library(readxl)
# veri <- read_excel("Veri/yas15_ustu.xls", col_names = TRUE, skip = 3)


### Veriyi Disa Aktarma ####
write.table(veri, file = 'veri_kaydet.txt',
            col.names=TRUE, #sütun isimleri yazılır.
            sep = '\t', #veriler arasında tab tuşu ile boşluk oluşturur. '' veya ',' da olabilir.
            row.names = FALSE, #satır isimleri yazılmaz.
            quote = FALSE) #Veriler '' içinde yazılmaz.

# library(openxlsx)
# write.xlsx(veri, file = 'veri_kaydet.xlsx')


## Veri ile ilgili Komutlar  ####
dim(veri)
str(veri)
head(veri) #ilk 6 satır
tail(veri) #son 6 satır
View(veri)
colnames(veri) 


## Satır ve sütun seçme ####
veri[, c("Yil", "ilKodu", "ilAdi", "Genel", "Genel.Erkek", "Genel.Kadin")]
veri[, 1:6]
veri[ , c(1, 3, 5)] #1., 3., ve 5. değişken

veri$Yil

veri[,] 
veri[1,1] 
veri[1, ] 
veri[ ,1] 


## Kayıp Veriler ve TRUE ya da FALSE Geri Dönütü ####
veri[1,2]
is.na(veri[1,2]) #Kayıp veri mi değil mi?
#View(veri)

veri[1,1] 
is.na(veri[1,1])

colnames(veri) 
colnames(veri)[1] <- 'Yıl'
colnames(veri)[1] <- 'Yil'
colnames(veri) 


length(veri$ilAdi)
veri[,'ilAdi']
attr(veri$ilAdi, "levels")

summary(veri[,'ilAdi'])
length(summary(veri[,'ilAdi']))

############################## 2- dplyr ile Veri Düzenleme ################################
#https://r4ds.had.co.nz

#install.packages("dplyr")
library(dplyr)



### Değişken seçme  ####

genel <- select(veri, Yil, ilKodu, ilAdi, Genel, Genel.Erkek, Genel.Kadin)
dim(genel)
genel


select(genel, -ilKodu)
select(genel, -ilKodu, ilKodu)

genel_kdn <- select(veri, ends_with("Kadin"))  
colnames(genel_kdn)
colnames(select(veri, -ends_with("Erkek")))
colnames(select(veri, contains("Okur")))

genel_kdn <- dplyr::select(veri, ends_with("Kadin"))  

### Satır Seçme ####
summary(genel$Yil)
filter(genel, Yil==2019)
genel 


filter(genel, is.na(ilKodu))
filter(genel, !ilAdi=='Türkiye-Turkey')
filter(genel, Yil==2019 & !ilAdi=='Türkiye-Turkey')
filter(genel, Genel<200000)
filter(genel, Genel<200000 & Yil==2019)
filter(genel, ilKodu== 37 | ilAdi == 'Türkiye-Turkey')
filter(genel, ilKodu %in% c(57, 18, 78, 74)) #Batı Karadeniz



## Yeni Değişken oluşturma - mutate() ####
genel_19 <- filter(genel, Yil==2019 & !ilAdi=='Türkiye-Turkey')
genel_19
mutate(genel_19, Kadin.yuzde = (Genel.Kadin/Genel)*100)
mutate(genel_19, il.yuzde = (Genel/62689647)*100, il.yuzde.2 = round(il.yuzde, 2))
mutate(genel_19, gen.250 = ifelse(Genel >= 250000, 1, 0))

mutate(genel_19, deg.yeni = case_when(Genel < 100000 ~ 0, 
                                      100000 <= Genel & Genel < 200000 ~ 1, 
                                      200000 <= Genel & Genel < 300000 ~ 2, 
                                      300000 <= Genel & Genel < 400000 ~ 3,
                                      400000 <= Genel ~ 4))

genel_19$test<- (genel_19$Genel/62689647)
genel_19
genel_19$test<- (genel_19$Genel/62689647)*100
genel_19


## Verileri Sıralama - arrange() ####
arrange(genel_19, Genel)
arrange(genel_19, desc(Genel))
arrange(genel, Yil, desc(Genel))



## %>% komutu ####
veri %>%
    dplyr::select(Yil, ilKodu, ilAdi, Genel, Genel.Erkek, Genel.Kadin) %>%
    filter(Yil==2019 & !ilAdi=='Türkiye-Turkey') %>%
    mutate(gen.250 = ifelse(Genel >= 250000, 1, 0)) %>%
    arrange(desc(Genel))
 

rm(list = ls())

veri <- read.table( file = 'yas15_ustu_BatiK_0819.txt', 
                    header = TRUE, #veride sütun isimleri var.
                    sep = '\t')           


colnames(veri)
summary(veri$Yil)


### Kesikli ya da Nitel Değişkenler
str(veri)

summary(veri[,'ilAdi']) 
length(summary(veri[,'ilAdi']))

summary(veri$Yil)
summary(as.factor(veri$Yil))

veri$ilKodu <- as.factor(veri$ilKodu)
summary(veri$ilKodu)
length(summary(as.factor(veri$ilKodu)))

### Sürekli Değişkenler
summary(veri$Genel)

mean(veri$Genel, na.rm = TRUE)
mean(veri[, "Genel"], na.rm = TRUE)
mean(veri$Genel)



sd(veri$Genel, na.rm = TRUE)
var(veri$Genel, na.rm = TRUE)
median(veri$Genel, na.rm = TRUE)
min(veri$Genel, na.rm = TRUE)
max(veri$Genel, na.rm = TRUE)
quantile(veri$Genel, na.rm = TRUE)
summary(veri$Genel)


### %>% komutu ve `summarise()` fonksiyonu
library(dplyr)
veri %>%
    dplyr::select(Yil, ilKodu, ilAdi, Genel, Genel.Erkek, Genel.Kadin) %>%
    filter(Yil==2019 & !ilAdi=='Türkiye-Turkey') %>%
    mutate(gen.250 = ifelse(Genel >= 250000, 1, 0)) %>%
    summarise(gen.ort = mean(Genel),
              gen.sd = sd(Genel),
              gen.max = max(Genel),
              gen.min = min(Genel),
              toplam = n()
    )

veri %>%
    dplyr::select(Yil, ilAdi, Genel, Genel.Erkek, Genel.Kadin) %>%
    filter(!ilAdi=='Türkiye-Turkey') %>%
    group_by(Yil) %>%
    summarise(gen.ort = mean(Genel),
              gen.sd = sd(Genel),
              gen.max = max(Genel),
              gen.min = min(Genel),
              toplam = n()
    )




### GRAFIKLER ###
library(ggplot2)
############################## 1- Nokta Grafiği ve Temel Bilgiler ####################
BatiKaradeniz <- read.table( file = 'yas15_ustu_BatiKaradeniz.txt', 
                             header = TRUE, #veride sutun isimleri var.
                             sep = '\t') 
dim(BatiKaradeniz)
head(BatiKaradeniz[,1:6])


library(dplyr)
Kastamonu <- BatiKaradeniz %>%
    filter(ilKodu==37) #Kastamonu iline ait veriler
dim(Kastamonu)
Kastamonu[,1:6] #ilk 6 sütun

ggplot(data = Kastamonu) +
    geom_point(mapping = aes(x = Genel, y = Yil)) 

### x ve y Eksenleri
ggplot(data = Kastamonu) +
    geom_point(mapping = aes(x = Genel, y = Yil)) +
    scale_x_continuous("Nüfus", breaks = seq(280000, 320000, 3000)) + 
    scale_y_continuous("Yıl", breaks=seq(2008, 2019, 1))


### Çizgi grafiği 
ggplot(data = Kastamonu) +
    geom_point(mapping = aes(x = Genel, y = Yil)) +
    geom_line(mapping = aes(x = Genel, y = Yil)) +
    scale_x_continuous("Nüfus", breaks = seq(280000, 320000, 3000)) + 
    scale_y_continuous("Yıl", breaks=seq(2008, 2019, 1))

### Tamamlayıcı Ögeler
ggplot(data = Kastamonu) +
    geom_point(mapping = aes(x = Genel, y = Yil)) +
    geom_line(mapping = aes(x = Genel, y = Yil)) +
    scale_x_continuous("Nüfus", breaks = seq(280000, 320000, 4000)) + 
    scale_y_continuous("Yıl", breaks=seq(2008, 2019, 1)) +
    theme_linedraw()+
    labs(title = "Kastamonu İli Nüfus Grafiği",
         subtitle = "15 yaş üstü*",
         caption = "*http://www.tuik.gov.tr/PreTablo.do?alt_id=1018"
    )

### Kaydetme
gr.Kastamonu <- ggplot(data = Kastamonu) +
    geom_point(mapping = aes(x = Genel, y = Yil)) +
    geom_line(mapping = aes(x = Genel, y = Yil)) +
    scale_x_continuous("Nüfus", breaks = seq(280000, 320000, 4000)) + 
    scale_y_continuous("Yıl", breaks=seq(2008, 2019, 1)) +
    theme_linedraw()+
    labs(title = "Kastamonu İli Nüfus Grafiği",
         subtitle = "15 yaş üstü*",
         caption = "*http://www.tuik.gov.tr/PreTablo.do?alt_id=1018"
    )

ggsave("Kastamonu.png", gr.Kastamonu, width = 21, height = 15, units = "cm")


############################## 2- Nokta Grafiği: Kesikli ve Sürekli Değişken ################################
BatiKaradeniz2019 <- BatiKaradeniz %>% filter(Yil==2019) #Sadece 2019 yılına ait veri

ggplot(data = BatiKaradeniz2019) +
    geom_point(mapping = aes(x = Genel, y = ilAdi)) +
    scale_x_continuous("15 yaş üstü nüfus", 
                       breaks = seq(150000, 550000, 50000)) + 
    scale_y_discrete("İl")

ggplot(data = BatiKaradeniz2019) +
    geom_point(mapping = aes(y = Genel, x = reorder(ilAdi, Genel))) +
    scale_y_continuous("Nüfus", breaks = seq(150000, 550000, 50000)) + 
    scale_x_discrete("İl") 

gr.BK2019 <- ggplot(data = BatiKaradeniz2019) +
    geom_point(mapping = aes(x = Genel, y = reorder(ilAdi, Genel))) +
    scale_x_continuous("Nüfus", breaks = seq(150000, 550000, 50000)) + 
    scale_y_discrete("İl") +
    theme_linedraw() +
    labs(title = "Batı Karadeniz Bölümü 15 yaş üstü Nüfus Grafiği",
         subtitle = "2019 Yılı*",
         caption = "*http://www.tuik.gov.tr/PreTablo.do?alt_id=1018"
    )

gr.BK2019


############################## 3- Grafik üzerinde gruplandırma ############################## 
BatiKaradeniz08_19 <- BatiKaradeniz %>% filter(Yil %in% c(2008, 2019))
BatiKaradeniz08_19$Yil <- as.factor(BatiKaradeniz08_19$Yil)

ggplot(data = BatiKaradeniz08_19) +
    geom_point(mapping = aes(x = Genel, y = reorder(ilAdi, Genel))) +
    facet_grid(Yil ~. ) +
    scale_x_continuous("Nüfus", breaks = seq(150000, 550000, 50000)) + 
    scale_y_discrete("İl") +
    theme_linedraw() +
    labs(title = "Batı Karadeniz Bölümü 15 yaş üstü Nüfus Grafiği",
         #subtitle = "",
         caption = "*http://www.tuik.gov.tr/PreTablo.do?alt_id=1018"
    )

#`color()` fonksiyonu
ggplot(data = BatiKaradeniz08_19) +
    geom_point(mapping = aes(x = Genel, 
                             y = reorder(ilAdi, Genel), color=Yil)) +
    scale_x_continuous("Nüfus", breaks = seq(150000, 550000, 50000)) + 
    scale_y_discrete("İl") +
    theme_linedraw() +
    labs(title = "Batı Karadeniz Bölümü 15 yaş üstü Nüfus Grafiği",
         #subtitle = "",
         caption = "*http://www.tuik.gov.tr/PreTablo.do?alt_id=1018"
    )

#`shape()` fonksiyonu
ggplot(data = BatiKaradeniz08_19) +
    geom_point(mapping = aes(x = Genel, 
                             y = reorder(ilAdi, Genel), shape=Yil)) +
    scale_x_continuous("Nüfus", breaks = seq(150000, 550000, 50000)) + 
    scale_y_discrete("İl") +
    theme_linedraw() +
    labs(title = "Batı Karadeniz Bölümü 15 yaş üstü Nüfus Grafiği",
         #subtitle = "",
         caption = "*http://www.tuik.gov.tr/PreTablo.do?alt_id=1018"
    )

################################# Histogram ############################## 
yas15_2019 <- read.table( file = 'yas15_ustu_19.txt', 
                          header = TRUE, #veride sutun isimleri var.
                          sep = '\t') 
dim(yas15_2019)

ggplot(yas15_2019) +
    geom_histogram(mapping = aes(x=Genel), binwidth = 200000) +
    scale_x_continuous("Nüfus")+
    theme_linedraw() +
    labs(title = "Türkiye 15 yaş üstü Nüfus Grafiği",
         subtitle = "2019 Yılı*",
         caption = "*http://www.tuik.gov.tr/PreTablo.do?alt_id=1018",
         y = "Sayım"
    )

################################ Box-plot ############################## 
ggplot(BatiKaradeniz08_19) +
    geom_boxplot(mapping = aes(y=Genel, x=as.factor(Yil))) +
    scale_x_discrete(limits=c("2008", "2019")) +
    scale_y_continuous(breaks = seq(150000, 550000, 50000))+
    theme_linedraw() +
    labs(title = "Türkiye 15 yaş üstü Nüfus Grafiği",
         #subtitle = "2019 Yılı*",
         caption = "*http://www.tuik.gov.tr/PreTablo.do?alt_id=1018",
         y = "Nüfus",
         x = "Yıl"
    )
