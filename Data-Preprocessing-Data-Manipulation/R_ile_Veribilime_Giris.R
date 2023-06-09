# ##  "R ile Veri Bilimi'ne Giriş" 
  
  ### Verisetinin yüklenmesi ve verisetiyle ilgili işlemler
   
data("mtcars") #mtcars isimli r içinde bulunan bir verisetini yükledik
mtcars # verisetini görüntüleme
print("Verisetinin Boyutu:")
dim(mtcars) # verisetinin boyutu 

#Verisetimiz 32 gözlemden(satırdan) oluşan ve 11 sütunu(niteliği) bulunan arabaların 
# belirli özelliklerini içeren bir veriseti. Özelliklerini tam olarak anlamak için 
# internetten araştırma yapılabilir. Veya R'ın help kısmına mtcars yazıp gelen açıklamaya bakılabilir.
# #Curse of Dimensionality yani "Boyut Laneti" denilen 1950'lerde ortaya koyulmuş bir kavram vardır. 
# Bu kavrama göre boyut arttıkça karmaşıklık artar ama ama modelin başarım sonucu 
# hakkında bir şey söylenemez. Yani daha fazla özellik ile bir veri üzerinde çalışmak 
# daha iyi sonuç alacağımız anlamına gelmez ama karmaşıklığı artıracağı kesindir.
#Bu veriseti içindeki değişkenlerin türlerini görüp veriyi anlamaya çalışalım.
 
summary(mtcars) 

# Normalde burada bütün değişkenlerin numerik değerler olduğu görülüyor.
# Biz bu değerler üzerine bir model kurabiliriz, hatta gayet güzel bir accuracy değeri de alırız.
# Fakat böyle bir model kurmak hata olur. Çünkü anlaşılacağı gibi buradaki bütün değerler sayısal değil.
# Peki bir değerin sayısal olmadığını nasıl anlarız?
# Eğer sayısal bir nitelik içerisinde 0 sayısı mutlak yokluğu ifade ediyorsa o değişken sayısal
# bir değer ifade eder. Yok eğer, 0 değeri mutlak yokluğu değil de bir değeri ifade ediyorsa o
# zaman o değer sayısal değil kategoriktir diyebiliriz.
#   Hemen Örnek verelim:
#   mtcars veriseti açıklamasında vs kısmına bakalım:
#   vs	Engine (0 = V-shaped, 1 = straight)
# Burada "vs" kısmının motorun v olup olmadığını belirttiği görülüyor ve bu özellik 0 ve 1
# değerlerini alıyor sadece. Peki burada 0 kısmı mutlak yokluk mu ifade ediyor yoksa bir başka
# değeri mi gösteriyor. Tabi ki bu motorun v olup olmadığını, yani başka bir değeri gösteriyor.
# Yani bu değişken sayısal değildir.
# O zaman bu sütunu sayısal değerden kategorik değişkene çevirelim ki modelimiz burada nasıl
# çalışacağını daha iyi anlayabilsin.
 
mtcars$cyl <- as.factor(mtcars$cyl) #kategorik (faktör) değişkene çevirme)
#şimdi diğer kategorik değişkenleri de dönüştürelim 
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)
str(mtcars) #şimdi tekrar özelliklere bakalım
summary(mtcars) #temel istatistiklere yeniden bakalım 

# Şimdi veri içindeki değerlere nasıl ulaşabileceğimizi görelim
 
mtcars[1,2] # 1. satır 2. eleman
mtcars[2, ] # 2. satır tüm elemanlar
mtcars["hp"] # hp sütununu getirir.
#is.na(mtcars) # veriseti içinde eksik değer var mı? 

#İçinde eksik veriler de olan kendi verisetimizi oluşturalım
 
veriseti <- c(3,5,NA,7,NA)
veriseti
print("Toplam Eksik Veri Sayısı:")
sum(is.na(veriseti))
print("Eksik Verilerin Konumları (İndeksleri):")
which(is.na(veriseti)) 

#Eğer eksik verilere rağmen ortalama hesaplamak istersek 
mean(veriseti, na.rm = TRUE) 

#Eğer matris şeklinde bir veriseti oluşturmak istersek, matrix komutunu kullanırız. 
#Eksik değerlerin toplamını sum ile görebilirken, kolon bazında, her kolonda kaç eksik 
#değer olduğunu görmek için colsums ifadesini kullanırız. 
 
matris <- matrix(c(1:5, NA), nrow = 2)
veri <- as.data.frame(matris)
veri
colSums(is.na(veri))
print(sum(is.na(veri))) 

# Görüldüğü gibi "as.veritipi" şeklinde bir veriyi istediğimiz tipe çevirebiliyoruz. 
# Fakat bir verisetinde çalışırken yaptığımız dönüştürmelere dikkat etmemiz gerekir. 
# Örneğin; as.integer ve as.numeric fonksiyonları tek başına bir ifadeyle düzgün çalışırken 
# bir veriseti üzerinde çalıştırdığımızda veriyi ascii hale çevirebilir ve veri kaybı yaşayabiliriz. 
# Ayrıca kategorik değişkenleri numerik olarak almamak için numeric ve integer fonksiyonlarını 
# dikkatli kullanmalıyız.
 
a <- "12"
b1 <- as.integer(a)
b2 <- as.numeric(a)
print(a)
print(b1)
print(b2) 
#Vektör oluşturmak istediğimizde seq() fonksiyonunu kullanırız. 
d <- seq(from=1, to = 5, by = 0.5) 
d 

# Verisetinin içinden belirli özelliklere sahip verileri ayıklamak ve yeni bir veriseti oluşturmak 
# istersek which fonksiyonundan yardım alabiliriz.
# Örneğin Sepal Length'i 7 den büyük ve Petal Width'i de 2.1'den büyük olan gözlemleri iris veriseti 
# içinden alıp yeni_data isimli bir verisetine atayalım

yeni_data <- iris[which(iris$Petal.Width>2.1 & iris$Sepal.Length>7),] 
yeni_data





### VERİSETİNDEKİ EKSİK GÖZLEMLERİ DOLDURMANIN YÖNTEMLERİ

# 1) DOĞRUSAL İNTERPOLASYON YÖNTEMİ: Bir doğru üzerindeki eksik bir noktayı bulmaya çalışır. 
# Dolayısıyla formülü nokta formülüne benzerdir. Verisetinde doğrusallık varsa bir başka değişkendeki 
# doğrusal artışın kendi değişkenimiz üzerindeki etkisinden faydalanarak eksik değerleri bulabiliriz.
# 
# 2) MAKSİMUM BEKLENTİ YÖNTEMİ: İlk olarak belirlenen bir ratgele değer üzerinden belirlenen 
# hassasiyete ulaşılana kadar, aritmetik ortalamanın rastgele değerden farkı yine aritmetik 
# ortalamaya eklenerek devam edilir. Ortalamanın belirlenen değerden farkı hassasiyet değerinden 
# küçük olana kadar devam edilir ve belirlenen değere ulaşıldığında bütün eksik gözlemler o 
# değerle doldurulur.
# 
# 3) JACKKNIFE YÖNTEMİ: Maksimum beklentiden farklı olarak bütün eksik değerlerin aynı sonuçla 
# doldurulması yerine, her eksik değer için ayrı hesaplama yapılmasıdır. Bir kere hesaplanan değer 
# artık bilinen değer olarak kabul edilip hesalamada bu değer bilinenler arsında kullanılır.
# 
# 4) MODELLEME YÖNTEMİ: Belirli bir model belirlenerek bu model ile eksik değerler tahmin edilir.
# 
# 5) EKSİK VERİYİ SİLME: Bu yöntemde eksik veriler silinir. Fakat veriseti küçükse bu yöntemi 
# tercih etmek verisetini daha da küçülteceğinden modelin başarısını etkileyecektir.
# 
# Eksik Verilerle Çalışmak için Kullanacağımız Kütüphaneler

install.packages("VIM")
library(VIM)
library(missForest)
install.packages("mice")
library(mice)
library(ISLR) # İÇİNDEN BASKETBOL VERİSETİNİ KULLANACAĞIZ
library(Hmisc)
library(e1071) 
# Yukarıdaki Paketlerin yaptığı işleri şöyle sıralayabliriz.

#### 1)Mice Paketi: (Multivariate Imputation via Chained Equations)
# Regresyon tabanlı olarak eksik verileri tahmin eder. 
# Sürekli değişkenler için lineer regresyon kategorik değişkenler için 
# ise logistic resression kullanılır.
# Paketteki Yöntemler:
# - PMM (Predictive Mean Matching): Sayısal değişkenler için kullanılır.
# - logreg (Logistic Regression): İkili (binary) değişkenler için kullanılır.
# - ployreg (Bayesian Polynomial Regression): 2 ya da daha fazla faktör değişkenler için.
# - Proportional Odds Model: İki ya da daha fazla sıralı değişkenler için.

#### 2)missForest Paketi: 
# Adından da anlaşılacağı gibi eksik verileri random forest yöntemiyle doldurmayı sağlar. 
# Bu paketi lineer olmayan bir yöntem kullanmak istediğimizde kullanabiliriz. 
# Non-parametric tahminleme yönetemiyle çalışır, yani verinin normal dağılmış olması veya 
# 30 gözlemden büyük olması gerekmez, dolayısıyla daha özgür çalışırız. Kullanacağımız 
# fonksiyon paketadı ile aynı olan missForest fonksiyonudur. 

#### 3) VIM Paketi: 
# Özellikle eksik verilerin görselleştirilmesi için kullanılır. aggr ve barMiss fonksiyonları 
# eksik veriyi görselleştirmek için kullanılır.
    
#### 4) Hmisc Paketi:
# Bu paket daha tek bir alana yönelmek yerine daha çoklu bir kullanım amacıyla sunulmuştur. 
# Veri manipülasyonu, görselleştirmesi, eksik veri doldurma ve modelleme için birçok fonksiyon 
# içerir. help("hmisc") yazarak özellik ve fonksiyonlara ulaşılabilir.
    
#### 5)e1071 Paketi:
# Bu paket SVM algoritması ile ilgili uygulamaları içerir. Eksik verileri SVM algoritması 
# kullanarak doldurmak istiyorsak bu paketi kullanırız.
# Eksik verileri görselleştirmek için VIM kütüphanesini kullanabiliriz. Bu kütüphane içinde 
# veri doldurmak için de yöntemler vardır.


veri <- Hitters
aggr(veri)
barMiss(veri) 

# Şimdi kendimiz eksik veriler oluşturup gözlemleyelim. İris verisetini kullanacağız. 
# Bunun için missforest içinden eksik veri oluşturmaya yarayan prodNa isimli bir fonksiyon 
# kullanacağız. Daha sonra mice kütüphanesinden md.pattern fonksiyonu ile eksik verileri 
# görselleştireceğiz.
 
missedData <- prodNA(iris, noNA = 0.2) #0.2 oranında rasgele eksik gözlem oluşturacak
missedData <- subset(missedData, select = -c(Species)) #Tür değişkenini çıkardık.
md.pattern(missedData, rotate.names = TRUE) #Eksikleri Bulma ve Görselleştirme
#Bir de VIM ile görselleştirelim ek olarak.
mice_plot <- aggr(missedData, labels=c("SL", "SW", "PL", "PW"), gap=3,
                  ylab=c("Eksik Veri", "Örüntü"), col=c("skyblue", "orange"))
barMiss(missedData)
mice_plot 




### EKSİK VERİYİ PMM (PREDICTIVE MEAN METHOD) İLE DOLDURMA
 
predictedData <- mice(missedData, m = 5, method = "pmm", maxit = 5)
summary(predictedData) # özetleyelim
# 5 iterasyon yaptıktan sonra 5 farklı veri tamamlama alneratifi oluştu.
# Biz ise 3. iterasyona göre veriyi tamamlamayı seçtik.
irisPredicted <- complete(predictedData,3) 

# Orjinal ve tahmin edilen değerleri çizdirelim. Kırmızı noktalar tahmin değerleri 
# iken çarpı işaretleri orjinal noktaları gösteriyor.

library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point(shape=4,  size=4) + 
  geom_point(aes(irisPredicted$Sepal.Length, irisPredicted$Sepal.Width),color="red", size=2)






### KORELASYON
# Korelasyon iki değişken arasındaki doğrusal ilişkidir. Aynı yönlü veya ters yönlü olabilir 
# ve dolayısıyla -1 ve +1 arasında değişir. ( 0 noktası ilişkinin olmadığı durumu gösterir). 
# Fakat kesin bir şekilde belitmek gerekir ki; KORELASYON NEDENSELLİK DEĞİLDİR! Yani iki veya 
# daha fazla değişken arasında bir ilişki olması bunun belirli bir nedeni olduğunu göstermez.
# Peki korelasyonu nasıl kullanırız? 
# Eğer verisetimizde bağımsız değişkenlerden bazıları arasında güçlü bir korelasyon varsa 
# (+1 veya -1'e yakın) bu değişkenlerden sadece birini modelimizde kullanmamız yetecek demektir.
# Eğer bağımlı değişkenimiz ile bazı bağımsız değişkenler arasında güçlü bir korelasyon 
# varsa burada çok daha dikkatli olmak gerekir. Çünkü bağımlı değişken ile güçlü lorelasyona 
# sahip olan bağımsız değişkenimiz, model kurulduğunda diğer değişkenleri baskılayacak ve 
# böylece tek tahmin edici kendisiymiş gibi hareket edecektir. Bu da modelde bias hatasına 
# (yanlılık sorunu) sebep olacaktır. Bu tür bağımsız değişkenleri modelde kullanmamak gerekir.
# Farklı hesaplanan korelasyon türleri vardır. Burada default olarak kullanaılanlar pearson 
# korelasyonu üzerinden hesaplanmıştır.

#### CORRPLOT PAKETİ

library(corrplot)
data <- subset(x = iris, select = -c(Species))
(KOR <- cor(data)) # parantez içinde yazılan ifade ekrana da yazdırılır.
#grafiğer dökerek gösterirsek daha anlaşılır olur. 
#3 farklı görünümde grafik çizelim.
corrplot(KOR, method = "circle")
corrplot(KOR, method = "number")
corrplot(KOR, method = "pie")

# Görüldüğü gibi Petal Width ve Petal Length arasında pozitif yönlü güçlü bir ilişki var. 
# Aynı zamanda Petal Length ile Sepal Length arasında da güçlü bir ilişki var. O zaman bu 
# üçünden sadece birini (Petal Length'i) kullanmak yeterlidir. Hatta kısa ve basit bir modelle deneyelim.

model_tam <- lm(as.integer(Species)~., data = iris, method = "qr")
yeni_data <- subset(iris, select = c(-Petal.Width, -Sepal.Length))
model_az_veri <- lm(as.integer(Species)~., data = yeni_data, method = "qr")
tam_tahmin <- predict(model_tam, iris[,1:4])
az_veriyle_tahmin <- predict(model_az_veri, yeni_data[,1:2])

# Yukarıdaki verilerle yapılan tahmin tamsayı sonuç değil noktalı değerler döndürecek. 
# Biz bu verileri tamsayı yaparak sonuçlara bakalım 
# 
# Aşağıda for döngüsüyle belirli aralıklardaki değerleri yakın olduğu tamsayıya yuvarladık. 
# R içinde bunu çok daha kısa yollarla yapabilirsiniz ve çok gerekmedikçe for döngüsü 
# kullanmak mantıklı değildir. Ama sadece örnek amaçlı olduğundan yapıyoruz şimdilik.

a <-1
for (x in tam_tahmin) {
    if (x>0 && x<=1.5) {
        tam_tahmin[a] = 1
    }
    if (x>1.5 && x<=2.5) {
        tam_tahmin[a] = 2
    }
    if (x>2.5){
        tam_tahmin[a] = 3
    }
    a = a + 1
}
a<-1
for (x in az_veriyle_tahmin) {
    if (x>0 && x<=1.5) {
        az_veriyle_tahmin[a] = 1
    }
    if (x>1.5 && x<=2.5) {
        az_veriyle_tahmin[a] = 2
    }
    if (x>2.5){
        az_veriyle_tahmin[a] = 3
    }
    a = a + 1
}
yarım_verili_model_hatası <- 0
tam_verili_model_hatası <- 0
sıra_no <- 1
for (i in as.integer(iris$Species)) {
    if (i != az_veriyle_tahmin[sıra_no]) {
        yarım_verili_model_hatası = yarım_verili_model_hatası + 1
    }
    if (i != tam_tahmin[sıra_no]) {
        tam_verili_model_hatası = tam_verili_model_hatası + 1
    }
    sıra_no = sıra_no + 1
}
print("Tam verili model MSE:")
ModelMetrics::mse(as.integer(iris$Species), tam_tahmin)
print("Tam verili model hata sayısı:")
print(tam_verili_model_hatası)
print("Yarım verili model MSE:")
ModelMetrics::mse(as.integer(iris$Species), az_veriyle_tahmin)
print("Yarım verili model hata sayısı:")
print(yarım_verili_model_hatası)

# Gördüğümüz gibi iki sütünu kaldırmamıza rağmen sadece 2 tane daha fazla hata yaptık. 
# Yani aslında verinin yarısıyla aynı sonucu verebilecek bir model kurabildik. 
# Şimdi burada bahsetmemiz gereken konu boyutsallık lanetidir.



### CURSE OF DIMENSINALITY (BOYUTSALLIK LANETİ)

# Daha önce de dediğimiz gibi (en başta) boyut arttıkça karmaşıklık artar ama model başarısı 
# için aynı şey söylenemez. İşte yukarıda verinin yarısını attığımız halde neredeyse aynı 
# başarıyı almamızı sağlayan şey de boyutsallık laneti dediğimiz kavramla çok benzerdir. 
# Yukarıda yaptığımız gibi verinin daha az kısmıyla da aynı başarı yakalanabilir. Hatta bazen 
# başarının artması bile söz konusu olabilir. Bu yüzden bütün veriyi alarak çalışıp modeli 
# karmaşıklaştırmak yerine boyut azaltma yöntemlleri kullanarak daha basit, daha açıklanabilir 
# modeller kurmak çok daha mantıklıdır. 
# Boyut azaltmak için çeşitli yöntemler bulunmaktadır. Bunlardan bazıları:
# -PCA (Principal Component Analysis) - Temel Bileşenler Analizi
# -LDA (Linear Discriminant Analysis) 
# -SWD
# -MARS
#     Bizim burada açıklayacağımız yöntem PCA yöntemidir.
    
#### PCA (Principal Component Analysis) - Temel Bileşenler Analizi
    
# Principal Component Analysis verisetinin kendi değişkenleri ile ifade edilmesinin yerine, 
# bu değişkenleri kullanarak kendi değişkenlerini (PC) oluşturur. Yani kendisi yeni bir 
# boyut tanımlayarak veriyi bu boyutta ifade eder. Bunu yaparken de veriseti içinde veriye 
# en çok katkı yapanları bir noktada toplayarak çalışır. PCA özvektör ve özdeğerler 
# üzerinden hesaplama yaparak sonuç hesaplar.
# Büyük bir verisetindeki en önemli değişkenler hangileridir? 
# - Bileşenlerin oluşturulması (PC)
# - Yüksek boyutlardaki verisetinin mümkün olan en fazla enformasyonu içerecek şekilde 
# daha düşük bir boyutta incelenmesi
# - Daha az bileşenle daha fazla anlama sahip görselleştirmenin yapılması
# - 3 veya daha fazla boyutlu verisetlerinde uygulanması daha iyi sonuçlar elde edilmesini sağlayacaktır.


library("devtools")
install_github("vqv/ggbiplot")
install.packages("ggbiplot")
library("ggbiplot")

rm(mtcars)
data("mtcars")
pca_info <- prcomp(mtcars[, c(1:7, 10,11)], center = TRUE, scale. = TRUE)
summary(pca_info)
ggbiplot(pca_info)
ggbiplot(pca_info, labels = rownames(mtcars))

# PCA Analizi yapıldığında 2 tane bileşen oluşturuldu. Bunlardan PC1 verisetindeki 
# varyansın %62.8'ini açıklarken, PC2 %23.1'ini açıklıyor. Toplamda bütün varyansın %80'inden 
# fazlası bu iki bileşenle açıklanabiliyor. Verisetindeki özelliklerin bu yeni bileşen 
# boyutları üzerindeki yön ve değerleri de grafikteki gibi oluşmuş.
# PCA yönteminin matematiksel açıklaması internette bulunabilir. Ama grafikte gördüğümüz 
# veriseti özelliklerinin PC1 ve PC2 uzayı üzerindeki dağılımının özvektörlerle ve 
# özdeğerlerle oluştuğunu anlayabiliyoruz.


### UNSUPERVISED LEARNING (DENETİMSİZ VEYA GÖZETİMSİZ ÖĞRENME)

# Denetimli öğrenme algoritmalarında veriden bir model kurulduğunda o verideki özellikler 
# ile tahmin edilen gözlemin gerçek sonucu da verimizde etkiket halinde bulunur. 
# Biz verimiz üzerinde model kurarken aslında modelin hangi sonuçlara ulaşması gerektiğini, 
# hangi tahminleri yapması gerektiğini biliriz. Çünkü gerçekte gözlenen değeri de biliyoruzdur. 
# 
# Fakat bir verisetinden ne öğreneceğimizi bilmiyorsak ve bazı yöntemler ile yine de veriyi 
# modellemek stiyorsak ne yaparız? İşte o zaman da denetimsiz öğrenme yöntemleri kullanılır. 
# Denetimsiz öğrenmede bağımlı-bağımsız değişken ayrımı yoktur, verinin tamamından bir sonuca 
# ulaşılmaya çalışır. 
# 
# Örneğin, bir marketin müşterileri belirli segmentlere ayrılıp buna göre pazarlama yöntemleri 
# geliştirilmek isteniyor olsun. Burada model kuracak olan kişi önceden belirlenmiş bir 
# segmentasyon verisine sahip değildir. Bu yüzden müşterileri otomatik olarak kümeleyip ayırabilen 
# bir model kurmak zorundadır.
# 
# En sık kullanılan unsupervised learning yöntemi kümeleme algoritmalarıdır.
# Kümeleme birbirine benzeyen veya farklılaşan verileri bir araya toplayıp diğerlerinden ayırarak, 
# belirli parçalar (kümeler) oluşturma işlemidir. 
# 
# Veriden bahsettiğimizde aslında her zaman bir veri uzayından bir çok boyutlu uzaydan bahsederiz. 
# Dolayısıyla bu uzaydaki veriler de birbirine göre uzaklıklarına göre kümelenebilir. 
# Kümeleme algoritmaları da bu uzaklıklardan yararlanır. Dolayısıyla burada en önemli parametre 
# hangi uzaklık ölçüm yöntemini kullanacağını belirlemektir. Öklid, Manhattan, Minkowski vb yöntemler 
# kullanılabilir.
# 
# Kümeleme algoritmalarında her kümedeki elemanların o kümenin merkezine olan uzaklıkları 
# hesaplanarak bir küme belirlenmeye çalışılır. Kaç tane küme oluşturulacağına ise WCSS (Within 
# cluster sum of squares) gibi bazı değerlere bakılarak karar verilmeye çalışılır. 
# 
# 3 Tür Kümeleme Modeli Oluşturulabilir:
# 
# - Bağlantı Tabanlı Modeller: Farklı grup üyeleri arasındaki farklılığa bağlı olarak yapılan
# hiyerarşik kümeleme yöntemidir. (SLINK Modeli)
# 
# - Yoğunluk Tabanlı Modeller: Bir noktanın kendisi ile olan farklılıkları belli bir değerden 
# az olan belli bir sayıdaki diğer noktalar ile çevrelenmesine bağlı olarak yapılan kümeleme/gruplama 
# türüdür. (DBSCAN Modeli)
# 
# - Centroid Tabanlı Modeller: Her küme o kümenin merkezi durumundaki tek bir nokta ile ifade edilir. 
# (K-MEANS Modeli) 
# 
# Genel olarak istediğimiz şey küme elemanları arası benzerlik en yüksek (mesafe en düşük), 
# kümeler arası benzerlik en düşük veya farklılık en yüksek (mesafe en yüksek) olmalıdır. 
# 
# İris verisini gerçekte nasıl kümelendiğini görelim

ggplot(iris, aes(Petal.Length, Petal.Width, color=Species)) + geom_point()

# Şimdi ise kendimiz bir model ile algoritma oluşturup nasıl kümelendiğine bakalım


irisCluster <- kmeans(iris[,3:4], centers = 3, nstart = 20)
ggplot(iris, aes(Petal.Length, Petal.Width, color=as.factor(irisCluster$cluster))) + geom_point()

#Aşağı yukarı benzer bir kümeleme yapıldığını görebiliyoruz.
#Bir tablo oluşturup hatalara bakalım

table(irisCluster$cluster, iris$Species)

#Hesaplamayı farklı yapmasından dolayı asal köşegen üzerinde görememiş olsak bile aslında 
#anlaşılıyor ki toplamda 6 hatalı gözlem tahminimiz olmuş. Grafikteki benzerlik de görülüyordu zaten.

### SUPRVISED LEARNING (GÖZETİMLİ/DENETİMLİ ÖĞRENME) ALGORİTMALARI

# Yukarıda anlattığımız k-means yönteminde bu sefer verisetindeki sınıfların önceden etiketlenmiş 
# olduğunu, yani, sınıfları artık bildiğimizi düşünelim.
# Yani iris verisetindeki (orjinaldeki) hangi gözlemin hangi türe ait olduğunu biliyorsam artık 
# gözetimli bir algoritma uygulayabilirim demektir. 
# 
# Bunun için knn algoritmasını kullanacağız. Fakat modelin nasıl çalıştığını test etmek için bu 
# sefer test ve train olarak ayırmam gerekecek. 
# 
# Verisetini 3 şekilde ayırabiliriz:
# 
# - Hold Out Yöntemi (Ayırıp Tutma)
# 
# - K-FOLD Yöntemi (K Katlı Çarpraz Doğrulama)
# 
# - LOOCV - Leave One Out Yöntemi
# 
# En sık karşılaşılan yöntem hold out olmasına rağmen tavsiye edilmez. Fakat kolay olması açısından 
# biz de burada bunu kullanacağız.







#### KNN (K NEAREST NEIGHBORHOOD) / K (SAYIDA) EN YAKIN KOMŞU ALGORİTMASI

# KNN algoritması k--means gibi bir kümeleme algoritmasıdır.(aslında sınıflandırma algoritmasıdır 
# ama şimdilik pek fark yok denilebilir.)  Fakat gözetimli bir algoritma olarak çalışır. Algoritma 
# gözetimli olduğu için verisetindeki kümeler bilinir. Dolayısıyla kümeleri kendisi bulmak yerine 
# şunu yapar; Yeni bir gözlem geldiğinde önceden gördüğü ve öğrendiği verisetinde (uzayda) yeni gelen 
# noktaya en yakın k sayıda noktaya bakar. Baktığı k sayıda nokta çoğunlukla hangi kümeye aitse 
# veya yeni nokta hangilerine daha yakınsa, yeni gelen gözlemi de o kümedendir diye tahmin eder. 
# Tıpkı k-means gibi burada da mesafe ölçmek için kullanılacak yöntem çok önemlidir. Fakat bundan 
# daha önemli olan bakılacak komşu sayısının yani k değerinin kaç olduğunu belirlemektir. Bazı 
# kaynaklarda k değerinin gözlem değerinin karekökü olarak belirlenebileceği belirtilirken, bazı 
# kaynaklarda ise gözlem sayısının karekökünü orta nokta kabul edip ona göre k değerleri hesaplamak 
# önerilir.

library(class)
library(caret)
data("mtcars")
veri <- mtcars
veri$am <- as.factor(veri$am)
ayir <- createDataPartition(y=veri$am, p = 0.75, list = FALSE)
egitim <- veri[ayir,]
test <- veri[-ayir,]
egitim_v <- egitim[,-9]
test_v <- test[,-9]
egitim_h <- egitim[[9]]
test_h <- test[[9]]
tahmin <- knn(egitim_v, test_v, egitim_h, k = 6)
(cm <- confusionMatrix(test_h, tahmin))

# Yukarıda modelin sonuçları görülüyor. Fakat Accuracy, Sensitivity, Specificity, Kappa gibi 
# birçok metrik ölçülmüş. Bunların hepsi iyi olsa bile bu modelin başarılı olduğu anlamına gelmez. 
# Şöyle bir örnek verelim; 100 tane kedi köpek resmi arasından hangisinin kedi hangisinin köpek 
# olduğunu bulmak istediğimizde, eğer test setimizde sadece 10 kedi resmi varsa ve model 
# hepsini köpek olarak tahmin etmişse, Accuracy %90 ve Sensitivity %100 çıkmasına rağmen 
# Specificity 0% çıkar. Köpek olma durumunu pozitif olarak belirlediğimizde;
# 
# Sensitivity = TP/(TP + FN) = 100% (Modelin köpekleri tanıma durumu)
# 
# Specificity = TN/(TN + FP) = 0%  (Modelin kedileri tanıma durumu)
# 
# Accuracy = (TN + TP)/(TN+TP+FN+FP) = 90% (Toplam başarı)
# 
# Bu demektir ki bizim modelimiz kedi olma durumunu hiç öğrenememiş. Yani karşımıza bir kedi 
# geldiğinde onu tanımamız mümkün değil. Hatta ve hatta model hiç bir şey öğrenememiş ve her 
# gelen veriye köpek diyip geçmiş de olabilir. Yani böyle bir durumda model aslında köpekleri 
# de tanımıyor olabilir. 
# 
# Dolayısıyla tek bir ölçüm yöntemine, hatta 2-3 tanesine göre bile karar vermek doğru değildir. 
# ROC eğrisi çizdirmek, F-Testi değeri, vs. gibi başka yöntemler de her zaman tercih edilmelidir.


## SON 