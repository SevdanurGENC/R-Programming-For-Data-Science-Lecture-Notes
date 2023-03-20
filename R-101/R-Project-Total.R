## Operatorler

### Atama Operatorleri
# <- 
# =
# <<-

a <- 10 
b <- 15
a = 19
rnorm(10, mean = 9 , sd = 2 )
#fonksiyon ici atama islemine ornek olmasi icin konulmustur.

### Aritmetik Operatorler
# + - * / ^ 
10 + 10 + 20
a <- 90*2 
a * b
a/b
b^2

### Mantiksel Operatorler
# >, <, >=, <=, !=, |, &, ==

40 < 10
veri <- 1:10
veri[veri > 5 ]
veri[veri <= 5 ]
veri[veri == 3 ]
veri[veri != 5 ]
veri[veri > 1 & veri < 3 ]
veri[veri > 1 | veri < 3 ]

#Matematiksel Islemler
#*, +, -, /, ^
#sqrt
#abs
#log, log10, log2
#exp(3)

a <- sqrt(9) #karekok
b <- abs(-19) #mutlak deger
a*b #iki degerin carpilmasi
c <- a*b #carpilan ifadenin kaydedilmesi 
(c <- a*b) #hem kayit hem de consol ciktisi
(d <- a*b) 
log(9) 
log2(9)
exp(3)
exp(5)
d^2
sin(180)
cos(180)


#Temel Fonksiyonlar
sessionInfo() #sistem bilgileri
getwd() #mevcut  dizin
dir() #dizin altindaki dosyalar
list.files() #dizin altindaki dosyalar
file.exists("veriler") #belirli bir dosyanin sorgulmasi
dir.create("veriler") #dizin olusturmak
object.size("R-Project-Total.R") #nesne boyutu
ls() #glob. env. kismindaki nesneler
rm(a) #glob. env. kismindaki nesneleri silmek
available.packages() #erisilebilecek tum kutuphaneler
installed.packages() #yuklu kutuphaneler
data() #r'daki veri setleri
options(digits = 10) #virgulden sonraki basamak ayari
print(3.1111111111111111)


## Atomic Vektorler 

# numeric
a <- 1
typeof(a) #tip sorgulamasi
is.vector(a) #vektor sorgulamasi


# integer
b <- 1L
typeof(b)
is.vector(b)


# character
c <- "bu bir karakterdir"
typeof(c)
is.vector(c)


# logical
e <- 5
f <- 1
log <- e < f
typeof(log)
is.vector(log)

# complex
z <- 1 + 2i
typeof(z)

# vektor
a <- 1:10
a <- 19:45
is.vector(a)
a <- c("a","b","c","d")
b <- c(1,2,3,4,5,6)
is.vector(a)
is.vector(b)
a[0]
a[5]
a[3]

# liste
list(111, "merhaba" , 1 + 2i)
Liste <- list(111, "merhaba" , 1 + 2i)
Liste
Liste[2]


# matris
m  <- matrix(1:9, nrow = 3 ,  ncol = 3)
m


# data frame
data.frame(
  isim = c("Ali","Cengiz","Yildiz"),
  yas = c(18,"02",1989)
)

df <- data.frame(
  isim = c("Ali","Cengiz","Yildiz"),
  yas = c(18,"02",1989)
)

df


#R'da nasil fonksiyon yazilir?
kare_fonksiyonu <- function(x, y) { 
  a <- x^2
  y*a 
}

#fonksiyonu calistirmak icin
kare_fonksiyonu(4, 3)


#Ne zaman fonksiyon yazmali?
df <- data.frame(
  a = c(1:10), 
  b = c(2:11),
  c = c(4:13),
  d = c(9:18)
)
df

# Her bir degiskeni standartlastirmak istiyoruz. 
# Bunu her birisi icin  el ile yapacak olursak
# (X - M) / SIGMA/KOK N

(df$a - mean(df$a)) / sd(df$a) / sqrt(length(df$a))

(df$b - mean(df$b)) / sd(df$b) / sqrt(length(df$b))

(df$c - mean(df$c)) / sd(df$c) / sqrt(length(df$c))

(df$d - mean(df$d)) / sd(df$d) / sqrt(length(df$d))

#olcek fonksiyonu
olcek <- function(x) { 
  (x - mean(x)) / sd(x) / sqrt(length(x)) 
} 

olcek(df$d) 

#IF-ELSE
sayi <- 6

if (sayi > 7) { 
  print("sayi 7'dan buyuktur") 
} else { 
  sayi * 9  
}
 
#### FONKSIYON ICINDE KONTROL IFADESI ####

df <- data.frame(
  a = c(1:10), 
  b = c(2:11),
  c = c(4:13),
  d = c(9:18),
  f = c("a","b","c","d","c","d","d","a","a","l")
)



olcek <- function(x) { 
  if(is.numeric(x) == FALSE ) { 
    message("LUTFEN NUMERIC BIR DEGISKEN GIRINIZ") 
  }  
  (x - mean(x)) / sd(x) / sqrt(length(x)) 
} 

olcek(df$f) 


#Ikinci argumanin eklenmesi
olcek <- function(x, n) { 
  if(is.numeric(x) == FALSE ) { 
    message("LUTFEN NUMERIC BIR DEGISKEN GIRINIZ") 
  } 
  (x - mean(x)) / sd(x) / sqrt(n) 
}


olcek(df$a, 100) 

olcek(df$a)


#On tanimli arguman belirlemek
olcek <- function(x, n =  10) { 
  if(is.numeric(x) == FALSE ) { 
    message("LUTFEN NUMERIC BIR DEGISKEN GIRINIZ") 
  } 
  (x - mean(x)) / sd(x) / sqrt(n) 
} 

olcek(df$a)  

## IC ICE KOSUL 
a <- 100

if (is.character(a) == TRUE) { 
  paste(a, "OK karakter") 
} else if (is.Date(a) == TRUE) { 
  paste(a, "tarih bu") 
} else if (is.numeric(a) == TRUE)  { 
  print("bu sayi numeric") 
} 
 
class(a) 

a <- as.Date("1989-02-18") 

#FOR

#Klasik
for (i in 1:5) print(1:i)


#Vektor icerisinde dondurme
a <- c(1,3,5,7,2)

for (i in a) { 
  print(i^2) 
}
 
##elimdeki sayilarin alfabedeki karsiliklari neler 
sayilar <- c(14,11,10,16,25,26)
letters[26]

for (i in sayilar) { 
  beraber <- paste(i, letters[i])
  print(beraber)
}
 
paste("a","b")
 
#### factor icinde dongu dondurme
df <- data.frame( 
  a = c(1:10), 
  b = c(2:11),
  c = c(4:13),
  d = c(9:18),
  f = c("a","b","c","d","c","d","d","a","a","l") 
) 

#while
i <- 1

while (i < 6) { 
  print(i) 
  i = i+1 
}


#break 
x <- 1:10

for (i in x) { 
  if (i == 7) { 
    break 
  } 
  print(i) 
}
 
#next
x <- 1:10

for (i in x) { 
  if (i == 7) { 
    next 
  } 
  print(i) 
} 


# ------   R icerisindeki veri setleri

data() #aktif kutuphanelerin icindeki veri setleri
veri <- AirPassengers
veri

e <- euro
e

data(package = .packages(all.available =  TRUE))#tum veri setleri

#Bu bolumdeki veri setleri Udemy uzerine indirilebilir dokuman olarak eklenmistir.
 

# -----   sik kullanilan fonksiyonlar
read.table("/Users/Nano/Desktop/R-Repo/Datasets/sherlock.txt")
read_delim("/Users/Nano/Desktop/R-Repo/Datasets/train.csv", delim = ";")
read_excel("/Users/Nano/Desktop/R-Repo/Datasets/linelist_raw.xlsx")
read_dta("/Users/Nano/Desktop/R-Repo/Datasets/oil.dta")


# ------ KULLANICI GIRISI
a <- scan()
b <- readline()
 

## GENEL NOTLAR ##

#Isimlendirmeler mumkun oldugu kadar kisa ve anlasilir olmali.
#Isimlendirmeler yapilirken varolan isimlendirmelerden farkli isimler kullanilmali.
#Isimlendirmeler kucuk harfler tercih edilmesi

#Operatorlerin etrafinda bosluklar olmali.
#Suslu parantez kullanimi tercih edilmeli ve kod yazimina alt satirdan basanmali.
#Satir uzunlugu kod okunabilirligi acisindan 80 karakteri gecmemelidir.
#Girinti icin iki bosluk kullanilmali
 
## ATAMA OPERATORU KULLANIMI ##

#fonksiyon iclerinde = diger tum durumlarda atama operatoru

a <- 1
a = 1

## DEGISKEN VE FONKSIYON ISIMLENDIRME ###

# Good
day_one
day_1

# Bad
first_day_of_the_month
DayOne
dayone
djm1
 
##  DOSYA ISIMLENDIRME ##  
# Good
fit-models.R
utility-functions.R

# Bad
foo.r
stuff.r
  
## BOSLUK KULLANIMI ##

# Good
average <- mean(feet / 12 + inches, na.rm = TRUE)

# Bad
average<-mean(feet/12+inches,na.rm=TRUE)



## Operatorlerin etrafinda her zaman bosluk olmayabilir.

# Good
x <- 1:10
base::get

# Bad
x <- 1 : 10
base :: get

  
## FONKSIYONLARDA BOSLUK KULLANIMI ##

# Good
if (debug) {
  
  do(x) 
  
} 

plot(x, y)

# Bad
if(debug)do(x)
plot (x, y)
 
# Good
if (debug) do(x)
diamonds[5, ]

# Bad
if ( debug ) do(x)  # No spaces around debug

x[1, ]   # Needs a space after the comma
x[1 ,]  # Space goes after comma not before
 

## SUSLU PARANTEZ ##

# Good 
if (y < 0 && debug) {
  
  message("Y is negative")
  
} 

if (y == 0) {
  log(x)
} else {
  y ^ x
}


# Bad

if (y < 0 && debug)
  message("Y is negative")

if (y == 0) {
  log(x)
} else {
  y ^ x
}

 