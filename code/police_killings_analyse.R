# Star Grup Üyeleri :
#                   AB 16' - R ile Veri Analizi
#                   
#                   Ümit Isikdag
#                   Gözde Yüksel
#                   Ege Savci
#                   
# 

require(data.table)
library(bit64)
library(tidyr) 
library(plyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(mapproj)
veri=fread("C:\\Users\\Public\\Documents\\police_killings.csv")
getwd()


veri %>%
  select(age,gender)

subset1= veri %>%
filter(age>=30)


tarih=paste(veri$year,veri$month,veri$day,sep='-')
tarih=strptime(tarih,"%Y-%B-%d")

veri=cbind(veri,tarih)



#---------------------------California Eyaletindekileri Sec-----------------
subset2=veri %>%
  filter(state=='CA') %>%
  select(name,age,gender,city)

#gender-state
ggplot(data=veri) + geom_point(aes(x=gender,y=state,color=gender))

#her ay oldurulen kisi

ggplot(data=veri %>% group_by(month) %>% summarise(count=n()), aes(x=month,y=death)) + geom_bar(aes(y=count),stat='identity')


#her state icin ortalama issizlik hesabi ve grafigi, her state icin ortalama issizligi her satira ekledik.

subset5=veri %>%
group_by(state)%>%
  summarise(ort_issizlik=mean(urate))


ggplot(data=subset5) + geom_bar(aes(x=state,y=ort_issizlik),stat='identity')

#---------------------------------her state icin olum sayisi
subset7=veri%>%
  group_by(state)%>%
  summarise(toplam_olum=n()) 

ggplot(data=subset7) + geom_bar(aes(x=state,y=toplam_olum),stat='identity')


#----------------------raceethnicity -toplam olum karsilastirmasi

subset10=veri %>%
  group_by(raceethnicity)%>%
  summarise(toplam_olum=n())


ggplot(data=subset10) + geom_bar(aes(x=raceethnicity,y=toplam_olum,fill=raceethnicity),stat='identity')

##-----------toplam olum sayisi  bulma--------------------
subset11=veri %>%
  group_by(month)%>%
  summarise(toplam_olum=n())


#-----------------------------Haritada gosterme---------------------

map <- get_map(location = 'USA', source='stamen', maptype='watercolor', zoom = 4)
ggmap(map)

ggmap(map)+
  geom_point(aes(x = longitude, y =latitude ), data = veri,
             alpha = .5, color="darkred", size = 3)


#-------------------------------------KUMELEME ANALIZI--------------
#kumelemede iki x ve y degiskeni de numerik olmalidir.
subset12=veri %>%
  select(age,urate)%>%
  arrange(age)

subset12=scale(subset12) #her zaman sart degil degisken sayisi fazla olursa

#kume=kmeans(subset12,3,iter.max = 10)
kume=kmeans(subset12,3)

#Asagidaki plot kodu scale(subset12) uygularsak calisir
plot(subset12[,1],subset12[,2],col=kume$cluster)
#scale kullanmadan ggplot kullanarak
ggplot(data=subset12,aes(x=age, y=urate))+geom_point(color=kume$cluster)


subset14=veri %>%
  #filter(state=='CA')%>%
  select(pov,urate)
  
####clustering
subset14 = veri %>% 
  
  select(urate,pov) %>%
  arrange(urate)


kume = kmeans(subset14,3)
ggplot(data = subset14,aes(x=pov,y=urate)) + geom_point(color=kume$cluster)


#------------------------Cluster paketi yukleyip PAM ile kumeleme analizi------------------
require(cluster)
alfa= pam(subset12,3)
plot(alfa)

subset14=veri %>%
  filter(state=='CA'& state=='AL')%>%
  select(pov,urate)

#-------------------------------Regresyon Analizi---------------------------
subset15=veri%>%
  group_by(state)%>%
  summarise(toplam_olum=n()) %>%
  left_join(veri,.,by='state')

regresyon=lm("toplam_olum~pov+urate+p_income+h_income",subset15)
summary(regresyon)
