theme_set(theme_bw(base_family="AppleGothic"))

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap")
register_google(key='AIzaSyCjTowht2164iGxK2MgiVXXrJAWFqDEkvI')
library(ggmap)
getwd()
setwd("/Users/kimdongkyu/Desktop/Data(f)")
a<-read.csv("WIFI.csv",header=TRUE,fileEncoding = 'euc-kr')
a

seoul<- get_map("seoul", zoom=11, maptype ="roadmap")
ggmap(seoul)

seoul2<- ggmap(seoul) + geom_point(data= a, aes(x=LON, y=LAT, color =INSTL_DIV),size=3, alpha=0.8)
seoul2

seoul2 + facet_wrap(~GU_NM)

ggplot(a, aes(x= factor(1))) + geom_bar(aes(fill=GU_NM), width=1) + coord_polar(theta ="y") +xlab("") + ylab("")

korea <-c(left=124, bottom=33, right=132, top=40)
#70년도 지도 스타일
get_stamenmap(korea, zoom=5, maptype="toner-lite") %>% ggmap()
#기본형 
get_googlemap("seoul", zoom=12) %>% ggmap()
#위성 사진. 산 물 --> 초록색, 땅 --> 회색, get_googlemap로 불러와서 뒤에 %>%ggmap(프린팅 해주는 기능)만 해주면 지도가 프린팅 됨. 
get_googlemap("seoul", zoom=12, maptype="satellite") %>% ggmap()
get_googlemap("seoul", zoom=12, maptype="hybrid") %>% ggmap()
#기본 로드맵
get_googlemap("seoul", zoom=12, maptype="roadmap") %>% ggmap()
#줌을 14이상으로 하면 지하철 노선도도 보임
map <- get_map(location="seoul", zoom=14, maptype="roadmap", source="google")
g<-ggmap(map)
print(g)
#The geocode function uses Googles Geocoding API to turn addresses from text to latitude and longitude pairs very simply.
landmarks <- c("nseoul tower, seoul", "city hall, seoul")
lbls<-cbind(geocode(landmarks), text=landmarks)
g<-ggmap(map)
g<-g + geom_point(data=lbls, aes(x=lon, y=lat), size=5, color="orange")
g<-g + geom_point(data=lbls, aes(x=lon, y=lat), size=3, color="red")
g<-g + geom_text(data=lbls, aes(x=lon, y=lat, label=text), size=5, color="blue", hjust=0, vjust=0)
print(g)

cent<- c(mean(a$LON), mean(a$LAT))
attach(a)
bmap<-ggmap(get_googlemap(center=c("seoul"), zoom=11, maptype="roadmap")) +
  geom_point(data=a, aes(x=LON, y=LAT, color=INSTL_DIV,size=4))
print(bmap)

bmap + facet_wrap(~INSTL_DIV)
ggplot(a, aes(x=factor(1)))+ geom_bar(aes(fill=INSTL_DIV),width=1) +
  coord_polar(theta="y") +xlab("") + ylab("")

install.packages("extrafont")
library(extrafont)
font_import()
fonts()
font_import(pattern = "NanumGothic")


ggplot(a, aes(CATEGORY))+geom_bar(aes(fill=INSTL_DIV))
ggplot(a, aes(INSTL_DIV)) + geom_bar(aes(fill=CATEGORY)) 

##건강검진 수검 데이터
setwd("/Users/kimdongkyu/Desktop/DATA")
lonlat<-read.csv("MF1.csv", header=T, fileEncoding = 'euc-kr')

rmap<- ggmap(get_googlemap(center = c(127.6607,36.0068), zoom=7,maptype="roadmap"))+
  geom_point(data=lonlat,aes(x=LON, y=LAT, color=RATE), size=2)
print(rmap)

rmap<- ggmap(get_googlemap(center = c(127.6607,36.0068), zoom=7, maptype ="roadmap"))+
  geom_point(data=lonlat,aes(x=LON,y=LAT, color=INCOME), size=2)
print(rmap)

# facet_wrap --> 네 개로 쪼개라
rmap+ facet_wrap(~RATE)
rmap + facet_wrap(~INCOME)

install.packages("ggplot2")
library(ggplot2)
library(devtools)

health <- read.csv("checkup2.csv", header=T)
head(health)
cent<-c(mean(health$lat), mean(health$lon))
attach(health)
bmap<- ggmap(get_googlemap(center = c("southkorea"), zoom=7, maptype="roadmap")) + 
  geom_point(data=health, aes(x=lon, y=lat, size= number), shape =16, color = "purple", alpha = 0.5)+
  scale_size_area(max_size=10)
print(bmap)

people <- read.csv("checkup2.csv", header=T)
cent<- c(mean(people$lat), mean(people$lon))
attach(people)
fmap<-ggmap(get_googlemap(center=c("southkorea"), zoom=7, maptype="roadmap")) +
  geom_point(data=people, aes(x=lon, y=lat, color= examinee), shape=16, alpha=0.9, size=3)
print(fmap)

pigmap<- ggmap(get_map(location='south korea', zoom=7, color='bw'))

#as.is=T 뜻: 비수치형 데이터를 요인으로 해석하면 안된다는 뜻. 
pig15 <-read.csv("map.csv", header=T, as.is=T, fileEncoding = 'euc-kr')
attach(pig15)
head(pig15)

ppp15<- subset(pig15, 년도=="15")
ppp15map<- pigmap+ geom_point(data=ppp15, aes(x=lon, y=lat))
ppp15map<- ppp15map + geom_text(data=ppp15, aes(x=lon+0.01, y=lat+0.01, label=위치, family="AppleGothic"),size=2.5, check_overlap=T)
ppp15map + geom_point(data=ppp15, aes(x=lon, y=lat, color=factor(위치)), size=2) +
  scale_color_discrete(name="위치") #범례 제목 달기 이산-->discrete 연속-->continuous

loc<- read.csv("서울_강동구_공영주차장_위경도.csv", header=T, fileEncoding = 'euc-kr')
loc

kd<- get_map("Amsa-dong", zoom=13, maptype="roadmap")

kor.map <- ggmap(kd) + geom_point(data=loc, aes(x=LON, y=LAT), size=3, alpha=0.7, color="red")
  
kor.map + geom_text(data=loc, aes(x=LON, y=LAT+0.001, label=주차장명), size=3)
ggsave("kd.png",dpi=500)  

pop<- read.csv("지역별인구현황_2014_4월기준.csv", header=T, fileEncoding = 'euc-kr')
pop

lon<- pop$LON
lat<- pop$LAT
data<- pop$총인구수

df<- data.frame(lon,lat,data)
df  

map1<- get_map("Jeonju", zoom=7, maptype="roadmap")  
map1<- ggmap(map1)  
map1 + geom_point(aes(x=lon, y=lat, color=data, size=data), data=df)  
ggsave("pop.png", scale=1, width=7, height=4, dpi=1000)  

map2 <- get_map("Jeonju", zoom=7, maptype="terrain")  
map2<-ggmap(map2)  
map2 + geom_point(aes(x=lon, y=lat, color=data, size=data), data=df)  

ggsave("pop2.png", scale=1, width=7, height=4, dpi=1000)  

map3<- get_map("Jeonju",zoom=7, maptype="satellite")  
map3<- ggmap(map3)  
map3 + geom_point(aes(x=lon, y=lat, color=data, size=data),data=df)
ggsave("pop3.png", scale=1, width=7, height=4, dpi=1000)

map4 <- get_map("Jeonju", zoom=7, maptype="hybrid")
map4<-ggmap(map4)
map4 + geom_point(aes(x=lon, y=lat, color= data, size=data), data=df)
ggsave("pop4.png", scale=1, width=7, height=4, dpi=700)

map5<- get_map("Jeonju", zoom=7, maptype="roadmap")
map5<-ggmap(map5)
#stat_bin2d: Count number of observation in rectangular bins.
map5 + stat_bin2d(aes(x=lon, y=lat, color=data, fill=factor(data), size=data), data=df)
ggsave("pop5.png",scale=2, width=7, height=4, dpi=700)
