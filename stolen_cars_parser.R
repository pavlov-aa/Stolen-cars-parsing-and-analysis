######## STOLEN CARS ##############

#### installing required packages
library("httr")
library("RCurl")
library("XML")
install.packages("gridExtra")
library("gridExtra")

#### collecting data

all_posts<-as.data.frame(matrix(1:17,byrow=T,ncol=17))
names<-c("region","city","ts_type","post_type","steal_date","ts_brand","ts_model","body_type","reg_id","ts_year_prod","engine_id","body_id","frame_id","vin","colour","steal_place","e_mail")
colnames(all_posts)<-names
  
for ( i in 6134:9632) {
url<-paste("http://www.ugnaly.com/advert.php?aid=",i,sep="")
doc <- htmlParse(url)
posts <- xpathSApply(doc, "//table[@class='ads']/tr/td", xmlValue)
posts_data<-matrix(posts[3:36],ncol=2,byrow=T)
all_posts[i,]<-posts_data[,2]
}

####### preprocessing data

all_posts_copy<-all_posts
write.csv(all_posts_copy,"collected_posts.csv",row.names = F)
all_posts_filtered<-all_posts[all_posts$region!="Регион",]
all_posts_filtered<-all_posts_filtered[all_posts_filtered$ts_type!="Ошибка! сообщите администратору!",]
all_posts_filtered<-all_posts_filtered[all_posts_filtered$region!="Страна",]
all_posts_filtered<-all_posts_filtered[all_posts_filtered$post_type!="Ошибка! сообщите администратору!",]
other<-all_posts_filtered[all_posts_filtered$ts_type!="Автомобиль" &all_posts_filtered$ts_type!="Мотоцикл",]
other[,1]<-NULL
other[,17]<-0
colnames(other)<-c(names)
auto<-all_posts[all_posts$ts_type=="Автомобиль", ]
moto<-all_posts[all_posts$ts_type=="Мотоцикл", ]

posts<-rbind(auto,moto,other)
rm(all_posts,all_posts_filtered,auto,moto,other,auto_moto,posts_data)

posts$body_type<-NULL
posts$e_mail<-NULL

write.csv(posts,"processed_posts.csv",row.names = F)
#### EDA
str(posts)
summary(posts)
table(posts$post_type)


year_prod<-as.data.frame(table(posts$ts_year_prod))
year_prod$Var1<-as.numeric(levels(year_prod$Var1))[year_prod$Var1]
year_prod<-year_prod[year_prod$Var1!=0 & year_prod$Freq>5,]
colnames(year_prod)<-c("Year of Production","Quantity of posts about stolen cars")

png(filename="steals_by_year_of_production")
plot(year_prod,main="Steals by year of production")
dev.off()

brands<-as.data.frame(table(posts$ts_brand))
brands<-brands[brands$Freq>10,]
brands<-brands[order(brands$Freq,decreasing=T),]
colnames(brands)<-c("brand","Frequency")

write.csv(brands,"most_stolen_brands.csv",row.names = F)

colours<-as.data.frame(table(posts$colour))
colnames(colours)<-c("colour","frequency")
png(filename="steals_by_colour")
pie(colours$frequency,colours$colour,main="Steals by colour")
dev.off()





