#load data
getwd()
setwd("G:/")
data1<-read.csv("cwurData.csv")

#check structure of data
str(data1)

#keeping original data safe
data_cwur<-data1

#print first 6 rows
head(data_cwur)

#Analysis ON Center for World University Rankings (CWUR)

#Descriptive Statistics

dim(data_cwur)
dim(data_shang)
dim(data_times)

summary(data_cwur)

#check for missing values
colSums(is.na(data_cwur))
library(Amelia)
missmap(data_cwur, col=c("black", "grey"), legend=FALSE)
#We see that only variable broad impact is having missing values.
#Removing Broad Impact variable because it contains missing values and imputing it is not a good option and also
#because it has data of ony two years i.e. 2014 and 2015.

data_cwur$broad_impact<-NULL
str(data_cwur)

#check class of each variable
sapply(data_cwur,class)

#check levels of factor variables
nlevels(data_cwur$institution)
nlevels(data_cwur$country)

#checking distribution of countries
sort(table(data_cwur$country),decreasing=TRUE)

#Convert year variable into factor variable
data_cwur$year<-as.factor(data_cwur$year)

cbind(frequency=table(data_cwur$country),percentage=prop.table(table(data_cwur$country)*100))
#We can see that USA>China>Japan>UK>Germany>France>Italy has highest no. of top class intitutions.
#India has 31 institutes out of 2200 top institutions that are listed in university rankings.

library(corrplot)
cor<-cor(data_cwur[,c(1,4,5,6,7,8,9,10,11,12)])
corrplot(cor,method="number")
# We see that world rank has high correlations with publications, influences and citations.
#Quality of education has high correlation with quality of faculty

#Data Visualization

library(ggplot2)
library(dplyr)

# 1. Top 5 universities according to world rank for all the years 2012-2015
data_cwur %>%
  group_by(year) %>%
  select(year,world_rank,institution) %>%
  top_n(-5,world_rank) 

data_cwur %>%
  group_by(year) %>%
  select(year,world_rank,institution) %>%
  top_n(-5,world_rank) %>%
  ggplot(aes(x=year,y=world_rank,group=institution))+
  geom_line(aes(color=institution))+
  labs(x="Year",y="World Rank",title="World Rank of universities for 2012-2015")

#We can say that Harvard University remained at 1st rank for 2012-2015. Stanford was
#ranked 3rd in 2012 and then ranked 2nd for 2013-2015.

# 2. Country name of top 10 universities for the years 2012-2015
data_cwur %>%
  group_by(year) %>%
  select(country,world_rank,institution) %>%
  top_n(-10,world_rank) %>%
  ggplot(aes(x=country,y=world_rank,color=country))+
  geom_line()+
  labs(x="Country",y="World Rank",title="Countries name of top 10 universities for 2012-2015")

#We see that only 2 countries are in top 10 i.e. USA and UK.

# 3. Top 5 Countries having high no. of institutions in world ranking
x<-head(sort(table(data_cwur$country),decreasing=TRUE),n=5)
x
barplot(x,xlab="Country",ylab="No. of institutions",main="Top 5 countries having high no. of
        institutions",col=c("grey","blue","green","violet","skyblue") )

#We see that top 5 countries having high no. of institutes in world ranking are: USA, China, Japan, UK, Germany


# 4. Year wise Top 5 institute's score
data_cwur %>%
  group_by(year) %>%
  select(year,world_rank,institution,score) %>%
  top_n(-5,world_rank)

fun<-function(yr){
data_cwur %>%
  filter(year==yr) %>%
  select(world_rank,institution,score) %>%
  top_n(-5,world_rank) %>%
  ggplot(aes(x=world_rank,y=score,group=institution))+
  geom_bar(aes(fill=institution),stat="identity")+
  labs(x="World Rank",y="Score",title=paste("Score of Top 5 institutions in",yr))
}

yr_2012<-fun(2012)
yr_2013<-fun(2013)
yr_2014<-fun(2014)
yr_2015<-fun(2015)

library(gridExtra)
grid.arrange(yr_2012,yr_2013,yr_2014,yr_2015, ncol=2)

#We see that Harvard has highest score of 100 then Stanford for 4 consecutive years 2012-2015

# 5. Year wise Top 5 institutes
data_cwur %>%
  group_by(year) %>%
  select(year,world_rank,institution) %>%
  top_n(-5,world_rank)

fun1<-function(yr){
  data_cwur %>%
    filter(year==yr) %>%
    select(world_rank,institution) %>%
    top_n(-5,world_rank) %>%
    ggplot(aes(x=world_rank,y=institution,fill=institution))+
    geom_bar(stat="identity")+
    labs(x="World Rank",y="Instituion",title=paste("Top 5 institutions in",yr))
}

yr1_2012<-fun1(2012)
yr1_2013<-fun1(2013)
yr1_2014<-fun1(2014)
yr1_2015<-fun1(2015)

library(gridExtra)
grid.arrange(yr1_2012,yr1_2013,yr1_2014,yr1_2015, ncol=2) 

# We see that Harvard was ranked 2nd in 2012 and after that it remained at 1st rank for 2013-2015.
#Also, in 2012, MIT was ranked 2nd but from 2013-2015, Stanford has got 2nd rank.

# 6. Top 3 countries having high ranking in publications

pub1<-data_cwur %>%
  filter(year==2012) %>%
  top_n(-10,publications) %>%
  ggplot(aes(x=country,y=publications,fill=country))+geom_bar(stat="identity")+
labs(x="Countries",y="Rank for Publications",title=paste("Top 3 countries who produced publications in 2012"))

pub2<-data_cwur %>%
  filter(year==2013) %>%
  top_n(-10,publications) %>%
  ggplot(aes(x=country,y=publications,fill=country))+geom_bar(stat="identity") +
  labs(x="Countries",y="Rank for Publications",title=paste("Top 3 countries who produced publications in 2013"))

pub3<-data_cwur %>%
  filter(year==2014) %>%
  top_n(-10,publications) %>%
  ggplot(aes(x=country,y=publications,fill=country))+geom_bar(stat="identity") +
  labs(x="Countries",y="Rank for Publications",title=paste("Top 3 countries who produced publications in 2014"))

pub4<-data_cwur %>%
  filter(year==2015) %>%
  top_n(-10,publications) %>%
  ggplot(aes(x=country,y=publications,fill=country))+geom_bar(stat="identity") +
  labs(x="Countries",y="Rank for Publications",title=paste("Top 3 countries who produced publications in 2015"))

grid.arrange(pub1,pub2,pub3,pub4,ncol=2)

#Top 3 countries having high ranking in quality of education are: USA>UK>Canada>Japan

# 7. Top 3 countries having high ranking in alumni employment

data_cwur %>%
  top_n(-20,alumni_employment) %>%
  ggplot(aes(x=country,y=alumni_employment,fill=country))+
  geom_bar(stat="identity")+facet_grid(.~year)+
  labs(x="Countries",y="Rank for Alumni employment",
  title="Top 3 countries having high rank in alumni employment ")

#Top 3 countries having high ranking in alumni employment are: USA>Japan>France

  # 8. Top 3 countries having high ranking in quality of education
  
  data_cwur %>%
    top_n(-20,quality_of_education) %>%
    ggplot(aes(x=country,y=quality_of_education,fill=country))+
    geom_bar(stat="identity")+facet_grid(.~year)+
  labs(x="Countries",y="Rank for Quality of education",
       title="Top 3 countries having high rank in quality of education")
  
#Top 3 countries having high ranking in quality of education are: USA>UK>Israel>France.
#For 2013 and 2014, Japan has high rank than USA.
  
# 9. Top 3 countries having high ranking in quality of faculty
  
data_cwur %>%
    top_n(-20,quality_of_faculty) %>%
    ggplot(aes(x=country,y=quality_of_faculty,fill=country))+
    geom_bar(stat="identity")+facet_grid(year~.)+
labs(x="Countries",y="Rank for Quality of faculty",
    title="Top 3 countries having high rank in quality of faculty")
  
#Top 3 countries having high ranking in quality of faculty are: USA>UK. 

# 10. Top 3 countries having high ranking for influence

data_cwur %>%
  top_n(-30,influence) %>%
  ggplot(aes(x=country,y=influence,fill=country))+
  geom_bar(stat="identity")+facet_grid(.~year)+
labs(x="Countries",y="Rank for Influence",
     title="Top 3 countries having high rank for Influence")

#Top 3 countries having high ranking for Influence are: USA>UK.
#In top 20 rankings, only USA is there.

# 11. Top 3 countries having high ranking for citations

data_cwur %>%
  top_n(-30,citations) %>%
  ggplot(aes(x=country,y=citations,fill=country))+
  geom_bar(stat="identity")+facet_wrap(~year)+
labs(x="Countries",y="Rank for Citations",
     title="Top 3 countries having high rank for Citations")

#Top 3 countries having high ranking for Citations are: USA>UK. 
#In top 20 rankings, only USA is there.

# 12. Top 3 countries having high ranking for Patents

data_cwur %>%
  top_n(-20,patents) %>%
  ggplot(aes(x=country,y=patents,fill=country))+
  geom_bar(stat="identity")+facet_wrap(~year)+
labs(x="Countries",y="Rank for Patents",
     title="Top 3 countries having high rank for Patents")

#Top 3 countries having high ranking for Patents are: USA>South Korea>Japan. 

#13. Top 5 Indian universities year wise according to world rank
library(dplyr)
data_cwur %>%
  filter(country=="India") %>%
  group_by(year)%>%
  select(year,world_rank,institution) %>%
  top_n(-5,world_rank)

library(dplyr)
data_cwur %>%
  filter(country=="India") %>%
  group_by(year)%>%
  select(year,world_rank,institution) %>%
  top_n(-5,world_rank)%>%
  ggplot(aes(x=world_rank,y=institution,fill=institution))+
  geom_bar(stat="identity")+
  facet_grid(.~year)+
  labs(x="World Rank",y="Instituion",title=paste("Top 5 institutions of India "))

#We see that top 5 countries world rank wise in 2015 were: IIT Delhi,Delhi University, 
#IISC Bangalore,Panjab University and IIT Madras.

#14. Score of top 5 Indian universities

data_cwur %>%
  filter(country=="India") %>%
  group_by(year)%>%
  select(year,world_rank,institution,score) %>%
  top_n(-5,world_rank)

#We see that score of Indian university is between 45-46.

#15. Compare scores of top 5 World rank institutes and top 5 Indian universities in 2015

a<-data_cwur %>%
  filter(year==2015) %>%
  select(year,world_rank,institution,score) %>%
  top_n(-5,world_rank)

b<-data_cwur %>%
  filter(country=="India",year==2015) %>%
  select(world_rank,institution,score) %>%
  top_n(-5,world_rank)

cbind(a,b)

#16. Check world rank, national rank, rank for quality of education,rank for quality of faculty, rank for alumni 
#employment,rank for influence, rank for publications, rank for citations, rank for patents and score of top 5 Indian 
#universities in 2015

data_cwur %>%
  filter(country=="India",year==2015) %>%
  select(world_rank,institution,national_rank,quality_of_education,alumni_employment,quality_of_faculty,
         publications,influence,citations,patents,score) %>%
  top_n(-5,world_rank)

#compare top 5 Indian universities in world rankings for the year 2014 and 2015

data_cwur %>%
  group_by(year) %>%
  filter(country=="India") %>%
  select(year,world_rank,institution) %>%
  top_n(-5,world_rank) %>%
  ggplot(aes(x=year,y=world_rank,group=institution))+
  geom_line(aes(color=institution))+
  labs(x="Year",y="World Rank",title="World Rank of Indian universities for 2012-2015")+
  geom_label(aes(label=institution))

#We can see rank of Indian universities has improved from 2014 to 2015 except from IIT Delhi
