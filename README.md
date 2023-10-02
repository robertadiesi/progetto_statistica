# progetto_statistica

###ABOUT THE DATA SET: deforestation area fo Brazilian rainforest (km²) by year and state, from 2004 to 2019
#data set downloaded from https://www.kaggle.com/datasets/mbogernetto/brazilian-amazon-rainforest-degradation
#the data set shows data from the following 9 Brazilian states: Acre, Amazonas, Amapa, Maranhao, Mato Grosso, Para, Rondonia, Roraima and Tocantins
#observations were gathered along a time span of 16 years: from 2004 to 2019
#columns show how much area (km^2) of rainforest was lost for a state through the years
#however, the last column considers the total sum of deforested area in Brazil throughtout the years 


#loading the packages needed
library(tidyverse)
library(GGally)

#loading our data
dat<-read_csv("C:/Users/Rober/Desktop/progetto_R_stats/data/def_area_2004_2019.csv")

###DATA SET INSPECTION
dim(dat)
#the data set presents 16 rows (observations) and 11 columns (variables)
names(dat)
#the default column names are acronyms and need to be changed for a better understanding
glimpse(dat)
#all columns show numeric values (double): the I column refers to the years in which the observations were collected
#there seems to be no anomalies in data types: type coercion hasn't occurred
view(dat)
#by viewing the complete table in a separate tab we can notice there are no missing values (0)

###RENAMING COLUMNS AND FIXING DATA TYPES
dat <- dat %>%
  rename(year = "Ano/Estados",
         acre = `AC`,
         amazonas = `AM`,
         amapa = `AP`,
         maranhao = `MA`,
         matogrosso = `MT`,
         para = `PA`,
         rondonia = `RO`,
         roraima = `RR`,
         tocantins = `TO`,
         def_tot = `AMZ LEGAL`)

glimpse(dat) #to cross check

#we then change data types from double to integer 
dat <- dat %>%
  mutate(year = as.integer(year),
         acre = as.integer(acre),
         amazonas = as.integer(amazonas),
         amapa = as.integer(amapa),
         maranhao = as.integer(maranhao),
         matogrosso = as.integer(matogrosso),
         para = as.integer(para),
         rondonia = as.integer(rondonia),
         roraima = as.integer(roraima),
         tocantins = as.integer(tocantins),
         def_tot = as.integer(def_tot))

glimpse(dat) #to cross check

## furthermore, we found it more convenient for data analysis to do some adjustments
#we could split the data set into two smaller data sets, referring to different time periods
dat_2004_11 <- dat[-9:-16,]
dat_2012_19 <- dat[-1:-8,]
view(dat_2004_11)
view(dat_2012_19)

#then we could also eliminate the "year" column to create an alternative data set (years are not an appropriate quantitative variable)
dat_no_year <- dat[,-1]
glimpse(dat_no_year)
#now that we're done with data cleaning, we can proceed with data analysis


###DATA ANALYSIS

##Summary statistics
summary(dat_no_year)
#we use the function summary() to display summary statistics for every column (except for the "year" column)
#these statistics show for every Brazilian state the min and max values of deforested area, the mean and median area and the I and III quartile of the distribution
#we immediately notice that the Amapa and the Tocantins states show the lowest max values among all Brazilian states
#while the highest max value appears in the "matogross_def_area" column

##Data visualization

dat_2004_11%>%
  ggplot(aes(x=year,y=acre))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Acre")

dat_2012_19%>%
  ggplot(aes(x=year,y=acre))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Acre")
#for the I variable "acre_def_area", which refers to the deforestation in the Acre state, we opted for a line graph where the single observations are visible for each year
#we plotted the line graph for the 2004-2011 time period and the 2012-2019 time period
#as we can notice, the plots display different patterns
#we now want to export them in the working directory
p1 <- dat_2004_11%>%
  ggplot(aes(x=year,y=acre))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Acre")

ggsave(filename="acre_deforestation_2004_11.jpg", plot=p1, width=12, height=8)

p2 <- dat_2012_19%>%
  ggplot(aes(x=year,y=acre))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Acre")

ggsave(filename="acre_deforestation_2012_19.jpg", plot=p2, width=12, height=8)
#we then proceed to do it for every variable

p3 <- dat_2004_11%>%
  ggplot(aes(x=year,y=amazonas))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Amazonas")

ggsave(filename="amazonas_deforestation_2004_11.jpg", plot=p3, width=12, height=8)

p4 <- dat_2012_19%>%
  ggplot(aes(x=year,y=amazonas))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Amazonas")

ggsave(filename="amazonas_deforestation_2012_19.jpg", plot=p4, width=12, height=8)

p5 <- dat_2004_11%>%
  ggplot(aes(x=year,y=amapa))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Amapa")

ggsave(filename="amapa_deforestation_2004_11.jpg", plot=p5, width=12, height=8)

p6 <- dat_2012_19%>%
  ggplot(aes(x=year,y=amapa))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Amapa")

ggsave(filename="amapa_deforestation_2012_19.jpg", plot=p6, width=12, height=8)

p7 <- dat_2004_11%>%
  ggplot(aes(x=year,y=maranhao))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Marnhao")

ggsave(filename="maranhao_deforestation_2004_11.jpg", plot=p7, width=12, height=8)

p8 <- dat_2012_19%>%
  ggplot(aes(x=year,y=maranhao))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Maranhao")

ggsave(filename="maranhao_deforestation_2012_19.jpg", plot=p8, width=12, height=8)

p9 <- dat_2004_11%>%
  ggplot(aes(x=year,y=matogrosso))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Mato Grosso")

ggsave(filename="matogrosso_deforestation_2004_11.jpg", plot=p9, width=12, height=8)

p10 <- dat_2012_19%>%
  ggplot(aes(x=year,y=matogrosso))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Mato Grosso")

ggsave(filename="matogrosso_deforestation_2012_19.jpg", plot=p10, width=12, height=8)

p11 <- dat_2004_11%>%
  ggplot(aes(x=year,y=para))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Para")

ggsave(filename="para_deforestation_2004_11.jpg", plot=p11, width=12, height=8)

p12 <- dat_2012_19%>%
  ggplot(aes(x=year,y=para))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Para")

ggsave(filename="para_deforestation_2012_19.jpg", plot=p12, width=12, height=8)

p13 <- dat_2004_11%>%
  ggplot(aes(x=year,y=rondonia))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Rondonia")

ggsave(filename="rondonia_deforestation_2004_11.jpg", plot=p13, width=12, height=8)

p14 <- dat_2012_19%>%
  ggplot(aes(x=year,y=rondonia))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Rondonia")

ggsave(filename="rondonia_deforestation_2012_19.jpg", plot=p14, width=12, height=8)

p15 <- dat_2004_11%>%
  ggplot(aes(x=year,y=roraima))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Roraima")

ggsave(filename="roraima_deforestation_2004_11.jpg", plot=p15, width=12, height=8)

p16 <- dat_2012_19%>%
  ggplot(aes(x=year,y=roraima))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Roraima")

ggsave(filename="roraima_deforestation_2012_19.jpg", plot=p16, width=12, height=8)

p17 <- dat_2004_11%>%
  ggplot(aes(x=year,y=tocantins))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Tocantins")

ggsave(filename="tocantins_deforestation_2004_11.jpg", plot=p17, width=12, height=8)

p18 <- dat_2012_19%>%
  ggplot(aes(x=year,y=tocantins))+
  geom_line()+
  geom_point()+
  labs(x="Years",y="Deforestation in Tocantins")

ggsave(filename="tocantins_deforestation_2012_19.jpg", plot=p18, width=12, height=8)

#all the plot were automatically saved in the working directory but we collected them in a folder called "outputs"

#by observing the line graph for each state in each time period, we can say that some of them show particular trends between the observations, while others don't: observations in these cases appear quite scattered
#another aspect we can point out is that, if we confront each state in its two time periods, quite everyone shows a different pattern just like the Acre state did
#to better understand the differences between the two time periods for each state, we can calculate the summary statistics for every variable (excluding the years)
#we should only consider deforestation in the different states and not consider the total deforestation
dat_2004_11_noyear <- dat_2004_11[,-1]
dat_2004_11_states <- dat_2004_11_noyear[,-10]
glimpse(dat_2004_11_states)

dat_2012_19_noyear <- dat_2012_19[,-1]
dat_2012_19_states <- dat_2012_19_noyear[,-10]
glimpse(dat_2012_19_states)

summary(dat_2004_11_states)
summary(dat_2012_19_states)


#however, we cannot run a t test for every single region since we have few observations for each of them (it would be 8 observations for each region for each time period)
#therefore, we must confront the 9x8 observations of the first time period with the 9x8 observations of the second time period
#we create then two vectors of 72 numeric elements each and we run the t test with those
a1<-dat_2004_11_states$acre
a2<-dat_2004_11_states$amazonas
a3<-dat_2004_11_states$amapa
a4<-dat_2004_11_states$maranhao
a5<-dat_2004_11_states$matogrosso
a6<-dat_2004_11_states$para
a7<-dat_2004_11_states$rondonia
a8<-dat_2004_11_states$roraima
a9<-dat_2004_11_states$tocantins
df04_11<-data.frame(a1,a2,a3,a4,a5,a6,a7,a8,a9)
#we created a matrix to then create a numeric vector of the 72 deforestation observations for the first time period

boxplot(a1,a2,a3,a4,a5,a6,a7,a8,a9, main = "Deforestation in 2004-2011",
        names = c("Acre","Amazonas","Amapa","Maranhao","Matogrosso","Para","Rondonia","Roraima","Tocantins"))
#since the figure margins are too large, to visualize the graph we exported it as "boxplot_2004_2011.jpeg" in the working directory

b1<-dat_2012_19_states$acre
b2<-dat_2012_19_states$amazonas
b3<-dat_2012_19_states$amapa
b4<-dat_2012_19_states$maranhao
b5<-dat_2012_19_states$matogrosso
b6<-dat_2012_19_states$para
b7<-dat_2012_19_states$rondonia
b8<-dat_2012_19_states$roraima
b9<-dat_2012_19_states$tocantins
df12_19<-data.frame(b1,b2,b3,b4,b5,b6,b7,b8,b9)
#we created a matrix to then create a numeric vector of the 72 deforestation observations for the second time period

boxplot(b1,b2,b3,b4,b5,b6,b7,b8,b9, main = "Deforestation in 2012-2019",
        names = c("Acre","Amazonas","Amapa","Maranhao","Matogrosso","Para","Rondonia","Roraima","Tocantins"))
#since the figure margins are too large, to visualize the graph we exported it as "boxplot_2012_2019.jpeg" in the working directory

v1<-as.vector(t(df04_11)) #this is the vector with the 72 elements for the first time period
v2<-as.vector(t(df12_19)) #this is the vector with the 72 elements for the second time period
#we are now going to use these two vectors to run the t-test

## Two sample t-test

#this kind of t test examines if there is a significant difference between the means of two samples
#H0: means of the two samples are equal
#H1: true difference in means is not equal to 0
#when the p-value > 0.05, we accept the null Hp (H0)

t.test(v1,v2)
# pvalue = 0.01197 < 0.05
# mean of v1 = 1479.4, mean of v2 = 747.5
#we don't accept the null hypothesis (H0)
#the greater the difference between the two means, the greater the probability of the two samples being statistically different
