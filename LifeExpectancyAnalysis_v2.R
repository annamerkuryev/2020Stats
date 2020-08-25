######Life expectancy analysis in the year 2015########
library("mice")
library(ggplot2)
library("ggpubr")

#Load data
Life <- read.csv("LifeExpectancyData.csv", header = TRUE, check.names = FALSE, stringsAsFactors = FALSE, na.strings = "NA")
#Limiting the data to year 2015
Life_2015<-subset(Life,Year ==2015)
#Importing geographic annotation of countries
Country_metadata<-read.csv("Country_Continent.csv", header = TRUE, na.strings = "",check.names = FALSE)
#Merge data with metadata
Life_2015<-merge(Life_2015, Country_metadata,by.x = "Country", by.y = "Country")

#The data contains randomly distributed NA's in the majority of rows (2/183 rows are complete)
sum(complete.cases(Life_2015))
#This limits the ability to model data using linear models given R's 
#inherent setting of doing casewise deletions for row's containing NA's

#Data imputation for NA's while preserving dataset structure is a 
#common method to avoid this behaviour. Using the sample mean (as we do here) is common.
Life_2015_mean <- complete(mice(data=Life_2015,m=1,method="mean"))

#Linear  model exploring the effects of all the variables on all countries
fit_all <- lm(`Life_expectancy` ~ `Adult_mortality` + `Infant_deaths`+ Alcohol+ `Percent_expenditure`+ `Hepatitis_B`+
                Measles+ BMI + `Under_five_deaths_` + Polio  + Diphtheria + `HIV_AIDS`+GDP+
                +Population+`Thinness_10_19_years`+`Thinness_5_9_years`+`Income_composition_of_resources`+Schooling, data=Life_2015_mean)
summary(fit_all)
#Significant: 'Adult_mortality', 'Infant_deaths', 'Under_five_deaths_','Income_composition_of_resources'

#plotting significant variables
#Adult_mortality
plot1<-ggplot(Life_2015_mean, aes(`Adult_mortality`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('All continents: Adult mortality vs. Life expectancy')
#Infant_deaths
plot2<-ggplot(Life_2015_mean, aes(`Infant_deaths`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('All continents: Infant deaths vs. Life expectancy')
#Under_five_deaths_
plot3<-ggplot(Life_2015_mean, aes(`Under_five_deaths_`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('All continents: Under-5 deaths vs. Life expectancy')
#Income_composition_of_resources
plot4<-ggplot(Life_2015_mean, aes(`Income_composition_of_resources`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('All continents: Income composition vs. Life expectancy')

#Create multipanel plot
ggarrange(plot1, plot2, plot3, plot4, 
          labels = c("A", "B", "C","D"),common.legend = TRUE,
          ncol = 2, nrow = 2)

###############################Linear models exploring the effects of variables per continent###########################
##########Africa#############
fit_Africa <- lm(`Life_expectancy` ~ `Adult_mortality` + `Infant_deaths`+ Alcohol+ `Percent_expenditure`+ `Hepatitis_B`+
                   Measles+ BMI + `Under_five_deaths_` + Polio  + Diphtheria + `HIV_AIDS`+GDP+
                   +Population+`Thinness_10_19_years`+`Thinness_5_9_years`+`Income_composition_of_resources`+Schooling, 
                 data=Life_2015_mean[which(Life_2015_mean$Continent=="Africa"),])
summary(fit_Africa)
#Significant: 'Adult_mortality','BMI',  'Population'
#Trends for 'Hepatitis_B' and 'Diptheria'
#Plotting significant variables
#Adult_mortality
plot1<-ggplot(Life_2015_mean[which(Life_2015_mean$Continent=="Africa"),], aes(`Adult_mortality`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('Africa: Adult mortality vs. Life expectancy')
#BMI
plot2<-ggplot(Life_2015_mean[which(Life_2015_mean$Continent=="Africa"),], aes(BMI,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('Africa: BMI vs. Life expectancy')
#Population
plot3<-ggplot(Life_2015_mean[which(Life_2015_mean$Continent=="Africa"),], aes(Population,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('Africa: Population vs. Life expectancy')
#Create multipanel plot
ggarrange(plot1, plot2, plot3, 
          labels = c("A", "B", "C"),common.legend = TRUE,
          ncol = 2, nrow = 2)

#######Asia#########
fit_Asia <- lm(`Life_expectancy` ~ `Adult_mortality` + `Infant_deaths`+ Alcohol+ `Percent_expenditure`+ `Hepatitis_B`+
                 Measles+ BMI + `Under_five_deaths_` + Polio  + Diphtheria + `HIV_AIDS`+GDP+
                 +Population+`Thinness_10_19_years`+`Thinness_5_9_years`+`Income_composition_of_resources`+Schooling,data=Life_2015_mean[which(Life_2015_mean$Continent=="Asia"),])
summary(fit_Asia)
#Significant: 'Adult mortality', 'BMI', `Thinness_10_19_years`, `Thinness_5_9_years`
#Trend for 'Schooling'

#Plotting singificant variables
#Adult_mortality
plot1<-ggplot(Life_2015_mean[which(Life_2015_mean$Continent=="Asia"),], aes(`Adult_mortality`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('Asia: Adult mortality vs. Life expectancy')
#BMI
plot2<-ggplot(Life_2015_mean[which(Life_2015_mean$Continent=="Asia"),], aes(BMI,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('Asia: BMI vs. Life expectancy')
#Thinness_10-19
plot3<-ggplot(Life_2015_mean[which(Life_2015_mean$Continent=="Asia"),], aes(`Thinness_10_19_years`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('Asia: Thinness 10-19y vs. Life expectancy')
#Thinness 5-9 years
plot4<-ggplot(Life_2015_mean[which(Life_2015_mean$Continent=="Asia"),], aes(`Thinness_5_9_years`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('Asia: Thinness 5-9y vs. Life expectancy')
#Create multipanel plot
ggarrange(plot1, plot2, plot3, plot4,
          labels = c("A", "B", "C", "D"),common.legend = TRUE,
          ncol = 2, nrow = 2)

######Europe######
fit_Europe <- lm(`Life_expectancy` ~ `Adult_mortality` + `Infant_deaths`+ Alcohol+ `Percent_expenditure`+ `Hepatitis_B`+
                   Measles+ BMI + `Under_five_deaths_` + Polio  + Diphtheria + `HIV_AIDS`+GDP+
                   +Population+`Thinness_10_19_years`+`Thinness_5_9_years`+`Income_composition_of_resources`+Schooling,data=Life_2015_mean[which(Life_2015_mean$Continent=="Europe"),])
summary(fit_Europe)
#Significant: 'BMI'

#Plotting significant variable
#BMI
ggplot(Life_2015_mean[which(Life_2015_mean$Continent=="Europe"),], aes(`BMI`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('Europe: BMI vs. Life expectancy')
#Plot does not support a strong impact of BMI


#####North America#####
fit_NorthAmerica <- lm(`Life_expectancy` ~ `Adult_mortality` + `Infant_deaths`+ Alcohol+ `Percent_expenditure`+ `Hepatitis_B`+
                         Measles+ BMI + `Under_five_deaths_` + Polio  + Diphtheria + `HIV_AIDS`+GDP+
                         +Population+`Thinness_10_19_years`+`Thinness_5_9_years`+`Income_composition_of_resources`+Schooling,data=Life_2015_mean[which(Life_2015_mean$Continent=="North America"),])
summary(fit_NorthAmerica)
#Significant: Income_composition_of_resources
#Trend: GDP

#Plotting significant variable
#Income_composition_of_resources
ggplot(Life_2015_mean[which(Life_2015_mean$Continent=="North America"),], aes(`Income_composition_of_resources`,`Life_expectancy`))+
  geom_point(aes(col=Status))+theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),legend.position="top")+
  geom_smooth(method='lm', formula= y~x, col="black")+
  ggtitle('North America: Income composition vs. Life expectancy')


########South America##########
fit_SouthAmerica <- lm(`Life_expectancy` ~ `Adult_mortality` + `Infant_deaths`+ Alcohol+ `Percent_expenditure`+ `Hepatitis_B`+
                         Measles+ BMI + `Under_five_deaths_` + Polio  + Diphtheria + `HIV_AIDS`+GDP+
                         +Population+`Thinness_10_19_years`+`Thinness_5_9_years`+`Income_composition_of_resources`+Schooling,data=Life_2015_mean[which(Life_2015_mean$Continent=="South America"),])
summary(fit_SouthAmerica)
#ALL 12 residuals are 0: no residual degrees of freedom!


#Oceania (not enough data for good LM)
fit_Oceania <- lm(`Life_expectancy` ~ `Adult_mortality` + `Infant_deaths`+ Alcohol+ `Percent_expenditure`+ `Hepatitis_B`+
                    Measles+ BMI + `Under_five_deaths_` + Polio  + Diphtheria + `HIV_AIDS`+GDP+
                    +Population+`Thinness_10_19_years`+`Thinness_5_9_years`+`Income_composition_of_resources`+Schooling,data=Life_2015_mean[which(Life_2015_mean$Continent=="Oceania"),])
summary(fit_Oceania)
#ALL 10 residuals are 0: no residual degrees of freedom!
