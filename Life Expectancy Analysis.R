Life <- read.csv("LifeExpectancyData.csv", header = TRUE, na.strings = "",check.names = FALSE)
Life_2015<-subset(Life,Year ==2015)
Country_metadata<-read.csv("Country_Continent.csv", header = TRUE, na.strings = "",check.names = FALSE)
Life_2015<-merge(Life_2015, Country_metadata,by.x = "Country", by.y = "Country")

#Linear mixed model exploring the effects of all the variables on all the countries together.
fit_all <- lm(as.numeric(`Life expectancy`) ~ as.numeric(`Adult mortality`) + as.numeric(`Infant deaths`)+ as.numeric(Alcohol)+as.numeric(`Percent expenditure`)+as.numeric(`Hepatitis B`)+
                   as.numeric(Measles)+as.numeric(BMI) +as.numeric(`Under-five deaths`)+as.numeric(Polio)+as.numeric(`Total expenditure`)+as.numeric(Diphtheria)+as.numeric(`HIV/AIDS`)+as.numeric(GDP)+
                   +as.numeric(Population)+as.numeric(`Thinness 10-19 years`)+as.numeric(`Thinness 5-9 years`)+as.numeric(`Income composition of resources`)+as.numeric(Schooling), data=Life_2015)
summary(fit_all)

#Linear mixed models exploring the effects of variables per continent.
#Africa
fit_Africa <- lm(as.numeric(`Life expectancy`) ~ as.numeric(`Adult mortality`) + as.numeric(`Infant deaths`)+ as.numeric(Alcohol)+as.numeric(`Percent expenditure`)+as.numeric(`Hepatitis B`)+
                   as.numeric(Measles)+as.numeric(BMI) +as.numeric(`Under-five deaths`)+as.numeric(Polio)+as.numeric(`Total expenditure`)+as.numeric(Diphtheria)+as.numeric(`HIV/AIDS`)+as.numeric(GDP)+
                   +as.numeric(Population)+as.numeric(`Thinness 10-19 years`)+as.numeric(`Thinness 5-9 years`)+as.numeric(`Income composition of resources`)+as.numeric(Schooling), data=Life_2015[which(Life_2015$Continent=="Africa"),])
summary(fit_Africa)

#Asia
fit_Asia <- lm(as.numeric(`Life expectancy`) ~ as.numeric(`Adult mortality`) + as.numeric(`Infant deaths`)+ as.numeric(Alcohol)+as.numeric(`Percent expenditure`)+as.numeric(`Hepatitis B`)+
                   as.numeric(Measles)+as.numeric(BMI) +as.numeric(`Under-five deaths`)+as.numeric(Polio)+as.numeric(`Total expenditure`)+as.numeric(Diphtheria)+as.numeric(`HIV/AIDS`)+as.numeric(GDP)+
                   +as.numeric(Population)+as.numeric(`Thinness 10-19 years`)+as.numeric(`Thinness 5-9 years`)+as.numeric(`Income composition of resources`)+as.numeric(Schooling), data=Life_2015[which(Life_2015$Continent=="Asia"),])
summary(fit_Asia)

#Europe
fit_Europe <- lm(as.numeric(`Life expectancy`) ~ as.numeric(`Adult mortality`) + as.numeric(`Infant deaths`)+ as.numeric(Alcohol)+as.numeric(`Percent expenditure`)+as.numeric(`Hepatitis B`)+
                   as.numeric(Measles)+as.numeric(BMI) +as.numeric(`Under-five deaths`)+as.numeric(Polio)+as.numeric(`Total expenditure`)+as.numeric(Diphtheria)+as.numeric(`HIV/AIDS`)+as.numeric(GDP)+
                   +as.numeric(Population)+as.numeric(`Thinness 10-19 years`)+as.numeric(`Thinness 5-9 years`)+as.numeric(`Income composition of resources`)+as.numeric(Schooling), data=Life_2015[which(Life_2015$Continent=="Europe"),])
summary(fit_Europe)

#North America
fit_NorthAmerica <- lm(as.numeric(`Life expectancy`) ~ as.numeric(`Adult mortality`) + as.numeric(`Infant deaths`)+ as.numeric(Alcohol)+as.numeric(`Percent expenditure`)+as.numeric(`Hepatitis B`)+
                 as.numeric(Measles)+as.numeric(BMI) +as.numeric(`Under-five deaths`)+as.numeric(Polio)+as.numeric(`Total expenditure`)+as.numeric(Diphtheria)+as.numeric(`HIV/AIDS`)+as.numeric(GDP)+
                 +as.numeric(Population)+as.numeric(`Thinness 10-19 years`)+as.numeric(`Thinness 5-9 years`)+as.numeric(`Income composition of resources`)+as.numeric(Schooling), data=Life_2015[which(Life_2015$Continent=="North America"),])
summary(fit_NorthAmerica)

#South America
fit_SouthAmerica <- lm(as.numeric(`Life expectancy`) ~ as.numeric(`Adult mortality`) + as.numeric(`Infant deaths`)+ as.numeric(Alcohol)+as.numeric(`Percent expenditure`)+as.numeric(`Hepatitis B`)+
                         as.numeric(Measles)+as.numeric(BMI) +as.numeric(`Under-five deaths`)+as.numeric(Polio)+as.numeric(`Total expenditure`)+as.numeric(Diphtheria)+as.numeric(`HIV/AIDS`)+as.numeric(GDP)+
                         +as.numeric(Population)+as.numeric(`Thinness 10-19 years`)+as.numeric(`Thinness 5-9 years`)+as.numeric(`Income composition of resources`)+as.numeric(Schooling), data=Life_2015[which(Life_2015$Continent=="South America"),])
summary(fit_SouthAmerica)

#Oceania (not enough data for good LM)
fit_Oceania <- lm(as.numeric(`Life expectancy`) ~ as.numeric(`Adult mortality`) + as.numeric(`Infant deaths`)+ as.numeric(Alcohol)+as.numeric(`Percent expenditure`)+as.numeric(`Hepatitis B`)+
                         as.numeric(Measles)+as.numeric(BMI) +as.numeric(`Under-five deaths`)+as.numeric(Polio)+as.numeric(`Total expenditure`)+as.numeric(Diphtheria)+as.numeric(`HIV/AIDS`)+as.numeric(GDP)+
                         +as.numeric(Population)+as.numeric(`Thinness 10-19 years`)+as.numeric(`Thinness 5-9 years`)+as.numeric(`Income composition of resources`)+as.numeric(Schooling), data=Life_2015[which(Life_2015$Continent=="Oceania"),])
summary(fit_Oceania)
