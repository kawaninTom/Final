# loading all necessary libraries for the project
library(tidyverse)
library(janitor)
library(knitr)
library(dplyr)
library(data.table)
library(gdata) # this library contains rename.vars
library(lmtest) # Breusch-Pagan Test
library(ggpubr)
library(ggthemes)

# Source of the CSV file
Source <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv' 

# Reading the CSV file
astronauts <- readr::read_csv(Source)

#give a more meaningful name to the column
astronauts<-rename.vars(astronauts,"field21","EVA",info=FALSE)

# Calculating the age of the astronauts at their selection
astronauts$AgeS <- astronauts$year_of_selection - astronauts$year_of_birth

# Calculating the age of the astronauts at their first mission
astronauts$AgeM <- astronauts$year_of_mission - astronauts$year_of_birth

# get the min and the max value to make the Agebreaks
summary(astronauts$AgeM)

# use histogram to have a basic understanding of the distribution of the AgeM
hist(astronauts$AgeM)

# create Agebreaks based on the summary statistics
Agebreaks <- c(19,29,39,49,59,79)

#labeling of the new categories
AgeLabels <- c("26-29","30-39","40-49","50-59","60-77")

# dividing the AgeM into subgroups by cut function
astronauts$AgeMG <- cut(astronauts$AgeM,Agebreaks,labels = AgeLabels)

# analyzing the distribution of the age according to gender
astronauts %>% 
  count(sex)
ggplot(astronauts,aes(AgeMG,fill=sex)) +
  geom_bar()

# get a basic understanding of occupation
astronauts %>% 
  count(occupation)
# most popular occupations: Commander,Flight engineer, pilot,PSP
# duplicated categories due to lower and uppercase letters (pilot,Pilot)
  astronauts$occupation <- tolower(astronauts$occupation)
# solve the issue mentioned-above


# creating the other category including 
#creating a vector 
categories<- c("other (journalist)","other (space tourist)","space tourist", "spaceflight participant")
#renaming them to other
astronauts$occupation[astronauts$occupation %in% categories] <- c("other")

# check the new categories
astronauts %>% 
count(occupation)

# now the occupations only have 6 different categories, so now we can plot them
ggplot(astronauts,aes(occupation,fill=sex)) +
  geom_bar()

# looking for further duplicated data
astronauts %>% 
count(military_civilian)

astronauts %>% 
count(nationality)

ggplot(astronauts,aes(occupation,fill=military_civilian)) +
  geom_bar()

# age and occupation
ggplot(astronauts,aes(occupation,AgeMG)) +
  geom_bar()


# dependent variable: hours mission
# independent variables: AGEM,Field21, gender, nationwide number, military_civilian

mean <- mean(astronauts$hours_mission)

Reg1<- lm(hours_mission ~ AgeM + EVA + occupation, data = astronauts)
summary(Reg1)
# Multiple R-squared:  0.2071,	Adjusted R-squared:  0.2059, based on the F-statistic the model is significantly different from the only intercept model
Reg2<- lm(hours_mission ~ AgeM + EVA + year_of_mission, data = astronauts)
summary(Reg2)

# Breusch-Pagan Test for heteroscedasticity 
bptest(Reg2)

# checking normality of the residuals
# only approximately 50-60% of data follows a normal distribution
ggqqplot(Reg2$residuals)
shapiro.test(Reg2$residuals)



