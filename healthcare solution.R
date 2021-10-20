
getwd()
setwd("C:/Users/1998a/OneDrive/Desktop/R assignment 1")

#Basic: healthcaredata <- read.csv("healthcare_stroke_dataset.csv") 

#Recognizing "-"values as NA so we could eliminate them when required.
healthcaredata <- read.csv("healthcare_stroke_dataset.csv",na.strings =c("-"))

# Task 1: Statistical Exploratory Data Analysis

#1a. Number of rows, Number of columns and Name of Columns

#Number of rows
nrow(healthcaredata)

#Number of columns
ncol(healthcaredata)

#Name of columns
names(healthcaredata)

#structure of data frame
str(healthcaredata)

#Types of data in columns

#1. Data type in id
typeof(healthcaredata$id)
#2. Data type in date
typeof(healthcaredata$date)
#3. Data type in gender
typeof(healthcaredata$gender)
#4. Data type in age
typeof(healthcaredata$age)
#5. Data type in hypertension
typeof(healthcaredata$hypertension)
#6. Data type in heart_disease
typeof(healthcaredata$heart_disease)
#7. Data type in ever_married
typeof(healthcaredata$ever_married)
#8. Data type in work_type
typeof(healthcaredata$work_type)
#9. Data type in Residence_type
typeof(healthcaredata$Residence_type)
#10. Data type in avg_glucose_level
typeof(healthcaredata$avg_glucose_level)
#11. Data type in bmi
typeof(healthcaredata$bmi)
#12. Data type in smoking_status
typeof(healthcaredata$smoking_status)
#13. Data type in stroke
typeof(healthcaredata$stroke)

#summary
summary(healthcaredata)

#1b. Number of rows and Number of columns
#Number of rows
nrow(healthcaredata)
#Number of columns
ncol(healthcaredata)

#1c. Descriptive Details (count, unique, top, freq etc)
#for "bmi" column in healthcaredata dataframe

table <- aggregate(data.frame(count=healthcaredata$bmi),
          list(BMI=healthcaredata$bmi),
          length)

#All unique values with frequencies
table

#count
sum(table$count)

#unique values
unique(table$BMI)

#maximum value in bmi column
max(table$BMI)

#minimum value in bmi column
min(table$BMI)

#top five values 
head(table,5)

#1d. All unique values in "age" column
unique(healthcaredata$age)

#Converting the type of variable viz. gender, ever_married, work_type,
#Residence_type, smoking_status, hypertension, heart_disease and stroke 
#from charachter or numeric to Categorical variables for future use.

healthcaredata$gender <- factor(healthcaredata$gender)
healthcaredata$ever_married <-factor(healthcaredata$ever_married)
healthcaredata$work_type <- factor(healthcaredata$work_type)
healthcaredata$Residence_type<- factor(healthcaredata$Residence_type)
healthcaredata$smoking_status <- factor(healthcaredata$smoking_status)
healthcaredata$hypertension <- factor(healthcaredata$hypertension)
healthcaredata$heart_disease <- factor(healthcaredata$heart_disease)
healthcaredata$stroke <- factor(healthcaredata$stroke)


# Task 2: Aggregation and Filtering and Rank

#2a. Quantity with maximum records 
#Aggregate table
aggregate_table <- aggregate(data.frame(count=healthcaredata$work_type),
                             list(WorkTypes=healthcaredata$work_type),
                             length)
aggregate_table
#Quantity with maximum records
max(aggregate_table$count)

#2b. Total number of males who never smoked

#Filtering dataframe for required data of males who never smoked
nonsmoking_males <- healthcaredata[healthcaredata$gender=="Male" & healthcaredata$smoking_status=="never smoked",]

#Number of males who never smoked
nrow(nonsmoking_males)  

#2c. Top 10 individuals with highest avg glucose levels

#Arranging data in decreasing order
dec_order <- healthcaredata[order(healthcaredata$avg_glucose_level,decreasing = T),]

#Information of top 10 individuals with highest average glucose levels
#in decreasing order
head(dec_order,10) 


#Task3. Visualization

#3a. Plot of work_type for smoking_status
library(ggplot2)
library(tidyverse)

#without using facets
healthcaredata %>%
  filter(smoking_status %in% c("formerly smoked","never smoked","smokes")) %>%
  drop_na(work_type) %>%
  ggplot(aes(smoking_status, fill = work_type))+
  geom_bar(position = "dodge",alpha=0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title = "Smoking status and Worktype",
       x="Smoking Status",
       y="Count")


  
#3b. Pie chart for work_types of individuals
Worktype_count <- table(healthcaredata$work_type)
Worktype_Percent <-table(healthcaredata$work_type)/5110

pie(Worktype_Percent,main="Work types of individuals",
    radius=1,col=c("red","blue","green","yellow","darkgreen"))



#4a.Insights

#Barchart of work types for genders
healthcaredata %>%
  filter(gender %in% c("Male","Female")) %>%
  drop_na(work_type) %>%
  ggplot(aes(work_type))+
  geom_bar(aes(fill =work_type),size=10)+
  facet_grid(.~gender)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")+
  labs(title = "Gender and Worktype",
       x="Gender",
       y="Count")


#Boxplot of BMI for different work types  
healthcaredata %>%
  drop_na(work_type)%>%
  drop_na(bmi)%>%
  ggplot(aes(work_type,bmi,colour = work_type))+
  geom_boxplot(size = 1.2)+
  theme_bw()+
  labs(title = "BMI for Work types")


