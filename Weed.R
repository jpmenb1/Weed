library(tidyverse)
library(data.table)
library(corrplot)
library(NCmisc)
library(readxl)
library(devtools)




#importing sheet1 and cleaning data
sheet1 <- read_excel("C:/Users/josep/Desktop/Srishdata/PotPlants_18.xlsx", sheet = 'SampleSetOne')
#changing "Sample.Name" to "Sample Name"
sheet1 <- (setnames(sheet1, old = c("Sample,Name"), new =c("Sample Name")))


#importing sheet2 and cleaning data
sheet2 <- read_excel("C:/Users/josep/Desktop/Srishdata/PotPlants_18.xlsx", sheet = 'SampleSetTwo')

#replacing potting mix with pm
sheet2 <- sheet2 %>% mutate(Group = recode(Group, 'potting mix' = 'pm'))
#removing NA's
sheet2<- sheet2%>%na.omit
#removing mean and variance values at the bottom
sheet2 <- (sheet2[-c(53,54),])
#changing "SampleName" to "Sample Name"
sheet2 <- (setnames(sheet2, old = c("SampleName"), new =c("Sample Name")))

#importing sheet3 and cleaning data
sheet3 <- read_excel("C:/Users/josep/Desktop/Srishdata/PotPlants_18.xlsx", sheet = 'SampleSetThree')
#remove "missing" values
sheet3 <- (sheet3[-c(49),])
#converting the elements from factor to numbers.
sheet3 <- mutate_at(sheet3, vars(-Sample.Name, -Group), function(x) as.numeric(as.character(x)))
#changing "Sample.Name" to "Sample Name"
sheet3 <- (setnames(sheet3, old = c("Sample.Name"), new =c("Sample Name")))

#combining all the sheets into one data set
sheet <- rbind(sheet1, sheet2, sheet3)


#ANALYSIS

#Creating a function "test" to filter the data set and show only the elements I want to analize for question 1.
test <-  sheet [c("Group", "Mg", "Al", "K", "Ca", "Sc")]

#"newData" is a new function arranges the data in "test"
newData <- test %>% gather(key = Element, value = Measure, Mg, Al, K, Ca, Sc)

#"newnewdata" give gives me the mean and standard deviations of the elements above
newnewdata <- newData %>% group_by(Element, Group) %>% summarise(mean = mean(Measure), sd = sd(Measure))

#graphs separated by soil type and with different colours for each element
ggplot(data = newData) + geom_point(aes(x = Element, y = Measure, colour = Element )) + facet_grid(~ Group)
ggplot(data = newData) + geom_boxplot(aes(x = Element, y = Measure, colour = Element )) + facet_grid(~ Group)
View(newnewdata)

#Question 2
#filtering the data to be shown only the columns I want
test2 <-  sheet [c("Sample Name", "Group", "Mg", "Al", "K", "Ca", "Sc")]

newData2 <- test2 %>% gather(key = Element, value = Measure, Mg, Al, K, Ca, Sc)

newData2 %>% group_by(Element)

#graphs to compare the measures of a pair of elements
#Ba and Zn
baeu <- ggplot(data = sheet, aes(x= Ba, y= Eu)) + geom_point(data = sheet, aes(x = Ba, y = Eu, colour = Group)) + geom_smooth(method = lm) + scale_y_continuous(limits = c(0,0.25))

#Ba and Sr
znsr <- ggplot(data = sheet, aes(x= Zn, y= Sr)) + geom_point(data = sheet, aes(x = Zn, y = Sr, colour = Group)) + geom_smooth(method = lm)

#Mo and Ga
moga <- ggplot(data = sheet, aes(x= Mo, y= Ga)) + geom_point(data = sheet, aes(x = Mo, y = Ga, colour = Group)) + geom_smooth(method = lm)

#Mo and Ba
moba <- ggplot(data = sheet, aes(x= Mo, y= Ba)) + geom_point(data = sheet, aes(x = Mo, y =Ba, colour = Group)) + geom_smooth(method = lm)

#Mo and Ca
moca <- ggplot(data = sheet, aes(x= Mo, y = Ca)) + geom_point(data = sheet, aes(x = Mo, y = Ca, colour = Group)) + geom_smooth(method = lm)

#correlation between the elements
corr1 <- cor(sheet[3:40])
corrplot(corr1, method = "square", type = "lower")

#How many elements have a correlation smaller than -0.5 or greater than +0.5? Divided by 2 to eliminate
#duplicates and subtracted 38 to remove correlation between the same elements (for example Mn and Mn)
sum(corr1 > 0.5 | corr1 < -0.5)/2-38

#summary
#sheet1
#th
summary(sheet1$Th)
sd(sheet1$Th)

#Al
summary(sheet1$Al)
sd(sheet1$Al)

#K
summary(sheet1$K)
sd(sheet1$K)

#Ca
summary(sheet1$Ca)
sd(sheet1$Ca)

#Sc
summary(sheet1$Sc)
sd(sheet1$Sc)

#table containing min, 1st qt, median, mean, 3rd qt and max of elements above in sheet 1
tab1 <- matrix(c(min(sheet1$Th), quantile(sheet1$Th, 0.25), median(sheet1$Th), mean(sheet1$Th), quantile(sheet1$Th, 0.75), max(sheet1$Th),
                 min(sheet1$Al), quantile(sheet1$Al, 0.25), median(sheet1$Al), mean(sheet1$Al), quantile(sheet1$Al, 0.75), max(sheet1$Al),
                 min(sheet1$K), quantile(sheet1$K, 0.25), median(sheet1$K), mean(sheet1$K), quantile(sheet1$K, 0.75), max(sheet1$K),
                 min(sheet1$Ca), quantile(sheet1$Ca, 0.25), median(sheet1$Ca), mean(sheet1$Ca), quantile(sheet1$Ca, 0.75), max(sheet1$Ca),
                 min(sheet1$Sc), quantile(sheet1$Sc, 0.25), median(sheet1$Sc), mean(sheet1$Sc), quantile(sheet1$Sc, 0.75), max(sheet1$Sc)), ncol=6, byrow=TRUE)
colnames(tab1) <- c('Min', '1st Qt', 'Median', 'Mean', '3rd Qt', 'Max')
rownames(tab1) <- c('Th', 'Al', 'K', 'Ca', 'Sc')
tab1 <- as.table(tab1)
round(tab1,2)
#sheet2
#Mg
summary(sheet2$Mg)
sd(sheet2$Mg)

#Al
summary(sheet2$Al)
sd(sheet2$Al)

#K
summary(sheet2$K)
sd(sheet2$K)

#Ca
summary(sheet2$Ca)
sd(sheet2$Ca)

#Sc
summary(sheet2$Sc)
sd(sheet2$Sc)

#table containing min, 1st qt, median, mean, 3rd qt and max of elements above in sheet 2
tab2 <- matrix(c(min(sheet2$Th), quantile(sheet2$Th, 0.25), median(sheet2$Th), mean(sheet2$Th), quantile(sheet2$Th, 0.75), max(sheet2$Th),
                 min(sheet2$Al), quantile(sheet2$Al, 0.25), median(sheet2$Al), mean(sheet2$Al), quantile(sheet2$Al, 0.75), max(sheet2$Al),
                 min(sheet2$K), quantile(sheet2$K, 0.25), median(sheet2$K), mean(sheet2$K), quantile(sheet2$K, 0.75), max(sheet2$K),
                 min(sheet2$Ca), quantile(sheet2$Ca, 0.25), median(sheet2$Ca), mean(sheet2$Ca), quantile(sheet2$Ca, 0.75), max(sheet2$Ca),
                 min(sheet2$Sc), quantile(sheet2$Sc, 0.25), median(sheet2$Sc), mean(sheet2$Sc), quantile(sheet2$Sc, 0.75), max(sheet2$Sc)), ncol=6, byrow=TRUE)
colnames(tab2) <- c('Min', '1st Qt', 'Median', 'Mean', '3rd Qt', 'Max')
rownames(tab2) <- c('Th', 'Al', 'K', 'Ca', 'Sc')
tab2 <- as.table(tab2)
round(tab2,2)

#sheet3
#Mg
summary(sheet3$Mg)
sd(sheet3$Mg)

#Al
summary(sheet3$Al)
sd(sheet3$Al)

#K
summary(sheet3$K)
sd(sheet3$K)

#Ca
summary(sheet3$Ca)
sd(sheet3$Ca)

#Sc
summary(sheet3$Sc)
sd(sheet3$Sc)

#table containing min, 1st qt, median, mean, 3rd qt and max of elements above in sheet 3
tab3 <- matrix(c(min(sheet3$Th), quantile(sheet3$Th, 0.25), median(sheet3$Th), mean(sheet3$Th), quantile(sheet3$Th, 0.75), max(sheet3$Th),
                 min(sheet3$Al), quantile(sheet3$Al, 0.25), median(sheet3$Al), mean(sheet3$Al), quantile(sheet3$Al, 0.75), max(sheet3$Al),
                 min(sheet3$K), quantile(sheet3$K, 0.25), median(sheet3$K), mean(sheet3$K), quantile(sheet3$K, 0.75), max(sheet3$K),
                 min(sheet3$Ca), quantile(sheet3$Ca, 0.25), median(sheet3$Ca), mean(sheet3$Ca), quantile(sheet3$Ca, 0.75), max(sheet3$Ca),
                 min(sheet3$Sc), quantile(sheet3$Sc, 0.25), median(sheet3$Sc), mean(sheet3$Sc), quantile(sheet3$Sc, 0.75), max(sheet3$Sc)), ncol=6, byrow=TRUE)
colnames(tab3) <- c('Min', '1st Qt', 'Median', 'Mean', '3rd Qt', 'Max')
rownames(tab3) <- c('Th', 'Al', 'K', 'Ca', 'Sc')
tab3 <- as.table(tab3)
round(tab3,2)

#document libraries used
list.functions.in.file("L:/MT5763_A1/MT5763_A1/MT5763_A1_Rcodenew.R")