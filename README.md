# R-programming

![](image/RmarkDown.png)


# R-programming
R programming for data analysis

#=========================================


# VERTOR - GENDER 


### reproducibility of random numbers . 
set.seed(1001)


#created gender variable.
gender <- sample(x=c("Male","Female"), 10000, replace = T, prob = c(0.35,0.65) )

# The first six roles of the newly created gender variable.
head(gender)

gender

# table showing the frequencies of the genders
prop.table(table(gender))


# number of observations in the gender variable
length(gender)


#=========================================

# VERTOR - RACE 



race <- c(rep("Africa", 0.1*10000), rep("S_America", 0.2*10000), rep("Europe", 0.35*10000),
          rep("Asia", 0.25*10000), rep("Australia", 0.1*10000))

race <- sample(race,10000)
head(race)

# number of obseravtions in the race variable.
length(race)


# table with the proportion of each unique levels in the race variable
prop.table(summary(as.factor(race)))


# Alternative to creating the race variable.
race <- rep(c("Africa","S_America","Europe","Asia","Australia"), 10000*c(0.1,0.2,0.35,0.25,0.1))
head(race)

length(race)

prop.table(summary(as.factor(race)))


#=========================================

# VERTOR #MATHEMATICS SCORE


mathScore <- rnorm(10000, mean = 55, sd = 10)
head(mathScore)


# rounding up to the nearest whole number
mathScore <- round(mathScore,0)
summary(mathScore)

#=========================================

# VERTOR  #CHEMISTRY SCORE

chemScore <- rnorm(10000, mean = 60, sd = 5)
chemScore <- round(chemScore,0)
summary(chemScore)

#=========================================

# VERTOR  #BIOLOGY SCORE

BioScore <- rnorm(10000, mean = 70, sd = 5)
BioScore <- round(BioScore,0)
summary(BioScore)

#=========================================
# VERTOR  #CLUB MEMBERSHIP

# NOT INCLUDED IN POWER POINT BUT WE RUN THE COMMAND.
# VECTOR  #CLUB MEMBERSHIP

n <- 10000
member <- character(n)
u <- runif(n)
member[u<=0.6] <- "Yes"
member[u>0.6] <- "No"
table(member)

prop.table(summary(as.factor(member)))

membership <- member
head(membership)

#=========================================
# VECTOR AGE 
Age = floor(runif(10000, min=18, max=23))
Age
table(Age)


#=========================================
# MATRIX class Performance


classPerformance <- cbind(gender, race, mathScore, chemScore, BioScore, membership, Age)
head(classPerformance)

# The cbind function is also called the column binding. 
# It binds each vectors by column into a resulting matrix object.

class(classPerformance)

summary(classPerformance)

#=========================================
# DATA FRAME class Performance


classPerformance <- as.data.frame(classPerformance)
classPerformance

# ALternative
classPerformance <- data.frame(gender, race, mathScore, chemScore, BioScore, membership, Age)
class(classPerformance)

head(classPerformance)
summary(classPerformance)

# NOTE: To write the data created out in csv extension in excel, 
# the code below is used; 
write.csv(classPerformance,"C:/Data/classPerformance.csv")



classPerformance 
tail(classPerformance)

#=========================================
# DATA MANIPULATION WITH THE DPLYR PACKAGE
# for Installing the dplyr package from CRAN

install.packages("dplyr") 
library(dplyr)

# =========================================
#Select including sub setting 
#SELECT AND SUBSET

class(classPerformance)

str(classPerformance)


# =========================================
# FACTOR: CATEGORICAL VARIABLE

classPerformance$gender <- as.factor(classPerformance$gender)
classPerformance$race <- as.factor(classPerformance$race) 
classPerformance$Age <- as.factor(classPerformance$Age) 
classPerformance$membership <- as.factor(classPerformance$membership) 
levels(classPerformance$Age)

select(classPerformance, mathScore, chemScore, BioScore)


scores <- select(classPerformance, mathScore, chemScore, BioScore)
head(scores)

# =========================================
# Students that scored above 50 in all the courses

above50 <- subset(classPerformance, mathScore>50 & chemScore>50 & BioScore>50)
select(above50, mathScore, chemScore, BioScore)


scores_above50 <- select(above50, mathScore, chemScore, BioScore)
min(scores_above50$mathScore)

min(scores_above50$chemScore)

min(scores_above50$BioScore)

max(scores_above50$mathScore)

max(scores_above50$chemScore)

max(scores_above50$BioScore)

#=========================================
# No student have 50 all through in all the courses

equal50 <- subset(classPerformance, mathScore==50 & chemScore==50 & BioScore==50)
equal50

#=========================================
# ARRANGE

arranged_cp <- arrange(classPerformance, mathScore, chemScore, BioScore)
head(arranged_cp)


# =========================================
# FILTER
# Filter out only Africans who score above 80 in maths 
# in order to give them a scholarship

dplyr::filter(classPerformance, mathScore>80, race=="Africa")

head(dplyr::filter(classPerformance, mathScore>80, race=="Africa"))

# Filter out female african. How many are they ?  

filter(classPerformance, race=="Africa", gender=="Female")

a <- filter(classPerformance, race=="Africa", gender=="Female")
head(filter(classPerformance, race=="Africa", gender=="Female"))

# How many are the female african ?  
nrow(filter(classPerformance, race=="Africa", gender=="Female"))

# =========================================
# PIPE OPERATOR %>%

# create a variable with information on the scores of the male Europeans within 
# age 18 who are not a member of any organization. Store the first 6 rows

df_EuroMale <- classPerformance %>%
  filter(race=="Europe", gender=="Male", Age== 18, membership=="No") %>%
  arrange(desc(mathScore)) %>%
  select(mathScore, chemScore, BioScore) %>%
  head()
df_EuroMale

#=========================================
# MUTATE
#Creating a new column using the existing columns.
#Find the average of all the scores

mutate(classPerformance, AvgScores = (mathScore+chemScore+BioScore)/3)

#=========================================
# RANK

classPerformance %>%
  group_by(gender,race) %>%
  summarise(total_cnt = n(), totalsc = sum(mathScore,chemScore,BioScore)) %>%
  arrange(gender, race, desc(total_cnt), desc(totalsc)) %>%
  mutate(rank = dense_rank(desc(total_cnt))) %>%
  arrange(rank) %>%
  head()

# =========================================
# DATA VISUALIZATION
# PIE CHART
# to create this chart we install ggplot2 package and ggthemes package
# Themes are also another important component to make a good R 
# visualization in R. To have access to more themes

install.packages(“ggplot2”)
library("ggplot2")
install.packages(“ggthemes”)



ggplot(classPerformance, aes(x = "", fill = factor(race))) +
  geom_bar(stat= "count", width = 1, color = "white") +
  geom_text(aes(label = scales::percent(..count.. / sum(..count..))),
            stat = "count", position = position_stack(vjust = .5)) +
  coord_polar("y", start = 0, direction = -1) + 
  scale_fill_manual(values = c(rgb(1,0,.5),rgb(.5,.5,1),rgb(.7,.2,.1),rgb(0,.2,.9),rgb(.7,.5,0
  ))) + ggtitle("Students percentage based on their race ") +
  theme()

# =========================================
# To automatically create a stacked bar.

pl <- ggplot(classPerformance, aes(x=race))
print(pl + geom_bar())

print(pl + geom_bar(color="blue")) ## for outline colour

print(pl + geom_bar(color="blue", fill="pink"))


equal50 <- subset(classPerformance, mathScore==50 & chemScore==50 & BioScore==50)


arranged_cp <- arrange(classPerformance, mathScore, chemScore, BioScore)
head(arranged_cp)
 

#=========================================
# PLOTS 

pl <- ggplot(classPerformance, aes(x=mathScore, y=BioScore))
pl + geom_point()

# changing the size of data points
pl + geom_point(size=1)

# overlapping points gets darker
pl + geom_point(alpha=0.5,size=5)

#=========================================
# HISTOGRAMS

pl <- ggplot(classPerformance, aes(x=mathScore))
pl + geom_histogram()

# Addditional arguments to the geometric component.
pl + geom_histogram(binwidth = 0.1)

pl + geom_histogram(binwidth = 0.1, color="red", fill='pink', alpha=0)

# The alpha is for setting transparency. The default is 1
#This shows the bar with the gridlines
pl + geom_histogram(binwidth = 0.1, color="red", fill='pink', alpha=0.4)


# The line of code is getting too long. This is why I have to store it in pl1
pl1 <- pl + geom_histogram(binwidth = 0.1, color="red", fill='pink', alpha=0.4)

pl2 <- pl1 + xlab('Math Score') + ylab('Count')
print(pl2)

 
print(pl + geom_bar(color="blue")) 
## for outline colour


pl <- ggplot(classPerformance, aes(x=Age, y=race))
pl + geom_point()

pl2 + ggtitle("My Graph")

 
# end 
# ==============================================================
