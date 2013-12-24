###### Setting working directory
getwd()
setwd('/Users/hawooksong/Desktop/programming_projects/incomeDataAnalysis/genderAnalysis')


###### Loading R packages
# update.packages()
# install.packages('psych')
# install.packages('ggplot2')
# install.packages('gridExtra')
# install.packages('arules')
library(psych)
library(ggplot2)
library(gridExtra)
library(lsr)
library(car)


###### Loading processed data (that contains no missing values)
load('./data/incomeData.RData')


###### Brief overview of training data
head(incomeData)
str(incomeData)


###### Subsetting male and female
male <- subset(incomeData, sex=='Male')
female <- subset(incomeData, sex=='Female')


###### Equal number of males and females who have never married
ggplot(incomeData, aes(x=marital_status, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

# selecing all male and female who have never married 
m.never_married <- subset(incomeData, sex=='Male' & marital_status=='Never-married')
f.never_married <- subset(incomeData, sex=='Female' & marital_status=='Never-married')
n.m.never_married <- nrow(m.never_married)
n.f.never_married <- nrow(f.never_married)
n.m.never_married
n.f.never_married

### Script omitted
# reducing the number of male individuals who have never married to set equal to females
# who have never married
# set.seed(1234)
# selectCond <- sample(n.m.never_married, n.f.never_married)
# m.never_married <- m.never_married[selectCond, ]
# n.m.never_married <- nrow(m.never_married)
# n.m.never_married == n.f.never_married

# combing data frames of male and female who have never married
mf.never_married <- rbind(m.never_married, f.never_married)



###### Is there an income gap between genders?
###### Which gender has higher percentage of people earning $50K/year or more?
ggplot(mf.never_married, aes(x=income, fill=sex)) + 
  geom_bar(position='dodge') +
  labs(title='Individuals who never married by income and gender')
dev.copy(png, 'images/individuals_who_never_married_by_income_and_gender.png')
dev.off()

#### creating income-gender table
mf.never_married.incomeGenderT <- table(mf.never_married$income, mf.never_married$sex)
mf.never_married.incomeGenderT

#### comparing percentages
mf.never_married.incomeGenderT[, 2][1] / sum(mf.never_married.incomeGenderT[ , 2])
# 94.3% of men who never married make less than $50K/year
mf.never_married.incomeGenderT[, 1][1] / sum(mf.never_married.incomeGenderT[ , 1])  
# 96.2% of women who never married make less than $50k/year

#### Comparison via Chi-square test
mf.never_married.incomeGenderT
chisq.test(mf.never_married.incomeGenderT)  # p-value << 0.05  
# Yes, it is a stat. sig. difference in number of individuals who've never married 
# with income greater than $50K/year between male and female (in favor of men).
# Note that this is without fixing any other variables, such as age group, education,
# race, number of work hours per week, or occupational field.
cramersV(mf.never_married.incomeGenderT)


###### Comparing average age of men and women for this dataset
#### means and medians for male and female who never married
m.never_married.age.mean <- round(mean(m.never_married$age), 1)
f.never_married.age.mean <- round(mean(f.never_married$age), 1)
m.never_married.age.median <- median(m.never_married$age)
f.never_married.age.median <- median(f.never_married$age)
m.never_married.age.mean
f.never_married.age.mean
m.never_married.age.median
f.never_married.age.median

#### plotting age histograms for male and female who never married
mf.never_married.age.histogram1 <- ggplot(mf.never_married, aes(x=age, fill=sex)) + 
  geom_histogram(position='stack', binwidth=1) 
mf.never_married.age.histogram1

mf.never_married.age.histogram2 <- ggplot(mf.never_married, aes(x=age, fill=sex)) + 
  geom_histogram(position='fill', binwidth=1)
mf.never_married.age.histogram2

mf.never_married.age.histogram3 <- ggplot(mf.never_married, aes(x=age, fill=sex)) + 
  geom_histogram(position='dodge', binwidth=1) + 
  labs(title='Histogram of male and female individuals\' age')
mf.never_married.age.histogram3

dev.copy(png, 'images/histogram_of_male_and_female_individuals_age.png')
dev.off()

m.never_married.age.hist <- ggplot(m.never_married, aes(x=age)) + 
  geom_histogram(fill='#00BFC4', binwidth=1) + 
  geom_segment(x=m.never_married.age.mean, y=0, xend=m.never_married.age.mean, yend=400, size=1, color='blue') +
  geom_segment(x=m.never_married.age.median, y=0, xend=m.never_married.age.median, yend=400, size=1, color='yellow') +
  annotate('text', x=75, y=200, label=paste('mean:', m.never_married.age.mean, '\n', 'median:', m.never_married.age.median)) + 
  labs(title='Histogram of age of males who never married')
m.never_married.age.hist

f.never_married.age.hist <- ggplot(f.never_married, aes(x=age)) + 
  geom_histogram(fill='#F8766D', binwidth=1) + 
  geom_segment(x=f.never_married.age.mean, y=0, xend=f.never_married.age.mean, yend=300, size=1, color='blue') +
  geom_segment(x=f.never_married.age.median, y=0, xend=f.never_married.age.median, yend=300, size=1, color='yellow') +
  annotate('text', x=75, y=200, label=paste('mean:', f.never_married.age.mean, '\n', 'median:', f.never_married.age.median)) + 
  labs(title='Histogram of age of females who never married')
f.never_married.age.hist

grid.arrange(m.never_married.age.hist, f.never_married.age.hist)
dev.copy(png, 'images/histograms_of_age_of_males_and_females_who_never_married.png')
dev.off()

#### Is the mean age difference between genders stat. sig.?
m.never_married.age.mean
f.never_married.age.mean

#### Conduct an indepedent t-test or Mann-Whitney U test
### Three assumptions of independent t-tests
## 1. Roughly the same sample sizes 
## 2. Normal distribution of samples compared
## 3. Homogeneity of variance

### checking for rough the same sample sizes
length(m.never_married$age)
length(f.never_married$age)

## checking for normal distributions
describe(m.never_married$age)  # skew < 3; kurtosis < 10; "normal enough"
describe(f.never_married$age)  # skew < 3; kurtosis < 10; "normal enough"

## checking for homogeneity of variance
leveneTest(mf.never_married$age ~ mf.never_married$sex)  
# homogeneity of variance assumption violated; resort to Mann-Whitney U test (non-parametric)

## Mann-Whitney U test 
wilcox.test(m.never_married$age, f.never_married$age, paired=F)  
# mean age difference between male and female NOT stat. sig. 

#### Since there seems to be no stat. sig. age difference between genders in our dataset, 
#### there must be another variable, which may account for the fact that men has
#### higher percentage of individuals making $50K/year or more than women.



###### Perhaps, there exists difference in work hours per week between male and female
###### Who works more hours: male or female? 
#### boxplot of working hours per week by gender
ggplot(mf.never_married, aes(x=sex, y=hours_per_week, fill=sex)) + 
  geom_boxplot() + 
  ylab('work hours per week') +
  labs(title='Work hours per week by gender of individuals \n who never married')
dev.copy(png, 'images/work_hours_per_week_by_gender_of_individuals_who_never_married.png')
dev.off()

#### mean and median work hours per week by gender
round(mean(m.never_married$hours_per_week), 1)
round(mean(f.never_married$hours_per_week), 1)
median(m.never_married$hours_per_week)
median(f.never_married$hours_per_week)

#### Is the means difference stat. sig.? Independent t-test or Mann-Whitney U test.
### Assumptions of independent t-tests
# 1. equal (or roughly equal) sample sizes
# 2. homogeneity of variance
# 3. normal distribution of samples

### comparing sample sizes (one of the assumptions for independent t-test)
length(m.never_married$hours_per_week)
length(f.never_married$hours_per_week)
# roughly equal sample sizes

### comparing distribution curves
hist(m.never_married$hours_per_week)  # normal distribution???
hist(f.never_married$hours_per_week)  # normal distribution???
describe(male$hours_per_week)  # skew < 3; kurtosis < 10; normal enough
describe(female$hours_per_week)  # skew < 3; kurtosis < 10; normal enough
# distribution curves for both samples "normal enough"

### test for homogeneity of variances across the two groups
leveneTest(mf.never_married$hours_per_week ~ mf.never_married$sex)
# no good; homogeneity assumption NOT satisfied
# no independent t-test; perform Mann-Whitney U test intead

### non-parametric test: Mann-Whitney U test
wilcox.test(m.never_married$hours_per_week, f.never_married$hours_per_week)  
# means difference stat. sig.

#### Yes, men work more hours than female on average and the means difference is stat. sig.



###### Is there difference in education between male and female?
ggplot(mf.never_married, aes(x=education, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
# Note: There are about 1100 more men than women in the dataset.
n.m.never_married - n.f.never_married

#### Independent t-test or Mann-Whitney U test (comparing education_num for men and women)
round(mean(m.never_married$education_num), 1)
round(mean(f.never_married$education_num), 1)
median(m.never_married$education_num)
median(f.never_married$education_num)

### checking for normal distribution
hist(m.never_married$education_num)  # normal enough?
hist(f.never_married$education_num)  # normal enough?
describe(m.never_married$education_num)  # skew < 3; kurtosis < 10; normal enough
describe(f.never_married$education_num)  # skew < 3; kurtosis < 10; normal enough

### checking for homogeneity of variance
var(m.never_married$education_num)
var(f.never_married$education_num)
leveneTest(mf.never_married$education_num ~ mf.never_married$sex)
# stat. sig.; not good for independent t-test; use Mann-Whitney U test instead

### conducting a non-parametric comparison test (Mann-Whitney U test)
wilcox.test(m.never_married$education_num, f.never_married$education_num)  
# means diff. stat. sig. (women on average have higher number of years in school)



###### Is there difference in occupation between male and female?
ggplot(mf.never_married, aes(x=occupation, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='Individuals who never married by occupation type and gender') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) 
dev.copy(png, 'images/individuals_who_never_married_by_occupation_type_and_gender.png')
dev.off()



###### Comparison in fixed conditions
###### Round 1 
###### Comparing individuals who've never married the same conditions in the following
###### categories: age group, number of work hours per week, occupational field, and 
###### education

# occupational field
ggplot(mf.never_married, aes(x=occupation, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
occupation.var <- c('Exec-managerial', 'Other-service', 'Prof-specialty', 'Sales')

# age group
ggplot(mf.never_married, aes(x=age_group, fill=sex)) + 
  geom_bar(position='dodge')
age_group.var <- c('[20,30)', '[30,40)')

# work hours per week
hpw.var <- c(40)

# education
ggplot(mf.never_married, aes(x=education, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))
education.var <- c('HS-grad', 'Some-college')

# possible combinations
fixed.conditionals <- expand.grid(occupation.var, age_group.var, hpw.var, education.var)
head(fixed.conditionals)
dim(fixed.conditionals)

# naming columns and ordering by occupation, age group, and education
colnames(fixed.conditionals) <- c('occupation', 'age_group', 'hours_per_week', 'education')
fixed.conditionals <- fixed.conditionals[order(fixed.conditionals$occupation,
                                               fixed.conditionals$age_group,
                                               - rank(fixed.conditionals$education)), ]
head(fixed.conditionals)
dim(fixed.conditionals)  


###### Case 1
fixed.conditionals[1, ]
m.exec.20s.40hpw.sc.never_married <- subset(m.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
f.exec.20s.40hpw.sc.never_married <- subset(f.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
mf.exec.20s.40hpw.sc.never_married <- rbind(m.exec.20s.40hpw.sc.never_married,
                                            f.exec.20s.40hpw.sc.never_married)

# individuals by income and gender
a <- ggplot(mf.exec.20s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(a) 20s; some college')
a

# income-gender table
mf.exec.20s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.exec.20s.40hpw.sc.never_married$income, mf.exec.20s.40hpw.sc.never_married$sex)
mf.exec.20s.40hpw.sc.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.exec.20s.40hpw.sc.never_married.incomeGenderT)  # not stat. sig.


###### Case 2
fixed.conditionals[2, ]
m.exec.20s.40hpw.hs.never_married <- subset(m.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
f.exec.20s.40hpw.hs.never_married <- subset(f.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
mf.exec.20s.40hpw.hs.never_married <- rbind(m.exec.20s.40hpw.hs.never_married,
                                            f.exec.20s.40hpw.hs.never_married)

# individuals by income and gender
b <- ggplot(mf.exec.20s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(b) 20s; high school')
b

# income-gender table
mf.exec.20s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.exec.20s.40hpw.hs.never_married$income, mf.exec.20s.40hpw.hs.never_married$sex)
mf.exec.20s.40hpw.hs.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.exec.20s.40hpw.hs.never_married.incomeGenderT)  # not stat. sig.


###### Case 3
fixed.conditionals[3, ]
m.exec.30s.40hpw.sc.never_married <- subset(m.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
f.exec.30s.40hpw.sc.never_married <- subset(f.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
mf.exec.30s.40hpw.sc.never_married <- rbind(m.exec.30s.40hpw.sc.never_married,
                                            f.exec.30s.40hpw.sc.never_married)

# individuals by income and gender
c <- ggplot(mf.exec.30s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(c) 30s; some college')
c

# income-gender table
mf.exec.30s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.exec.30s.40hpw.sc.never_married$income, mf.exec.30s.40hpw.sc.never_married$sex)
mf.exec.30s.40hpw.sc.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.exec.30s.40hpw.sc.never_married.incomeGenderT)  # not stat. sig.


###### Case 4
fixed.conditionals[4, ]
m.exec.30s.40hpw.hs.never_married <- subset(m.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
f.exec.30s.40hpw.hs.never_married <- subset(f.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
mf.exec.30s.40hpw.hs.never_married <- rbind(m.exec.30s.40hpw.hs.never_married,
                                            f.exec.30s.40hpw.hs.never_married)

# individuals by income and gender
d <- ggplot(mf.exec.30s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(d) 30s; high school')
d

# income-gender table
mf.exec.30s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.exec.30s.40hpw.hs.never_married$income, mf.exec.30s.40hpw.hs.never_married$sex)
mf.exec.30s.40hpw.hs.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.exec.30s.40hpw.hs.never_married.incomeGenderT)  # not stat. sig.


###### Case 5
fixed.conditionals[5, ]
m.other.20s.40hpw.sc.never_married <- subset(m.never_married, 
                                            occupation == 'Other-service' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
f.other.20s.40hpw.sc.never_married <- subset(f.never_married, 
                                            occupation == 'Other-service' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
mf.other.20s.40hpw.sc.never_married <- rbind(m.other.20s.40hpw.sc.never_married,
                                            f.other.20s.40hpw.sc.never_married)

# individuals by income and gender
e <- ggplot(mf.other.20s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(e) 20s; some college')
e

# income-gender table
mf.other.20s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.other.20s.40hpw.sc.never_married$income, mf.other.20s.40hpw.sc.never_married$sex)
mf.other.20s.40hpw.sc.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.other.20s.40hpw.sc.never_married.incomeGenderT)  # not stat. sig.


###### Case 6
fixed.conditionals[6, ]
m.other.20s.40hpw.hs.never_married <- subset(m.never_married, 
                                            occupation == 'Other-service' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
f.other.20s.40hpw.hs.never_married <- subset(f.never_married, 
                                            occupation == 'Other-service' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
mf.other.20s.40hpw.hs.never_married <- rbind(m.other.20s.40hpw.hs.never_married,
                                            f.other.20s.40hpw.hs.never_married)

# individuals by income and gender
f <- ggplot(mf.other.20s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(f) 20s; high school')
f

# income-gender table
mf.other.20s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.other.20s.40hpw.hs.never_married$income, mf.other.20s.40hpw.hs.never_married$sex)
mf.other.20s.40hpw.hs.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.other.20s.40hpw.hs.never_married.incomeGenderT)  # not stat. sig.


###### Case 7
fixed.conditionals[7, ]
m.other.30s.40hpw.sc.never_married <- subset(m.never_married, 
                                            occupation == 'Other-service' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
f.other.30s.40hpw.sc.never_married <- subset(f.never_married, 
                                            occupation == 'Other-service' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
mf.other.30s.40hpw.sc.never_married <- rbind(m.other.30s.40hpw.sc.never_married,
                                            f.other.30s.40hpw.sc.never_married)

# individuals by income and gender
g <- ggplot(mf.other.30s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(g) 30s; some college')
g

# income-gender table
mf.other.30s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.other.30s.40hpw.sc.never_married$income, mf.other.30s.40hpw.sc.never_married$sex)
mf.other.30s.40hpw.sc.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.other.30s.40hpw.sc.never_married.incomeGenderT)  # not stat. sig.


###### Case 8
fixed.conditionals[8, ]
m.other.30s.40hpw.hs.never_married <- subset(m.never_married, 
                                            occupation == 'Other-service' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
f.other.30s.40hpw.hs.never_married <- subset(f.never_married, 
                                            occupation == 'Other-service' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
mf.other.30s.40hpw.hs.never_married <- rbind(m.other.30s.40hpw.hs.never_married,
                                            f.other.30s.40hpw.hs.never_married)

# individuals by income and gender
h <- ggplot(mf.other.30s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(h) 30s; high school')
h 

# income-gender table
mf.other.30s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.other.30s.40hpw.hs.never_married$income, mf.other.30s.40hpw.hs.never_married$sex)
mf.other.30s.40hpw.hs.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.other.30s.40hpw.hs.never_married.incomeGenderT)  # not stat. sig.


###### Case 9
fixed.conditionals[9, ]
m.specialty.20s.40hpw.sc.never_married <- subset(m.never_married, 
                                            occupation == 'Prof-specialty' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
f.specialty.20s.40hpw.sc.never_married <- subset(f.never_married, 
                                            occupation == 'Prof-specialty' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
mf.specialty.20s.40hpw.sc.never_married <- rbind(m.specialty.20s.40hpw.sc.never_married,
                                            f.specialty.20s.40hpw.sc.never_married)

# individuals by income and gender
i <- ggplot(mf.specialty.20s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(i) 20s; some college')
i

# income-gender table
mf.specialty.20s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.specialty.20s.40hpw.sc.never_married$income, mf.specialty.20s.40hpw.sc.never_married$sex)
mf.specialty.20s.40hpw.sc.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.specialty.20s.40hpw.sc.never_married.incomeGenderT)  # not stat. sig.


###### Case 10
fixed.conditionals[10, ]
m.specialty.20s.40hpw.hs.never_married <- subset(m.never_married, 
                                            occupation == 'Prof-specialty' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
f.specialty.20s.40hpw.hs.never_married <- subset(f.never_married, 
                                            occupation == 'Prof-specialty' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
mf.specialty.20s.40hpw.hs.never_married <- rbind(m.specialty.20s.40hpw.hs.never_married,
                                            f.specialty.20s.40hpw.hs.never_married)

# individuals by income and gender
j <- ggplot(mf.specialty.20s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(j) 20s; high school')
j

# income-gender table
mf.specialty.20s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.specialty.20s.40hpw.hs.never_married$income, mf.specialty.20s.40hpw.hs.never_married$sex)
mf.specialty.20s.40hpw.hs.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.specialty.20s.40hpw.hs.never_married.incomeGenderT)  # not stat. sig.


###### Case 11
fixed.conditionals[11, ]
m.specialty.30s.40hpw.sc.never_married <- subset(m.never_married, 
                                            occupation == 'Prof-specialty' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
f.specialty.30s.40hpw.sc.never_married <- subset(f.never_married, 
                                            occupation == 'Prof-specialty' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'Some-college')
mf.specialty.30s.40hpw.sc.never_married <- rbind(m.specialty.30s.40hpw.sc.never_married,
                                            f.specialty.30s.40hpw.sc.never_married)

# individuals by income and gender
k <- ggplot(mf.specialty.30s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(k) 30s; some college')
k

# income-gender table
mf.specialty.30s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.specialty.30s.40hpw.sc.never_married$income, mf.specialty.30s.40hpw.sc.never_married$sex)
mf.specialty.30s.40hpw.sc.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.specialty.30s.40hpw.sc.never_married.incomeGenderT)  # not stat. sig.


###### Case 12
fixed.conditionals[12, ]
m.specialty.30s.40hpw.hs.never_married <- subset(m.never_married, 
                                            occupation == 'Prof-specialty' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
f.specialty.30s.40hpw.hs.never_married <- subset(f.never_married, 
                                            occupation == 'Prof-specialty' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40 &
                                              education == 'HS-grad')
mf.specialty.30s.40hpw.hs.never_married <- rbind(m.specialty.30s.40hpw.hs.never_married,
                                            f.specialty.30s.40hpw.hs.never_married)

# individuals by income and gender
l <- ggplot(mf.specialty.30s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(l) 30s; high school')
l

# income-gender table
mf.specialty.30s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.specialty.30s.40hpw.hs.never_married$income, mf.specialty.30s.40hpw.hs.never_married$sex)
mf.specialty.30s.40hpw.hs.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.specialty.30s.40hpw.hs.never_married.incomeGenderT)  # not stat. sig.


###### Case 13
fixed.conditionals[13, ]
m.sales.20s.40hpw.sc.never_married <- subset(m.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[20,30)' &
                                               hours_per_week == 40 &
                                               education == 'Some-college')
f.sales.20s.40hpw.sc.never_married <- subset(f.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[20,30)' &
                                               hours_per_week == 40 &
                                               education == 'Some-college')
mf.sales.20s.40hpw.sc.never_married <- rbind(m.sales.20s.40hpw.sc.never_married,
                                             f.sales.20s.40hpw.sc.never_married)

m <- ggplot(mf.sales.20s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(m) 20s; some college')
m

mf.sales.20s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.sales.20s.40hpw.sc.never_married$income, mf.sales.20s.40hpw.sc.never_married$sex)
mf.sales.20s.40hpw.sc.never_married.incomeGenderT

# Fisher's exact Test
fisher.test(mf.sales.20s.40hpw.sc.never_married.incomeGenderT)  # not stat. sig.


###### Case 14
fixed.conditionals[14, ]
m.sales.20s.40hpw.hs.never_married <- subset(m.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[20,30)' &
                                               hours_per_week == 40 &
                                               education == 'HS-grad')
f.sales.20s.40hpw.hs.never_married <- subset(f.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[20,30)' &
                                               hours_per_week == 40 &
                                               education == 'HS-grad')
mf.sales.20s.40hpw.hs.never_married <- rbind(m.sales.20s.40hpw.hs.never_married,
                                             f.sales.20s.40hpw.hs.never_married)

# individuals by income and gender
n <- ggplot(mf.sales.20s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(n) 20s; high school')
n

# income-gender table
mf.sales.20s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.sales.20s.40hpw.hs.never_married$income, mf.sales.20s.40hpw.hs.never_married$sex)
mf.sales.20s.40hpw.hs.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.sales.20s.40hpw.hs.never_married.incomeGenderT)  # not stat. sig.


###### Case 15
fixed.conditionals[15, ]
m.sales.30s.40hpw.sc.never_married <- subset(m.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[30,40)' &
                                               hours_per_week == 40 &
                                               education == 'Some-college')
f.sales.30s.40hpw.sc.never_married <- subset(f.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[30,40)' &
                                               hours_per_week == 40 &
                                               education == 'Some-college')
mf.sales.30s.40hpw.sc.never_married <- rbind(m.sales.30s.40hpw.sc.never_married,
                                             f.sales.30s.40hpw.sc.never_married)

# individuals by income and gender
o <- ggplot(mf.sales.30s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(o) 30s; some college')
o

# gender-income table
mf.sales.30s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.sales.30s.40hpw.sc.never_married$income, mf.sales.30s.40hpw.sc.never_married$sex)
mf.sales.30s.40hpw.sc.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.sales.30s.40hpw.sc.never_married.incomeGenderT)  # not stat. sig.


###### Case 16
fixed.conditionals[16, ]
m.sales.30s.40hpw.hs.never_married <- subset(m.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[30,40)' &
                                               hours_per_week == 40 &
                                               education == 'HS-grad')
f.sales.30s.40hpw.hs.never_married <- subset(f.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[30,40)' &
                                               hours_per_week == 40 &
                                               education == 'HS-grad')
mf.sales.30s.40hpw.hs.never_married <- rbind(m.sales.30s.40hpw.hs.never_married,
                                             f.sales.30s.40hpw.hs.never_married)

# Individuals by income and gender
p <- ggplot(mf.sales.30s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(p) 30s; high school')
p

# gender-income table
mf.sales.30s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.sales.30s.40hpw.hs.never_married$income, mf.sales.30s.40hpw.hs.never_married$sex)
mf.sales.30s.40hpw.hs.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.sales.30s.40hpw.hs.never_married.incomeGenderT)  # not stat. sig.


###### Creating plots
grid.arrange(a, b, c, d, main='Individuals in executive/managerial field \n who never married and work 40 hours per week')
dev.copy(png, 'images/individuals_in_exec-managerial_field_who_never_married_and_work_40hpw.png')
dev.off()

grid.arrange(e, f, g, h, main='Individuals in other service field \n who never married and work 40 hours per week')
dev.copy(png, 'images/individuals_in_other-service_field_who_never_married_and_work_40hpw.png')
dev.off()

grid.arrange(i, j, k, l, main='Individuals in professional specialty field \n who never married and work 40 hours per week')
dev.copy(png, 'images/individuals_in_prof-specialty_field_who_never_married_and_work_40hpw.png')
dev.off()

grid.arrange(m, n, o, p, main='Individuals in sales field \n who never married and work 40 hours per week')
dev.copy(png, 'images/individuals_in_sales_field_who_never_married_and_work_40hpw.png')
dev.off()



###### Comparison in fixed conditions
###### Round 2
###### Comparing individuals who've never married the same conditions in the following
###### categories: age group, number of work hours per week, and occupational field
###### (not fixing the education variable)

# occupational field
occupation.var <- c('Exec-managerial', 'Other-service', 'Prof-specialty', 'Sales')

# age group
age_group.var <- c('[20,30)', '[30,40)')

# work hours per week
hpw.var <- c(40)

# possible combinations
fixed.conditionals <- expand.grid(occupation.var, age_group.var, hpw.var)
head(fixed.conditionals)
dim(fixed.conditionals)

# naming columns and ordering by occupation, age group, and education
colnames(fixed.conditionals) <- c('occupation', 'age_group', 'hours_per_week')
fixed.conditionals <- fixed.conditionals[order(fixed.conditionals$occupation,
                                               fixed.conditionals$age_group), ]
head(fixed.conditionals)
dim(fixed.conditionals)  


###### Case 1
fixed.conditionals[1, ]
m.exec.20s.40hpw.never_married <- subset(m.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40)
f.exec.20s.40hpw.never_married <- subset(f.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[20,30)' &
                                              hours_per_week == 40)
mf.exec.20s.40hpw.never_married <- rbind(m.exec.20s.40hpw.never_married,
                                            f.exec.20s.40hpw.never_married)

# individuals by income and gender
q <- ggplot(mf.exec.20s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') +
  labs(title='(q) 20s')
q

# income-gender table
mf.exec.20s.40hpw.never_married.incomeGenderT <- 
  table(mf.exec.20s.40hpw.never_married$income, mf.exec.20s.40hpw.never_married$sex)
mf.exec.20s.40hpw.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.exec.20s.40hpw.never_married.incomeGenderT)  # not stat. sig.


###### Case 2
fixed.conditionals[2, ]
m.exec.30s.40hpw.never_married <- subset(m.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40)
f.exec.30s.40hpw.never_married <- subset(f.never_married, 
                                            occupation == 'Exec-managerial' &
                                              age_group == '[30,40)' &
                                              hours_per_week == 40)
mf.exec.30s.40hpw.never_married <- rbind(m.exec.30s.40hpw.never_married,
                                            f.exec.30s.40hpw.never_married)

# individuals by income and gender
r <- ggplot(mf.exec.30s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(r) 30s')
r

# income-gender table
mf.exec.30s.40hpw.never_married.incomeGenderT <- 
  table(mf.exec.30s.40hpw.never_married$income, mf.exec.30s.40hpw.never_married$sex)
mf.exec.30s.40hpw.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.exec.30s.40hpw.never_married.incomeGenderT)  # stat. sig.


###### Case 3
fixed.conditionals[3, ]
m.other.20s.40hpw.never_married <- subset(m.never_married, 
                                             occupation == 'Other-service' &
                                               age_group == '[20,30)' &
                                               hours_per_week == 40)
f.other.20s.40hpw.never_married <- subset(f.never_married, 
                                             occupation == 'Other-service' &
                                               age_group == '[20,30)' &
                                               hours_per_week == 40)
mf.other.20s.40hpw.never_married <- rbind(m.other.20s.40hpw.never_married,
                                             f.other.20s.40hpw.never_married)

# individuals by income and gender
s <- ggplot(mf.other.20s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(s) 20s')
s

# income-gender table
mf.other.20s.40hpw.never_married.incomeGenderT <- 
  table(mf.other.20s.40hpw.never_married$income, mf.other.20s.40hpw.never_married$sex)
mf.other.20s.40hpw.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.other.20s.40hpw.never_married.incomeGenderT)  # not stat. sig.


###### Case 4
fixed.conditionals[4, ]
m.other.30s.40hpw.never_married <- subset(m.never_married, 
                                             occupation == 'Other-service' &
                                               age_group == '[30,40)' &
                                               hours_per_week == 40)
f.other.30s.40hpw.never_married <- subset(f.never_married, 
                                             occupation == 'Other-service' &
                                               age_group == '[30,40)' &
                                               hours_per_week == 40)
mf.other.30s.40hpw.never_married <- rbind(m.other.30s.40hpw.never_married,
                                             f.other.30s.40hpw.never_married)

# individuals by income and gender
t <- ggplot(mf.other.30s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(t) 30s')
t

# income-gender table
mf.other.30s.40hpw.never_married.incomeGenderT <- 
  table(mf.other.30s.40hpw.never_married$income, mf.other.30s.40hpw.never_married$sex)
mf.other.30s.40hpw.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.other.30s.40hpw.never_married.incomeGenderT)  # not stat. sig.


###### Case 5
fixed.conditionals[5, ]
m.specialty.20s.40hpw.never_married <- subset(m.never_married, 
                                                 occupation == 'Prof-specialty' &
                                                   age_group == '[20,30)' &
                                                   hours_per_week == 40)
f.specialty.20s.40hpw.never_married <- subset(f.never_married, 
                                                 occupation == 'Prof-specialty' &
                                                   age_group == '[20,30)' &
                                                   hours_per_week == 40)
mf.specialty.20s.40hpw.never_married <- rbind(m.specialty.20s.40hpw.never_married,
                                                 f.specialty.20s.40hpw.never_married)

# individuals by income and gender
u <- ggplot(mf.specialty.20s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(u) 20s')
u

# income-gender table
mf.specialty.20s.40hpw.never_married.incomeGenderT <- 
  table(mf.specialty.20s.40hpw.never_married$income, mf.specialty.20s.40hpw.never_married$sex)
mf.specialty.20s.40hpw.never_married.incomeGenderT  # one of the cell counts 5 or less; cannot perform chi-square test

# Fisher's exact test
fisher.test(mf.specialty.20s.40hpw.never_married.incomeGenderT)  # not stat. sig.


###### Case 6
fixed.conditionals[6, ]
m.specialty.30s.40hpw.never_married <- subset(m.never_married, 
                                                 occupation == 'Prof-specialty' &
                                                   age_group == '[30,40)' &
                                                   hours_per_week == 40)
f.specialty.30s.40hpw.never_married <- subset(f.never_married, 
                                                 occupation == 'Prof-specialty' &
                                                   age_group == '[30,40)' &
                                                   hours_per_week == 40)
mf.specialty.30s.40hpw.never_married <- rbind(m.specialty.30s.40hpw.never_married,
                                                 f.specialty.30s.40hpw.never_married)

# individuals by income and gender
v <- ggplot(mf.specialty.30s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(v) 30s')
v

# income-gender table
mf.specialty.30s.40hpw.never_married.incomeGenderT <- 
  table(mf.specialty.30s.40hpw.never_married$income, mf.specialty.30s.40hpw.never_married$sex)
mf.specialty.30s.40hpw.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.specialty.30s.40hpw.never_married.incomeGenderT)  # stat. sig.


###### Case 7
fixed.conditionals[7, ]
m.sales.20s.40hpw.never_married <- subset(m.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[20,30)' &
                                               hours_per_week == 40)
f.sales.20s.40hpw.never_married <- subset(f.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[20,30)' &
                                               hours_per_week == 40)
mf.sales.20s.40hpw.never_married <- rbind(m.sales.20s.40hpw.never_married,
                                             f.sales.20s.40hpw.never_married)

w <- ggplot(mf.sales.20s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(w) 20s')
w

mf.sales.20s.40hpw.never_married.incomeGenderT <- 
  table(mf.sales.20s.40hpw.never_married$income, mf.sales.20s.40hpw.never_married$sex)
mf.sales.20s.40hpw.never_married.incomeGenderT

# Fisher's exact Test
fisher.test(mf.sales.20s.40hpw.never_married.incomeGenderT)  # not stat. sig.


###### Case 8
fixed.conditionals[8, ]
m.sales.30s.40hpw.never_married <- subset(m.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[30,40)' &
                                               hours_per_week == 40)
f.sales.30s.40hpw.never_married <- subset(f.never_married, 
                                             occupation == 'Sales' &
                                               age_group == '[30,40)' &
                                               hours_per_week == 40)
mf.sales.30s.40hpw.never_married <- rbind(m.sales.30s.40hpw.never_married,
                                             f.sales.30s.40hpw.never_married)

# individuals by income and gender
x <- ggplot(mf.sales.30s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(x) 30s')
x

# gender-income table
mf.sales.30s.40hpw.never_married.incomeGenderT <- 
  table(mf.sales.30s.40hpw.never_married$income, mf.sales.30s.40hpw.never_married$sex)
mf.sales.30s.40hpw.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.sales.30s.40hpw.never_married.incomeGenderT)  # stat. sig.


###### Creating plots
grid.arrange(q, r, main='Individuals in executive/managerial field \n who never married and work 40 hours per week')
dev.copy(png, 'images/individuals_in_exec-managerial_field_who_never_married_and_work_40hpw_2.png')
dev.off()

grid.arrange(s, t, main='Individuals in other service field \n who never married and work 40 hours per week')
dev.copy(png, 'images/individuals_in_other-service_field_who_never_married_and_work_40hpw_2.png')
dev.off()

grid.arrange(u, v, main='Individuals in professional specialty field \n who never married and work 40 hours per week')
dev.copy(png, 'images/individuals_in_prof-specialty_field_who_never_married_and_work_40hpw_2.png')
dev.off()

grid.arrange(w, x, main='Individiuals in sales field \n who never married and work 40 hours per week')
dev.copy(png, 'images/individuals_in_sales_field_who_never_married_and_work_40hpw_2.png')
dev.off()



###### Comparison in fixed conditions
###### Round 3
###### Comparing individuals who've never married the same conditions in the following
###### categories: age group and occupational field
###### (not fixing the education background or the number of work hours per week)

# occupational field
occupation.var <- c('Exec-managerial', 'Other-service', 'Prof-specialty', 'Sales')

# age group
age_group.var <- c('[20,30)', '[30,40)')

# possible combinations
fixed.conditionals <- expand.grid(occupation.var, age_group.var)
head(fixed.conditionals)
dim(fixed.conditionals)

# naming columns and ordering by occupation and age group
colnames(fixed.conditionals) <- c('occupation', 'age_group')
fixed.conditionals <- fixed.conditionals[order(fixed.conditionals$occupation,
                                               fixed.conditionals$age_group), ]
head(fixed.conditionals)
dim(fixed.conditionals)  


###### Case 1
fixed.conditionals[1, ]
m.exec.20s.never_married <- subset(m.never_married, 
                                         occupation == 'Exec-managerial' &
                                           age_group == '[20,30)')
f.exec.20s.never_married <- subset(f.never_married, 
                                         occupation == 'Exec-managerial' &
                                           age_group == '[20,30)')
mf.exec.20s.never_married <- rbind(m.exec.20s.never_married,
                                         f.exec.20s.never_married)

# individuals by income and gender
y <- ggplot(mf.exec.20s.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') +
  labs(title='(y) 20s')
y

# income-gender table
mf.exec.20s.never_married.incomeGenderT <- 
  table(mf.exec.20s.never_married$income, mf.exec.20s.never_married$sex)
mf.exec.20s.never_married.incomeGenderT

# Chi-square test
chisq.test(mf.exec.20s.never_married.incomeGenderT)  # not stat. sig.


###### Case 2
fixed.conditionals[2, ]
m.exec.30s.never_married <- subset(m.never_married, 
                                         occupation == 'Exec-managerial' &
                                           age_group == '[30,40)')
f.exec.30s.never_married <- subset(f.never_married, 
                                         occupation == 'Exec-managerial' &
                                           age_group == '[30,40)')
mf.exec.30s.never_married <- rbind(m.exec.30s.never_married,
                                         f.exec.30s.never_married)

# individuals by income and gender
z <- ggplot(mf.exec.30s.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(z) 30s')
z

# income-gender table
mf.exec.30s.never_married.incomeGenderT <- 
  table(mf.exec.30s.never_married$income, mf.exec.30s.never_married$sex)
mf.exec.30s.never_married.incomeGenderT

# Chi-square test
chisq.test(mf.exec.30s.never_married.incomeGenderT)  # not stat. sig.


###### Case 3
fixed.conditionals[3, ]
m.other.20s.never_married <- subset(m.never_married, 
                                          occupation == 'Other-service' &
                                            age_group == '[20,30)')
f.other.20s.never_married <- subset(f.never_married, 
                                          occupation == 'Other-service' &
                                            age_group == '[20,30)')
mf.other.20s.never_married <- rbind(m.other.20s.never_married,
                                          f.other.20s.never_married)

# individuals by income and gender
aa <- ggplot(mf.other.20s.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(aa) 20s')
aa

# income-gender table
mf.other.20s.never_married.incomeGenderT <- 
  table(mf.other.20s.never_married$income, mf.other.20s.never_married$sex)
mf.other.20s.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.other.20s.never_married.incomeGenderT)  # not stat. sig.


###### Case 4
fixed.conditionals[4, ]
m.other.30s.never_married <- subset(m.never_married, 
                                          occupation == 'Other-service' &
                                            age_group == '[30,40)')
f.other.30s.never_married <- subset(f.never_married, 
                                          occupation == 'Other-service' &
                                            age_group == '[30,40)')
mf.other.30s.never_married <- rbind(m.other.30s.never_married,
                                          f.other.30s.never_married)

# individuals by income and gender
ab <- ggplot(mf.other.30s.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(ab) 30s')
ab

# income-gender table
mf.other.30s.never_married.incomeGenderT <- 
  table(mf.other.30s.never_married$income, mf.other.30s.never_married$sex)
mf.other.30s.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.other.30s.never_married.incomeGenderT)  # not stat. sig.


###### Case 5
fixed.conditionals[5, ]
m.specialty.20s.never_married <- subset(m.never_married, 
                                              occupation == 'Prof-specialty' &
                                                age_group == '[20,30)')
f.specialty.20s.never_married <- subset(f.never_married, 
                                              occupation == 'Prof-specialty' &
                                                age_group == '[20,30)')
mf.specialty.20s.never_married <- rbind(m.specialty.20s.never_married,
                                              f.specialty.20s.never_married)

# individuals by income and gender
ac <- ggplot(mf.specialty.20s.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(ac) 20s')
ac

# income-gender table
mf.specialty.20s.never_married.incomeGenderT <- 
  table(mf.specialty.20s.never_married$income, mf.specialty.20s.never_married$sex)
mf.specialty.20s.never_married.incomeGenderT

# Chi-square test
chisq.test(mf.specialty.20s.never_married.incomeGenderT)  # not stat. sig.


###### Case 6
fixed.conditionals[6, ]
m.specialty.30s.never_married <- subset(m.never_married, 
                                              occupation == 'Prof-specialty' &
                                                age_group == '[30,40)')
f.specialty.30s.never_married <- subset(f.never_married, 
                                              occupation == 'Prof-specialty' &
                                                age_group == '[30,40)')
mf.specialty.30s.never_married <- rbind(m.specialty.30s.never_married,
                                              f.specialty.30s.never_married)

# individuals by income and gender
ad <- ggplot(mf.specialty.30s.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(ad) 30s')
ad

# income-gender table
mf.specialty.30s.never_married.incomeGenderT <- 
  table(mf.specialty.30s.never_married$income, mf.specialty.30s.never_married$sex)
mf.specialty.30s.never_married.incomeGenderT

# Chi-square test
chisq.test(mf.specialty.30s.never_married.incomeGenderT)  # stat. sig.


###### Case 7
fixed.conditionals[7, ]
m.sales.20s.never_married <- subset(m.never_married, 
                                          occupation == 'Sales' &
                                            age_group == '[20,30)')
f.sales.20s.never_married <- subset(f.never_married, 
                                          occupation == 'Sales' &
                                            age_group == '[20,30)')
mf.sales.20s.never_married <- rbind(m.sales.20s.never_married,
                                          f.sales.20s.never_married)

ae <- ggplot(mf.sales.20s.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(ae) 20s')
ae

mf.sales.20s.never_married.incomeGenderT <- 
  table(mf.sales.20s.never_married$income, mf.sales.20s.never_married$sex)
mf.sales.20s.never_married.incomeGenderT

# Fisher's exact Test
fisher.test(mf.sales.20s.never_married.incomeGenderT)  # stat. sig.


###### Case 8
fixed.conditionals[8, ]
m.sales.30s.never_married <- subset(m.never_married, 
                                          occupation == 'Sales' &
                                            age_group == '[30,40)')
f.sales.30s.never_married <- subset(f.never_married, 
                                          occupation == 'Sales' &
                                            age_group == '[30,40)')
mf.sales.30s.never_married <- rbind(m.sales.30s.never_married,
                                          f.sales.30s.never_married)

# individuals by income and gender
af <- ggplot(mf.sales.30s.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(af) 30s')
af

# gender-income table
mf.sales.30s.never_married.incomeGenderT <- 
  table(mf.sales.30s.never_married$income, mf.sales.30s.never_married$sex)
mf.sales.30s.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.sales.30s.never_married.incomeGenderT)  # stat. sig.


###### Creating plots
grid.arrange(y, z, main='Individuals in executive/managerial field who never married')
dev.copy(png, 'images/individuals_in_exec-managerial_field_who_never_married.png')
dev.off()

grid.arrange(aa, ab, main='Individuals in other service field who never married')
dev.copy(png, 'images/individuals_in_other-service_field_who_never_married.png')
dev.off()

grid.arrange(ac, ad, main='Individuals in professional specialty field who never married')
dev.copy(png, 'images/individuals_in_prof-specialty_field_who_never_married.png')
dev.off()

grid.arrange(ae, af, main='Individiuals in sales field who never married')
dev.copy(png, 'images/individuals_in_sales_field_who_never_married.png')
dev.off()



###### Comparison in fixed conditions
###### Round 4
###### Comparing individuals who've never married the same condition in the following
###### category: occupational field
###### (not fixing for the age group, education background, or number of work hours per week)

# occupational field
occupation.var <- c('Exec-managerial', 'Other-service', 'Prof-specialty', 'Sales')

# possible combinations
fixed.conditionals <- expand.grid(occupation.var)
head(fixed.conditionals)
dim(fixed.conditionals)

# naming columns and ordering by occupation, age group, and education
colnames(fixed.conditionals) <- c('occupation')
head(fixed.conditionals)
dim(fixed.conditionals)  


###### Case 1
fixed.conditionals[1, ]
m.exec.never_married <- subset(m.never_married, 
                                   occupation == 'Exec-managerial')
f.exec.never_married <- subset(f.never_married, 
                                   occupation == 'Exec-managerial')
mf.exec.never_married <- rbind(m.exec.never_married,
                                   f.exec.never_married)

# individuals by income and gender
ag <- ggplot(mf.exec.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') +
  labs(title='(ag) executives/managerial')
ag

# income-gender table
mf.exec.never_married.incomeGenderT <- 
  table(mf.exec.never_married$income, mf.exec.never_married$sex)
mf.exec.never_married.incomeGenderT

# Chi-square test
chisq.test(mf.exec.never_married.incomeGenderT)  # stat. sig.


###### Case 2
fixed.conditionals[2, ]
m.other.never_married <- subset(m.never_married, 
                                    occupation == 'Other-service')
f.other.never_married <- subset(f.never_married, 
                                    occupation == 'Other-service')
mf.other.never_married <- rbind(m.other.never_married,
                                    f.other.never_married)

# individuals by income and gender
ah <- ggplot(mf.other.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(ah) other service')
ah

# income-gender table
mf.other.never_married.incomeGenderT <- 
  table(mf.other.never_married$income, mf.other.never_married$sex)
mf.other.never_married.incomeGenderT

# Fisher's exact test
fisher.test(mf.other.never_married.incomeGenderT)  # not stat. sig.


###### Case 3
fixed.conditionals[3, ]
m.specialty.never_married <- subset(m.never_married, 
                                        occupation == 'Prof-specialty')
f.specialty.never_married <- subset(f.never_married, 
                                        occupation == 'Prof-specialty')
mf.specialty.never_married <- rbind(m.specialty.never_married,
                                        f.specialty.never_married)

# individuals by income and gender
ai <- ggplot(mf.specialty.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(ai) professional specialty')
ai

# income-gender table
mf.specialty.never_married.incomeGenderT <- 
  table(mf.specialty.never_married$income, mf.specialty.never_married$sex)
mf.specialty.never_married.incomeGenderT

# Chi-square test
chisq.test(mf.specialty.never_married.incomeGenderT)  # stat. sig.


###### Case 4
fixed.conditionals[4, ]
m.sales.never_married <- subset(m.never_married, 
                                    occupation == 'Sales')
f.sales.never_married <- subset(f.never_married, 
                                    occupation == 'Sales')
mf.sales.never_married <- rbind(m.sales.never_married,
                                    f.sales.never_married)

aj <- ggplot(mf.sales.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge') + 
  labs(title='(aj) sales')
aj

mf.sales.never_married.incomeGenderT <- 
  table(mf.sales.never_married$income, mf.sales.never_married$sex)
mf.sales.never_married.incomeGenderT

# Chi-square test
chisq.test(mf.sales.never_married.incomeGenderT)  # stat. sig.


###### Creating plots
grid.arrange(ag, ah, ai, aj, main='Individuals who never married')
dev.copy(png, 'images/individuals_who_never_married.png')
dev.off()
