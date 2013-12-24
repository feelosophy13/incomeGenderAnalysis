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
load('../init/incomeData.RData')


###### Brief overview of training data
head(incomeData)
str(incomeData)


###### Subsetting male and female
male <- subset(incomeData, sex=='Male')
female <- subset(incomeData, sex=='Female')


###### Trimming data so that we have equal number of male and female
table(incomeData$sex)
set.seed(13)
n <- nrow(female); n
rSelect <- sample(nrow(male), n, replace=F)
male <- male[rSelect, ]


###### Verifying equal number of sample sizes
head(male)
nrow(male)
nrow(female)


###### Creating a new dataframe with equal number of male and female
data <- rbind(male, female)


###### Who works more hours: male or female? ######

#### boxplot of working hours per week by gender
ggplot(data, aes(x=sex, y=hours_per_week, fill=sex)) + 
  geom_boxplot() + 
  ylab('work hours per week') +
  labs(title='Work Hours Per Week by Gender')
dev.copy(png, 'images/work_hours_per_week_by_gender.png')
dev.off()

#### means difference between working hours
mean(male$hours_per_week) 
mean(female$hours_per_week)
mean(male$hours_per_week) - mean(female$hours_per_week)

#### Is the means difference stat. sig.? Independent t-test.
## Assumptions of independent t-tests
# 1. equal (or roughly equal) sample sizes
# 2. homogeneity of variance
# 3. normal distribution of samples

## comparing distribution curves
hist(male$hours_per_week)  # normal distribution???
hist(female$hours_per_week)  # normal distribution???
describe(male$hours_per_week)  # normal enough
describe(female$hours_per_week)  # normal enough

## test for homogeneity of variances across the two groups
leveneTest(data$hours_per_week ~ data$sex)  # good; homogeneity assumption satisfied

## comparing sample sizes (one of the assumptions for independent t-test)
length(male$hours_per_week)
length(female$hours_per_week)

## independent t-test
t.test(male$hours_per_week, female$hours_per_week, var.equal=T)
# there exists stat. sig. difference in working hours per week between male and female

## non-parametric test: Mann-Whitney U test
wilcox.test(male$hours_per_week, female$hours_per_week)  # means difference stat. sig.

# Yes, men work more hours than female on average.


###### Which group has more people making over $50K salary: male or female
male.income.table <- table(male$income); male.income.table
female.income.table <- table(female$income); female.income.table

male.income.table[2] / length(male$income)  # 31% of all male make more than $50/year
female.income.table[2] / length(female$income)  # 11% of all female make more than $50/year
# There are more men making over $50K/year than female making over $50K/year.
# Is that a statistically significant phenomenon?

#### Comparison via Chi-square test
observed <- table(data$income, data$sex); observed
chisq.test(observed)  # p-value << 0.05  
# Yes, it is a statistically significant phenomenon.


###### Could this discrepancy due to the fact that women tend to work less hours than men?
###### We can compare only males and females who work 40 hours per week.
male.40hpw <- subset(male, hours_per_week == 40)
female.40hpw <- subset(female, hours_per_week == 40)
both.40hpw <- rbind(male.40hpw, female.40hpw)

male.40hpw.income.table <- table(male.40hpw$income); male.40hpw.income.table
female.40hpw.income.table <- table(female.40hpw$income); female.40hpw.income.table
# Is there a stat. sig. difference in the 'income' outcome biased by gender?

male.40hpw.income.table[2] / sum(male.40hpw.income.table)  # 27% of male who work 40 hours per week make more than $50K/year
female.40hpw.income.table[2] / sum(female.40hpw.income.table)  # 10% of female who work 40 hours per week make more than $50K/year

#### Comparison via Chi-square test
observed <- table(both.40hpw$income, both.40hpw$sex); observed
chisq.test(observed)  # p-value << 0.05  
# Yes, it is a statistically significant difference.


###### Even when male and female work the same number of hours (40 hours per week), more males
###### earned $50K/year than the female counterparts. Could this be due to the differences in 
###### the TYPES of jobs women and men perform? For example, do more female resort to
###### lower-paying jobs than do male (as many critics have argued)? 

###### Let's compare men and women in equal conditions to eliminate all other attributable 
###### factors (occupation, education, age group, marital status, etc.), that may account 
###### for the discrepancy in income.

###### ROUND 1
##### Picking out "fix" conditions for occupation
ggplot(data, aes(x=occupation, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

## Let's compare individuals in the same occupational field: 'Exec-managerial', 
## 'Prof-specialty', and 'Sales'job categories (since they contain roughly the equal number 
## of male and female individuals and still have large sample sizes). 
occupation_var <- c('Exec-managerial', 'Prof-specialty', 'Sales')


###### Picking out "fix" conditions for "workclass"
ggplot(data, aes(x=workclass, fill=sex)) + 
  geom_bar(position='dodge') +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## We will only compare individuals working in the 'Private' sector.
workclass_var <- c('Private')


###### Picking out "fix" conditions for age groups
ggplot(data, aes(x=age_group, fill=sex)) + 
  geom_bar(position='dodge')

## We will only compare individuals in the same age groups: the 30s and 40s age groups.
age_group_var <- c('[30,40)', '[40,50)')


###### Picking out "fix" conditions for individuals' education
ggplot(data, aes(x=education, fill=sex)) + 
  geom_bar(position='dodge') +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## We will only compare individiduals who share the same education level: 'HS-grad' and 
## 'Bachelors
education_var <- c('HS-grad', 'Bachelors')


###### Picking out "fix" conditions for marital status
ggplot(data, aes(x=marital_status, fill=sex)) +
  geom_bar(position='dodge') +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## We will only compare individuals who share the same marital status: 'Married-civ-spouse'
## and 'Never-married'
marital_status_var <- c('Married-civ-spouse', 'Never-married')


###### Picking out "fix" conditions for race
ggplot(data, aes(x=race, fill=sex)) + 
  geom_bar(position='dodge') +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## We will only compare white individuals.
race_var <- c('White')


####### Picking out "fix" conditions for working hours per week
ggplot(data, aes(x=hours_per_week, fill=sex)) + 
  geom_histogram(aes(y=..count..), binwidth=1)
sort(table(incomeData$hours_per_week))  # 40-hours-per-week is the most common

## We will only compare individuals who work 40 hours a week.
hpw_var <- c(40)


###### Picking out "fix" conditions for native country
ggplot(data, aes(x=native_country, fill=sex)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

## We will only compare individuals whose native country is the U.S.
native_country_var <- c('United-States')


###### Create a list of combinatoric cases with fixed variables and conditions
fixed.conditionals <- expand.grid(occupation_var, workclass_var, age_group_var, education_var,
                                  marital_status_var, race_var, hpw_var, native_country_var)
head(fixed.conditionals)
dim(fixed.conditionals)
colnames(fixed.conditionals) <- c('occupation', 'workclass', 'age_group', 'education', 
                                  'marital_status', 'race', 'hours_per_week', 'native_country')
head(fixed.conditionals)
dim(fixed.conditionals)  # 24 variation cases with fixed conditions/variables

###### Unfactoring to allow easier subsetting below

fixed.conditionals$occupation <- as.character(fixed.conditionals$occupation)
fixed.conditionals$workclass <- as.character(fixed.conditionals$workclass)
fixed.conditionals$age_group <- as.character(fixed.conditionals$age_group)
fixed.conditionals$education <- as.character(fixed.conditionals$education)
fixed.conditionals$marital_status <- as.character(fixed.conditionals$marital_status)
fixed.conditionals$race <- as.character(fixed.conditionals$race)
fixed.conditionals$hours_per_week <- as.character(fixed.conditionals$hours_per_week)
fixed.conditionals$native_country <- as.character(fixed.conditionals$native_country)


###### View the list of combinatoric cases with fixed variables and conditions
fixed.conditionals


###### Re-ordering fixed conditionals data frame
fixed.conditionals <- fixed.conditionals[order(fixed.conditionals$occupation,
                                               fixed.conditionals$age_group,
                                               - rank(fixed.conditionals$education),
                                               fixed.conditionals$marital_status), ]
fixed.conditionals


###### Case 1 with fixed conditions
fixed.conditionals[1, ]
m <- subset(male, 
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
mf <- rbind(m, f)

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# Is there a statistically significant difference in income by gender?

#### Comparison via Chi-square test
observed <- table(mf$income, mf$sex); observed
chisq.test(observed)  # p-value from Chi-square test discounted
# Chi-square method is a parametric test. When any of the cells in the observed table has
# 5 or less counts, we must resort to non-parametric test: Fisher's Exact test.

#### Comparison via Fisher's Exact test
fisher.test(observed)  # p-value = 1
# No, there is no stat. sig. difference.

m.income.table[2] / sum(m.income.table)  # 42.1% male with over $50K/year salary
f.income.table[2] / sum(f.income.table)  # 44.4% female with over $50K/year salary
# small:large = 11:8 for male
# smale:large = 5:4 for female

# no noticeable difference between male and female

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 36
quantile(f$age)  # median: 35
mean(m$age)  # mean: 34.9
mean(f$age)  # mean: 34.1

# independent t-test
leveneTest(test$age ~ test$sex)
t.test(m$age, f$age, var.equal=T)  # no stat. sig. means difference in age
# Whitney-Mann U test (since sample sizes differ)
wilcox.test(m$age, f$age, paired=F)  # no stat. sig. means difference in age

# keep this case (Case 1)


###### Case 2 with fixed conditions
fixed.conditionals[2, ]
m <- subset(male, 
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
mf <- rbind(m, f)

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table

m.income.table[2] / sum(m.income.table)
f.income.table[2] / sum(f.income.table)
# too small sample (n = 2 for male and n = 5 for female)


###### Case 3 with fixed conditions
fixed.conditionals[3, ]
m <- subset(male, 
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 & 
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 & 
              native_country == 'United-States')
mf <- rbind(m, f)

m.income.table <- table(m$income); m.income.table  # small:large = 6:17 for male
f.income.table <- table(f$income); f.income.table  # small:large = 2:7 for female
# Is there a statistically significant difference?

#### Comparison via Chi-square test
observed <- table(mf$income, mf$sex); observed
chisq.test(observed)  # p-value from Chi-square test discounted
# Chi-square method is a parametric test. When any of the cells in the observed table has
# 5 or less counts, we must resort to non-parametric test: Fisher's Exact test.

#### Comparison via Fisher's Exact test
fisher.test(observed)  # p-value = 1
# No, there is no stat. sig. difference.

m.income.table[2] / sum(m.income.table)  # 73.9% male with over $50K/year salary
f.income.table[2] / sum(f.income.table)  # 77.8% female with over $50K/year salary
# no large noticeable difference between male and female (in fact, slightly
# higher percentage for women)

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 34
quantile(f$age)  # median: 34
mean(m$age)  # mean: 34.4
mean(f$age)  # mean: 34.3
mf <- rbind(m, f)
# independent t-test
leveneTest(test$age ~ test$sex)
t.test(m$age, f$age, var.equal=T)  # no stat. sig. means difference in age
# Whitney-Mann U test (since sample sizes differ)
wilcox.test(m$age, f$age, paired=F)  # no stat. sig. means difference in age

# keep this case (Case 3)


###### Case 4 with fixed variables
fixed.conditionals[4, ]
m <- subset(male, 
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table

m.income.table[2] / sum(m.income.table)  # 33% male with large income
f.income.table[2] / sum(f.income.table)  # 5% female with large income
# big difference between male and female

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 36
quantile(f$age)  # median: 32
mean(m$age)  # mean: 35.8
mean(f$age)  # mean: 33.4
# male population is older
# discount case 

mf <- rbind(m, f)
# independent t-test
leveneTest(mf$age ~ mf$sex)
t.test(m$age, f$age, var.equal=T)  # means age difference almost stat. sig.
# Mann-Whitney U test (since sample sizes differ)
wilcox.test(m$age, f$age, paired=F)  # mean age difference stat. sig.
# definitely discount case


###### Case 5 with fixed conditions
fixed.conditionals[5, ]
m <- subset(male, 
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table

m.income.table[2] / sum(m.income.table)  # 37.5% of male with $50K/year salary
f.income.table[2] / sum(f.income.table)  # 57.1% of female with $50K/year salary
# small sample but more female executives/managers make more than their male counterparts

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 41
quantile(f$age)  # median: 44
mean(m$age)  # mean: 42.6
mean(f$age)  # mean: 44.1
# discount this case (since the differences in age means and medians may account for the 
# large income percentange disparity)


###### Case 6 with fixed conditions
fixed.conditionals[6, ]
m <- subset(male, 
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table

m.income.table[2] / sum(m.income.table)
f.income.table[2] / sum(f.income.table)
# too small sample (n = 2 for male and n = 2 for female)
# discount this case


###### Case 7 with fixed conditions
fixed.conditionals[7, ]
m <- subset(male, 
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table

m.income.table[2] / sum(m.income.table)  # 80.8% of male with $50K/year salary
f.income.table[2] / sum(f.income.table)  # 75% of female with $50K/year salary
# very small sample for female (n = 4)

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 44
quantile(f$age)  # median: 43.5
mean(m$age)  # mean: 43.96
mean(f$age)  # mean: 43.75
mf <- rbind(m, f)
# Whitney-Mann U test (instead of indepedent t-test) since the equal sample sizes assumption 
# was violated
wilcox.test(m$age, f$age, var.equal=T)  # no stat. sig. means difference in age
# possibly, discount this case (since the sample size for female is too small)


###### Case 8 with fixed conditions
fixed.conditionals[8, ]
m <- subset(male, 
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Exec-managerial' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# 0 sample sizes for both groups
# case discounted


###### Case 9 with fixed conditions
fixed.conditionals[9, ]
m <- subset(male, 
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# too small sizes (n = 1 for female)
# case discounted 


###### Case 10 with fixed conditions
fixed.conditionals[10, ]
m <- subset(male, 
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# too small sample sizes (n = 2 for male and n = 1 for female)
# discount case


###### Case 11 with fixed conditions
fixed.conditionals[11, ]
m <- subset(male, 
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 & 
              native_country == 'United-States')
mf <- rbind(m, f)

m.income.table <- table(m$income); m.income.table  # small:large = 7:15 for male
f.income.table <- table(f$income); f.income.table  # small:large = 3:7 for female
# Is there a statistically significant difference in income by gender?

#### Comparison via Chi-square test
observed <- table(mf$income, mf$sex); observed
chisq.test(observed)  # p-value from Chi-square test discounted
# Chi-square method is a parametric test. When any of the cells in the observed table has
# 5 or less counts, we must resort to non-parametric test: Fisher's Exact test.

#### Comparison via Fisher's Exact test
fisher.test(observed)  # p-value = 1
# No, there is no stat. sig. difference.

m.income.table[2] / sum(m.income.table)  # 68% of male whose salary reached $50K/year or more
f.income.table[2] / sum(f.income.table)  # 70% of female whose salary reach $50K/year or more
# no noticeable/large difference

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 33.5
quantile(f$age)  # median: 35
mean(m$age)  # mean: 34.4
mean(f$age)  # mean: 34.8

# independent t-test for difference in means age
leveneTest(test$age ~ test$sex)
t.test(m$age, f$age, var.equal=T)  # no stat. sig. means difference in age
# Mann-Whitney U test for difference in means age (since sample sizes differ)
wilcox.test(m$age, f$age, paired=F)

# keep this case (Case 11)


###### Case 12 with fixed conditions
fixed.conditionals[12, ]
m <- subset(male, 
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 & 
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table

m.income.table[2] / sum(m.income.table)  # 10% of male with salary $50K/year or more
f.income.table[2] / sum(f.income.table)  # 0% of female with sarly $50K/year or more
# One case in male group that creates bias in the percentages

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 32.5
quantile(f$age)  # median: 32.5
mean(m$age)  # mean: 33.7
mean(f$age)  # mean: 32.43
mf <- rbind(m, f)
# independent t-test for means age difference
leveneTest(test$age ~ test$sex)
t.test(m$age, f$age, var.equal=T)  # not stat. sig. means difference in age
# Whitney-Mann U test for means age difference (since sample sizes differ)
wilcox.test(m$age, f$age, paired=F)  # not stat. sig. means difference in age

# discount case since there 10% difference is created by a single case 


###### Case 13 with fixed conditions
fixed.conditionals[13, ]
m <- subset(male, 
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# too small sample sizes (n = 2 for male; n = 1 for female)
# discount case


###### Case 14 with fixed conditions
fixed.conditionals[14, ]
m <- subset(male, 
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# 0 sample sizes for both
# discount case


###### Case 15 with fixed conditions
fixed.conditionals[15, ]
m <- subset(male, 
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# no sample size for female
# discount case 


###### Case 16 with fixed conditions
fixed.conditionals[16, ]
m <- subset(male, 
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Prof-specialty' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# too small sample sizes (n = 4 for male; n = 1 for female)
# discount case


###### Case 17 with fixed conditions
fixed.conditionals[17, ]
m <- subset(male, 
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# small:large = 5:8
# small:large = 2:1

m.income.table[2] / sum(m.income.table)  # 61.5% of male with salary of $50K/year or more
f.income.table[2] / sum(f.income.table)  # 33.3% of female with salary of $50K/year or more
# too small sample size for female (n = 3)
# possibly discount case

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 36 years old
quantile(f$age)  # median: 32 years old
mean(m$age)  # mean: 35.08
mean(f$age)  # mean: 32.67

# discount case


###### Case 18 with fixed conditions
fixed.conditionals[18, ]
m <- subset(male, 
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# small:large = 6:0 for male
# small:large = 12:0 for female

m.income.table[2] / sum(m.income.table)  # 0% of male with $50K/year salary or more
f.income.table[2] / sum(f.income.table)  # 0% of female with $50K/year salary or more
# no noticeable difference

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 31
quantile(f$age)  # median: 35
mean(m$age)  # mean: 32.3
mean(f$age)  # mean: 34.5
mf <- rbind(m, f)
# independent t-test for means age difference
leveneTest(test$age ~ test$sex)
t.test(m$age, f$age, var.equal=T)  # no stat. sig. means difference in age
# Whitney-Mann U test for means age difference (since sample sizes differ)
wilcox.test(m$age, f$age, paired=F)

# median age difference of 4; mean age difference of 2.2
# discount case


###### Case 19 with fixed conditions
fixed.conditionals[19, ]
m <- subset(male, 
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# too small sample for female (n = 1)


###### Case 20 with fixed conditions
fixed.conditionals[20, ]
m <- subset(male, 
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[30,40)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table

m.income.table[2] / sum(m.income.table)  # 40% of male with $50K/year or more salary
f.income.table[2] / sum(f.income.table)  # 10% of female with $50K/year or more salary
# small sample for male but more male with salary equal to or greather than $50K/year
# than their female counterparts

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 31
quantile(f$age)  # median: 33
mean(m$age)  # mean: 31.4
mean(f$age)  # mean: 33.4
# discount case (since the mean/median differences exceed 1)


###### Case 21 with fixed conditions
fixed.conditionals[21, ]
m <- subset(male, 
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
mf <- rbind(m, f)

m.income.table <- table(m$income); m.income.table  # small:large = 9:5 for male
f.income.table <- table(f$income); f.income.table  # small:large = 2:4 for female
# Is there a statistically significant difference in income by gender?

#### Comparison via Chi-square test
observed <- table(mf$income, mf$sex); observed
chisq.test(observed)  # p-value from Chi-square test discounted
# Chi-square method is a parametric test. When any of the cells in the observed table has
# 5 or less counts, we must resort to non-parametric test: Fisher's Exact test.

#### Comparison via Fisher's Exact test
fisher.test(observed)  # p-value ~= 0.34
# No, there is no stat. sig. difference.
# Or, at least, any difference is undetectable from this small sample sizes.

m.income.table[2] / sum(m.income.table)  # 35.7% male with $50K/year or higher salary
f.income.table[2] / sum(f.income.table)  # 66.7% female with $50K/year or higher salary
# small female sample size (n = 6) but more female executives/managers make more than their 
# male counterparts

### Making sure that the two groups (male and female) have comparatively are the same age
quantile(m$age)  # median: 44 
quantile(f$age)  # median: 44
mean(m$age)  # mean: 44.21 
mean(f$age)  # mean: 43.67

# independent t-test
leveneTest(test$age ~ test$sex)
t.test(m$age, f$age, var.equal=T)  # no stat. sig. means difference in age
# Whitney-Mann U test (since sample sizes differ)

# keep this case (Case 21)


###### Case 22 with fixed conditions
fixed.conditionals[22, ]
m <- subset(male, 
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'HS-grad' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# too small sample size for male (n = 1)
# discount case


###### Case 23 with fixed conditions
fixed.conditionals[23, ]
m <- subset(male, 
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Married-civ-spouse' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# 0 sample size for female (while n = 11 for male)
# discount case


###### Case 24 with fixed conditions
fixed.conditionals[24, ]
m <- subset(male, 
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')
f <- subset(female,
            occupation == 'Sales' & 
              workclass == 'Private' &
              age_group == '[40,50)' &
              education == 'Bachelors' &
              marital_status == 'Never-married' &
              race == 'White' &
              hours_per_week == 40 &
              native_country == 'United-States')

m.income.table <- table(m$income); m.income.table
f.income.table <- table(f$income); f.income.table
# 0 sample size for female
# discount case

## Cases 1, 3, 11, and 21 suggest that there aren't necessarily more men (percentage-wise)
## with salary greater than or equal to $50K/year than women, when we control for age 
## groups, work-class, education, marital status, occupation type, race, gender, number of
## work hours per week, and native country. In fact, in case 21, there were more
## women (percentage-wise) making greater than or equal to $50K/year than there were men,
## though the difference wasn't statistically significant.


###### So why do fewer women make over $50K/year? ######
male.income.table  
female.income.table

male.income.table[2] / sum(male.income.table)  # 31% of male with $50K/year or more
female.income.table[2] / sum(female.income.table)  # 11.4% of female with $50K/year or more

###### Is there any age discrepancy between men and women in the data, which may account for 
###### the income count difference?
m.age.mean <- round(mean(male$age), 1); m.age.mean
f.age.mean <- round(mean(female$age), 1); f.age.mean
m.age.median <- median(male$age); m.age.median
f.age.median <- median(female$age); f.age.median

ggplot(data, aes(x=age, fill=sex)) + 
  geom_histogram(position='stack', binwidth=1)
ggplot(data, aes(x=age, fill=sex)) + 
  geom_histogram(position='fill', binwidth=1)
ggplot(data, aes(x=age, fill=sex)) + 
  geom_histogram(position='dodge', binwidth=1) + 
  labs(title='Histograms of Male and Female Individuals\' Age')
dev.copy(png, 'images/histograms_of_male_and_female_individuals_1.png')
dev.off()

m.age.hist <- ggplot(male, aes(x=age)) + 
  geom_histogram(fill='#00BFC4', binwidth=1) + 
  geom_segment(x=m.age.mean, y=0, xend=m.age.mean, yend=350, size=1, color='blue') +
  geom_segment(x=m.age.median, y=0, xend=m.age.median, yend=350, size=1, color='yellow') +
  annotate('text', x=75, y=200, label=paste('mean:', m.age.mean, '\n', 'median:', m.age.median)) + 
  labs(title='Histogram of Male Individuals\' Age')
m.age.hist

f.age.hist <- ggplot(female, aes(x=age)) + 
  geom_histogram(fill='#F8766D', binwidth=1) + 
  geom_segment(x=f.age.mean, y=0, xend=f.age.mean, yend=350, size=1, color='blue') +
  geom_segment(x=f.age.median, y=0, xend=f.age.median, yend=350, size=1, color='yellow') +
  annotate('text', x=75, y=200, label=paste('mean:', f.age.mean, '\n', 'median:', f.age.median)) + 
  labs(title='Histogram of Female Individuals\' Age')
f.age.hist

grid.arrange(m.age.hist, f.age.hist)
dev.copy(png, 'images/histograms_of_male_and_female_individuals_age_2.png')
dev.off()

#### Is the mean age difference stat. sig.?
m.age.mean
f.age.mean

#### Conduct an indepedent t-test
## Three assumptions of independent t-tests
## 1. Roughly the same sample sizes (In this case, it is.)
## 2. Normal distribution of samples compared
## 3. Homogeneity of variance

## checking for normal distributions
hist(male$age)  # distribution roughly normal
hist(female$age)  # distribution roughly normal
describe(male$age)  # skew < 3; kurtosis < 10; "normal enough"
describe(female$age)  # skew < 3; kurtosis < 10; "normal enough"

## checking for homogeneity of variance
leveneTest(data$age ~ data$sex)  
# homogeneity of variance assumption violated; resort to Whitney-Mann U test (non-parametric)

## Whitney-Mann U test 
wilcox.test(male$age, female$age, paired=F)  # p-value << 0.05
# age difference between male and female stat. sig. 

# Yes, male are older than women in the sample and the difference is stat. sig., which may
# account for the fact that there are also more men with $50K/year or higher salary than 
# women.


###### Histograms of age by income and gender
female.large <- subset(female, income=='large')
female.small <- subset(female, income=='small')
male.large <- subset(male, income=='large')
male.small <- subset(male, income=='small')

ggplot(female, aes(x=age, fill=income)) + 
  geom_histogram()
ggplot(female, aes(x=age_group, fill=income)) + 
  geom_histogram()

f.l.age.mean <- round(mean(female.large$age), 1)
f.l.age.median <- median(female.large$age)
f.s.age.mean <- round(mean(female.small$age), 1)
f.s.age.median <- median(female.large$age)

m.l.age.mean <- round(mean(male.large$age), 1)
m.l.age.median <- median(male.large$age)
m.s.age.mean <- round(mean(male.small$age), 1)
m.s.age.median <- median(male.large$age)

# histogram of female who earn more than $50K/year
a <- ggplot(female.large, aes(x=age)) + 
  geom_histogram(fill='#F8766D') + 
  geom_segment(x=f.l.age.mean, y=0, xend=f.l.age.mean, yend=150, size=1, color='blue') + 
  geom_segment(x=f.l.age.median, y=0, xend=f.l.age.median, yend=150, size=1, color='yellow') +
  annotate('text', x=80, y=100, label=paste('mean:', f.l.age.mean, '\n', 'median:', f.l.age.median)) +
  labs(title='Histogram of Female with \n Income Greater than $50K/year')
a

# histogram of female who earn less than $50K/year
b <- ggplot(female.small, aes(x=age)) + 
  geom_histogram(fill='#F8766D') + 
  geom_segment(x=f.s.age.mean, y=0, xend=f.s.age.mean, yend=1000, size=1, color='blue') +
  geom_segment(x=f.s.age.median, y=0, xend=f.s.age.median, yend=1000, size=1, color='yellow') +
  annotate('text', x=70, y=600, label=paste('mean:', f.s.age.mean, '\n', 'median:', f.s.age.median)) +
  labs(title='Histogram of Female with \n Income Less than $50K/year')
b

# histogram of male who earn more than $50K/year
c <- ggplot(male.large, aes(x=age)) + 
  geom_histogram(fill='#00BFC4') + 
  geom_segment(x=m.l.age.mean, y=0, xend=m.l.age.mean, yend=700, size=1, color='blue') +
  geom_segment(x=m.l.age.median, y=0, xend=m.l.age.median, yend=700, size=1, color='yellow') +
  annotate('text', x=70, y=150, label=paste('mean:', m.l.age.mean, '\n', 'median:', m.l.age.median)) +
  labs(title='Histogram of Male with \n Income Greater than $50K/year')
c

# histogram of male who earn less than $50K/year
d <- ggplot(male.small, aes(x=age)) + 
  geom_histogram(fill='#00BFC4') + 
  geom_segment(x=m.s.age.mean, y=0, xend=m.s.age.mean, yend=1200, size=1, color='blue') +
  geom_segment(x=m.s.age.median, y=0, xend=m.s.age.median, yend=1200, size=1, color='yellow') +
  annotate('text', x=70, y=400, label=paste('mean:', m.s.age.mean, '\n', 'median:', m.s.age.median)) +
  labs(title='Histogram of Male with \n Income Less than $50K/year')
d

grid.arrange(a, b, c, d)
dev.copy(png, 'images/histograms_of_age_by_gender_and_income.png')
dev.off()


###### Do fewer women (compared to men) make $50K/year or more because of the TYPES of 
###### occupation that they choose?

ggplot(data, aes(x=occupation, fill=sex)) +
  geom_bar() + 
  labs(title='Individuals by Occupation Type and Gender') + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) 
dev.copy(png, 'images/individuals_by_occupation_type_and_gender.png')
dev.off()

#### Conduct Chi-square test to see if the income variable (DV) is swayed by the 
#### occupation variable (IV). (By common sense, we already know that it is.)
observed <- table(data$income, data$occupation); observed

# Deselect individ'Armed-Forces' and 'Priv-house-serv'
selectCond <- !(data$occupation %in% c('Armed-Forces', 'Priv-house-serv'))
occupation <- data[selectCond, ]

# Getting rid of unused factor levels
head(occupation$occupation)  # 14 levels with 2 unused
occupation$occupation <- as.factor(as.character(occupation$occupation))
head(occupation$occupation)  # 12 levels

observed <- table(occupation$income, occupation$occupation); observed
chisq.test(observed)  
# p-value << 0.05
# Yes, there is a stat. sig. sway on the income variable by the outcome variable.

#### Examine occupation types by the percentages of individuals who make $50K/year or more.
#### Skip 'Armed-Forces' 
#### Skip 'Priv-house-serv'

#### 'Adm-clerical' 
### subsetting 
adm.clerical.40hpw <- subset(data, occupation=='Adm-clerical' & hours_per_week==40)

### initial peek
ggplot(adm.clerical.40hpw, aes(x=income, fill=sex)) + 
  geom_bar() + 
  labs(title='Individuals by Income and Gender') 
dev.copy(png, 'images/individuals_by_income_and_gender.png')
dev.off()

### income-gender table creation
adm.clerical.40hpw.incomeGenderT <- table(adm.clerical.40hpw$income, adm.clerical.40hpw$sex)
adm.clerical.40hpw.incomeGenderT

### female
adm.clerical.40hpw.incomeGenderT[ , 1]  
adm.clerical.40hpw.incomeGenderT[ , 1] / sum(adm.clerical.40hpw.incomeGenderT[ , 1])  
# 91.6% under $50K/year

### male
adm.clerical.40hpw.incomeGenderT[ , 2]  
adm.clerical.40hpw.incomeGenderT[ , 2] / sum(adm.clerical.40hpw.incomeGenderT[ , 2])
# 75.2% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
adm.clerical.40hpw.incomeGenderT
chisq.test(adm.clerical.40hpw.incomeGenderT)  # diff. stat. sig.
cramersV(adm.clerical.40hpw.incomeGenderT)  # Cramer's V effect size: 0.2

##### 'Craft-repair' 
### subsetting and table creation
craft.repair.40hpw <- subset(data, occupation=='Craft-repair' & hours_per_week==40)
craft.repair.40hpw.incomeGenderT <- table(craft.repair.40hpw$income, craft.repair.40hpw$sex)
craft.repair.40hpw.incomeGenderT

### female
craft.repair.40hpw.incomeGenderT[ , 1]
craft.repair.40hpw.incomeGenderT[ , 1] / sum(craft.repair.40hpw.incomeGenderT[ , 1])  
# 91.2% under $50K/year

### male
craft.repair.40hpw.incomeGenderT[ , 2]
craft.repair.40hpw.incomeGenderT[ , 2] / sum(craft.repair.40hpw.incomeGenderT[ , 2])  
# 79% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
craft.repair.40hpw.incomeGenderT
chisq.test(craft.repair.40hpw.incomeGenderT)  # diff. stat. sig.
cramersV(craft.repair.40hpw.incomeGenderT)  # Cramer's V effect size: 0.09

##### 'Exec-managerial'
exec.managerial.40hpw <- subset(data, occupation=='Exec-managerial' & hours_per_week==40)
exec.managerial.40hpw.incomeGenderT <- table(exec.managerial.40hpw$income, exec.managerial.40hpw$sex)
exec.managerial.40hpw.incomeGenderT 

### female
exec.managerial.40hpw.incomeGenderT[ , 1]
exec.managerial.40hpw.incomeGenderT[ , 1] / sum(exec.managerial.40hpw.incomeGenderT[ , 1])  
# 81% under $50K/year

### male
exec.managerial.40hpw.incomeGenderT[ , 2]
exec.managerial.40hpw.incomeGenderT[ , 2] / sum(exec.managerial.40hpw.incomeGenderT[ , 2])  
# 45% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
exec.managerial.40hpw.incomeGenderT
chisq.test(exec.managerial.40hpw.incomeGenderT)  # diff. stat. sig.
cramersV(exec.managerial.40hpw.incomeGenderT)  # Cramer's V effect size: 0.37

##### 'Farming-fishing'
farming.fishing.40hpw <- subset(data, occupation=='Farming-fishing' & hours_per_week==40)
farming.fishing.40hpw.incomeGenderT <- table(farming.fishing.40hpw$income, farming.fishing.40hpw$sex)
farming.fishing.40hpw.incomeGenderT

### female
farming.fishing.40hpw.incomeGenderT[ , 1]
farming.fishing.40hpw.incomeGenderT[ , 1] / sum(farming.fishing.40hpw.incomeGenderT[ , 1])  
# 100% under $50K/year

### male
farming.fishing.40hpw.incomeGenderT[ , 2]
farming.fishing.40hpw.incomeGenderT[ , 2] / sum(farming.fishing.40hpw.incomeGenderT[ , 2])  
# 91.7% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
farming.fishing.40hpw.incomeGenderT  # Too low cell count (5 or less)

### Fisher's Exact test
fisher.test(farming.fishing.40hpw.incomeGenderT)  # diff. not stat. sig.

##### 'Handlers-cleaners'
handlers.cleaners.40hpw <- subset(data, occupation=='Handlers-cleaners' & hours_per_week==40)
handlers.cleaners.40hpw.incomeGenderT <- table(handlers.cleaners.40hpw$income, handlers.cleaners.40hpw$sex)
handlers.cleaners.40hpw.incomeGenderT

### female
handlers.cleaners.40hpw.incomeGenderT[ , 1]
handlers.cleaners.40hpw.incomeGenderT[ , 1] / sum(handlers.cleaners.40hpw.incomeGenderT[ , 1])  
# 96.5% under $50K/year

### male
handlers.cleaners.40hpw.incomeGenderT[ , 2]
handlers.cleaners.40hpw.incomeGenderT[ , 2] / sum(handlers.cleaners.40hpw.incomeGenderT[ , 2])  
# 90.5% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
handlers.cleaners.40hpw.incomeGenderT  # too low cell count (5 or less) in one of the cells

### Fisher's Exact test to see if income difference by gender is stat. sig.
fisher.test(handlers.cleaners.40hpw.incomeGenderT)  # diff. not stat. sig.

##### 'Machine-op-inspct'
machine.op.inspct.40hpw <- subset(data, occupation=='Machine-op-inspct' & hours_per_week==40)
machine.op.inspct.40hpw.incomeGenderT <- table(machine.op.inspct.40hpw$income, machine.op.inspct.40hpw$sex) 
machine.op.inspct.40hpw.incomeGenderT

### female
machine.op.inspct.40hpw.incomeGenderT[ , 1]
machine.op.inspct.40hpw.incomeGenderT[ , 1] / sum(machine.op.inspct.40hpw.incomeGenderT[ , 1])  
# 96.2% under $50K/year

### male
machine.op.inspct.40hpw.incomeGenderT[ , 2]
machine.op.inspct.40hpw.incomeGenderT[ , 2] / sum(machine.op.inspct.40hpw.incomeGenderT[ , 2])  
# 84.8% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
machine.op.inspct.40hpw.incomeGenderT
chisq.test(machine.op.inspct.40hpw.incomeGenderT)  # diff. stat. sig.
cramersV(machine.op.inspct.40hpw.incomeGenderT)  # Cramer's V effect size: 0.18

##### 'Other-service'
other.service.40hpw <- subset(data, occupation=='Other-service' & hours_per_week==40)
other.service.40hpw.incomeGenderT <- table(other.service.40hpw$income, other.service.40hpw$sex)
other.service.40hpw.incomeGenderT

### female
other.service.40hpw.incomeGenderT[ , 1]
other.service.40hpw.incomeGenderT[ , 1] / sum(other.service.40hpw.incomeGenderT[ , 1])  
# 97% under $50K/year

### male
other.service.40hpw.incomeGenderT[ , 2]
other.service.40hpw.incomeGenderT[ , 2] / sum(other.service.40hpw.incomeGenderT[ , 2])  
# 94.2% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
other.service.40hpw.incomeGenderT
chisq.test(other.service.40hpw.incomeGenderT)  # p-value very close to stat. sig.
cramersV(other.service.40hpw.incomeGenderT)  # Cramer's V effect size: 0.06

##### 'Prof-specialty'
prof.specialty.40hpw <- subset(data, occupation=='Prof-specialty' & hours_per_week==40)
prof.specialty.40hpw.incomeGenderT <- table(prof.specialty.40hpw$income, prof.specialty.40hpw$sex)
prof.specialty.40hpw.incomeGenderT

### female
prof.specialty.40hpw.incomeGenderT[ , 1]
prof.specialty.40hpw.incomeGenderT[ , 1] / sum(prof.specialty.40hpw.incomeGenderT[ , 1])  
# 78.3% under $50K/year

### male
prof.specialty.40hpw.incomeGenderT[ , 2]
prof.specialty.40hpw.incomeGenderT[ , 2] / sum(prof.specialty.40hpw.incomeGenderT[ , 2])  
# 47.6% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
prof.specialty.40hpw.incomeGenderT
chisq.test(prof.specialty.40hpw.incomeGenderT)  # diff. stat. sig.
cramersV(prof.specialty.40hpw.incomeGenderT)  # Cramer's V effect size

##### 'Protective-serv'
protective.serv.40hpw <- subset(data, occupation=='Protective-serv' & hours_per_week==40)
protective.serv.40hpw.incomeGenderT <- table(protective.serv.40hpw$income, protective.serv.40hpw$sex)
protective.serv.40hpw.incomeGenderT

### female
protective.serv.40hpw.incomeGenderT[ , 1]
protective.serv.40hpw.incomeGenderT[ , 1] / sum(protective.serv.40hpw.incomeGenderT[ , 1])  
# 88.5% under $50K/year

### male
protective.serv.40hpw.incomeGenderT[ , 2]
protective.serv.40hpw.incomeGenderT[ , 2] / sum(protective.serv.40hpw.incomeGenderT[ , 2])  
# 67.3% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
protective.serv.40hpw.incomeGenderT
chisq.test(protective.serv.40hpw.incomeGenderT)  # diff. stat. sig.
cramersV(protective.serv.40hpw.incomeGenderT)  # Cramer's V effect size: 0.196

##### 'Sales'
sales.40hpw <- subset(data, occupation=='Sales' & hours_per_week==40)
sales.40hpw.incomeGenderT <- table(sales.40hpw$income, sales.40hpw$sex)
sales.40hpw.incomeGenderT

### female
sales.40hpw.incomeGenderT[ , 1]
sales.40hpw.incomeGenderT[ , 1] / sum(sales.40hpw.incomeGenderT[ , 1])  
# 93% under $50K/year

### male
sales.40hpw.incomeGenderT[ , 2]
sales.40hpw.incomeGenderT[ , 2] / sum(sales.40hpw.incomeGenderT[ , 2])  
# 62.9% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
sales.40hpw.incomeGenderT
chisq.test(sales.40hpw.incomeGenderT)  # diff. stat. sig.
cramersV(sales.40hpw.incomeGenderT)  # Cramer's V effect size: 0.364

##### 'Tech-support'
tech.support.40hpw <- subset(data, occupation=='Tech-support' & hours_per_week==40)
tech.support.40hpw.incomeGenderT <- table(tech.support.40hpw$income, tech.support.40hpw$sex)
tech.support.40hpw.incomeGenderT

### female
tech.support.40hpw.incomeGenderT[ , 1]
tech.support.40hpw.incomeGenderT[ , 1] / sum(tech.support.40hpw.incomeGenderT[ , 1])  
# 89.1% under $50K/year

### male 
tech.support.40hpw.incomeGenderT[ , 2]
tech.support.40hpw.incomeGenderT[ , 2] / sum(tech.support.40hpw.incomeGenderT[ , 2])  
# 61.2% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
tech.support.40hpw.incomeGenderT
chisq.test(tech.support.40hpw.incomeGenderT)  # diff. stat. sig.
cramersV(tech.support.40hpw.incomeGenderT)  # Cramer's V effect size: 0.31

##### 'Transport-moving'
transport.moving.40hpw <- subset(data, occupation=='Transport-moving' & hours_per_week==40)
transport.moving.40hpw.incomeGenderT <- table(transport.moving.40hpw$income, transport.moving.40hpw$sex)
transport.moving.40hpw.incomeGenderT

### female 
transport.moving.40hpw.incomeGenderT[ , 1]
transport.moving.40hpw.incomeGenderT[ , 1] / sum(transport.moving.40hpw.incomeGenderT[ , 1])
# 88.4% under $50K/year

### male
transport.moving.40hpw.incomeGenderT[ , 2]
transport.moving.40hpw.incomeGenderT[ , 2] / sum(transport.moving.40hpw.incomeGenderT[ , 2])
# 84% under $50K/year

### Chi-square test to see if income difference by gender is stat. sig.
transport.moving.40hpw.incomeGenderT  # too low cell count (5 or less) in one of the cells

### Fisher's Exact test to see if income difference by gender is stat. sig.
fisher.test(transport.moving.40hpw.incomeGenderT)  # diff. not stat. sig.


###### So there are stat. sig. differences between male and female among individuals who 
###### work 40 hours per week in the same occupation fields. Why?
###### We saw earlier that the dataset contained men who were collectively 
###### older than women. Perhaps we should compare men and women in the same age groups.

#### 'Adm-clerical' 
ggplot(adm.clerical.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')

### subsetting individuals in 20s, 30s, and 40s
adm.clerical.40hpw.20s <- subset(adm.clerical.40hpw, age_group=='[20,30)')
adm.clerical.40hpw.30s <- subset(adm.clerical.40hpw, age_group=='[30,40)')
adm.clerical.40hpw.40s <- subset(adm.clerical.40hpw, age_group=='[40,50)')

### income-gender table creation
adm.clerical.40hpw.20s.incomeGenderT <- table(adm.clerical.40hpw.20s$income, 
                                              adm.clerical.40hpw.20s$sex)
adm.clerical.40hpw.30s.incomeGenderT <- table(adm.clerical.40hpw.30s$income, 
                                              adm.clerical.40hpw.30s$sex)
adm.clerical.40hpw.40s.incomeGenderT <- table(adm.clerical.40hpw.40s$income, 
                                              adm.clerical.40hpw.40s$sex)
adm.clerical.40hpw.20s.incomeGenderT
adm.clerical.40hpw.30s.incomeGenderT
adm.clerical.40hpw.40s.incomeGenderT

### comparing individuals in their 20s
## female (97.1% make less than $50K/year)
adm.clerical.40hpw.20s.incomeGenderT[ , 1]
adm.clerical.40hpw.20s.incomeGenderT[ , 1] / sum(adm.clerical.40hpw.20s.incomeGenderT[ , 1])  
## male (94.7% make less than $50K/year)
adm.clerical.40hpw.20s.incomeGenderT[ , 2]
adm.clerical.40hpw.20s.incomeGenderT[ , 2] / sum(adm.clerical.40hpw.20s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
adm.clerical.40hpw.20s.incomeGenderT
fisher.test(adm.clerical.40hpw.20s.incomeGenderT)  # diff. not stat. sig.

### comparing individuals in their 30s
## female (90% make less than $50K/year)
adm.clerical.40hpw.30s.incomeGenderT[ , 1]
adm.clerical.40hpw.30s.incomeGenderT[ , 1] / sum(adm.clerical.40hpw.30s.incomeGenderT[ , 1])   
## male (74% make less than $50K/year)
adm.clerical.40hpw.30s.incomeGenderT[ , 2]
adm.clerical.40hpw.30s.incomeGenderT[ , 2] / sum(adm.clerical.40hpw.30s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
adm.clerical.40hpw.30s.incomeGenderT
chisq.test(adm.clerical.40hpw.30s.incomeGenderT)  # diff. stat. sig.
cramersV(adm.clerical.40hpw.incomeGenderT)  # Cramer's V effect size: 0.2

### comparing individuals in their 40s
## female (85.8% make less than $50K/year)
adm.clerical.40hpw.40s.incomeGenderT[ , 1]
adm.clerical.40hpw.40s.incomeGenderT[ , 1] / sum(adm.clerical.40hpw.40s.incomeGenderT[ , 1])  
## male (64.2% make less than $50K/year)
adm.clerical.40hpw.40s.incomeGenderT[ , 2]
adm.clerical.40hpw.40s.incomeGenderT[ , 2] / sum(adm.clerical.40hpw.40s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
adm.clerical.40hpw.40s.incomeGenderT
chisq.test(adm.clerical.40hpw.40s.incomeGenderT)  # diff. stat. sig.
cramersV(adm.clerical.40hpw.40s.incomeGenderT)  # Cramer's V effect size: 0.216


##### 'Craft-repair' 
ggplot(craft.repair.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')

### subsetting 30s and 40s age groups
craft.repair.40hpw.30s <- subset(craft.repair.40hpw, age_group=='[30,40)')
craft.repair.40hpw.40s <- subset(craft.repair.40hpw, age_group=='[40,50)')

### creating income-gender tables
craft.repair.40hpw.30s.incomeGenderT <- table(craft.repair.40hpw.30s$income, 
                                              craft.repair.40hpw.30s$sex)
craft.repair.40hpw.40s.incomeGenderT <- table(craft.repair.40hpw.40s$income,
                                              craft.repair.40hpw.40s$sex)
craft.repair.40hpw.30s.incomeGenderT
craft.repair.40hpw.40s.incomeGenderT

### comparing individuals in their 30s
## female (91.2% make less than $50K/year)
craft.repair.40hpw.30s.incomeGenderT[ , 1]
craft.repair.40hpw.30s.incomeGenderT[ , 1] / sum(craft.repair.40hpw.30s.incomeGenderT[ , 1])  
## male (81.9% make less than $50K/year)
craft.repair.40hpw.30s.incomeGenderT[ , 2]
craft.repair.40hpw.30s.incomeGenderT[ , 2] / sum(craft.repair.40hpw.30s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
craft.repair.40hpw.30s.incomeGenderT
fisher.test(craft.repair.40hpw.30s.incomeGenderT)  # diff. not stat. sig.

### comparing individuals in their 40s
## female (88.9% make less than $50K/year)
craft.repair.40hpw.40s.incomeGenderT[ , 1]
craft.repair.40hpw.40s.incomeGenderT[ , 1] / sum(craft.repair.40hpw.40s.incomeGenderT[ , 1])  
## male (70.9% make less than $50K/year)
craft.repair.40hpw.40s.incomeGenderT[ , 2]
craft.repair.40hpw.40s.incomeGenderT[ , 2] / sum(craft.repair.40hpw.40s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
craft.repair.40hpw.40s.incomeGenderT
fisher.test(craft.repair.40hpw.40s.incomeGenderT)  # diff. stat. sig.

##### 'Exec-managerial'
ggplot(exec.managerial.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')

### subsetting 30s and 40s age groups
exec.managerial.40hpw.30s <- subset(exec.managerial.40hpw, age_group=='[30,40)')
exec.managerial.40hpw.40s <- subset(exec.managerial.40hpw, age_group=='[40,50)')

### creating gender-income tables
exec.managerial.40hpw.30s.incomeGenderT <- table(exec.managerial.40hpw.30s$income, 
                                                 exec.managerial.40hpw.30s$sex)
exec.managerial.40hpw.40s.incomeGenderT <- table(exec.managerial.40hpw.40s$income, 
                                                 exec.managerial.40hpw.40s$sex)
exec.managerial.40hpw.30s.incomeGenderT
exec.managerial.40hpw.40s.incomeGenderT 

### comparing individuals in their 30s
## female (77.4% make less than $50K/year)
exec.managerial.40hpw.30s.incomeGenderT[ , 1]
exec.managerial.40hpw.30s.incomeGenderT[ , 1] / sum(exec.managerial.40hpw.30s.incomeGenderT[ , 1])  
## male (51.1% make less than $50K/year)
exec.managerial.40hpw.30s.incomeGenderT[ , 2]
exec.managerial.40hpw.30s.incomeGenderT[ , 2] / sum(exec.managerial.40hpw.30s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
exec.managerial.40hpw.30s.incomeGenderT
chisq.test(exec.managerial.40hpw.30s.incomeGenderT)  # diff. stat. sig.
cramersV(exec.managerial.40hpw.30s.incomeGenderT)  # Cramer's V effect size: 0.267

### comparing individuals in their 40s
## female (75.6% make less than $50K/year)
exec.managerial.40hpw.40s.incomeGenderT[ , 1]
exec.managerial.40hpw.40s.incomeGenderT[ , 1] / sum(exec.managerial.40hpw.40s.incomeGenderT[ , 1])  
## male (34.4% make less than $50K/year)
exec.managerial.40hpw.40s.incomeGenderT[ , 2]
exec.managerial.40hpw.40s.incomeGenderT[ , 2] / sum(exec.managerial.40hpw.40s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
exec.managerial.40hpw.40s.incomeGenderT
chisq.test(exec.managerial.40hpw.40s.incomeGenderT)  # diff. stat. sig.
cramersV(exec.managerial.40hpw.40s.incomeGenderT)  # Cramer's V effect size: 0.408


##### 'Farming-fishing'
ggplot(farming.fishing.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')
# skip this occupation category since there are too few female samples


##### 'Handlers-cleaners'
ggplot(handlers.cleaners.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')

### subsetting 30s age group
handlers.cleaners.40hpw.30s <- subset(handlers.cleaners.40hpw, age_group=='[30,40)')

### creating gender-income tables
handlers.cleaners.40hpw.30s.incomeGenderT <- table(handlers.cleaners.40hpw.30s$income, 
                                                   handlers.cleaners.40hpw.30s$sex)
handlers.cleaners.40hpw.30s.incomeGenderT

### comparing individuals in their 30s
## female (92% make less than $50K/year)
handlers.cleaners.40hpw.30s.incomeGenderT[ , 1]
handlers.cleaners.40hpw.30s.incomeGenderT[ , 1] / sum(handlers.cleaners.40hpw.30s.incomeGenderT[ , 1])  
## male (91.9% make less than $50K/year)
handlers.cleaners.40hpw.30s.incomeGenderT[ , 2]
handlers.cleaners.40hpw.30s.incomeGenderT[ , 2] / sum(handlers.cleaners.40hpw.30s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
handlers.cleaners.40hpw.30s.incomeGenderT
fisher.test(handlers.cleaners.40hpw.30s.incomeGenderT)  # diff. not stat. sig.


##### 'Machine-op-inspct'
ggplot(machine.op.inspct.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')

### subsetting for 20s, 30s, and 40s age groups
machine.op.inspct.40hpw.20s <- subset(machine.op.inspct.40hpw, age_group=='[20,30)')
machine.op.inspct.40hpw.30s <- subset(machine.op.inspct.40hpw, age_group=='[30,40)')
machine.op.inspct.40hpw.40s <- subset(machine.op.inspct.40hpw, age_group=='[40,50)')

### creating gender-income tables
machine.op.inspct.40hpw.20s.incomeGenderT <- table(machine.op.inspct.40hpw.20s$income, 
                                                   machine.op.inspct.40hpw.20s$sex) 
machine.op.inspct.40hpw.30s.incomeGenderT <- table(machine.op.inspct.40hpw.30s$income, 
                                                   machine.op.inspct.40hpw.30s$sex) 
machine.op.inspct.40hpw.40s.incomeGenderT <- table(machine.op.inspct.40hpw.40s$income, 
                                                   machine.op.inspct.40hpw.40s$sex) 
machine.op.inspct.40hpw.20s.incomeGenderT
machine.op.inspct.40hpw.30s.incomeGenderT
machine.op.inspct.40hpw.40s.incomeGenderT

### comparing individuals in their 20s
## female  (99.1% make less than $50K/year)
machine.op.inspct.40hpw.20s.incomeGenderT[ , 1]
machine.op.inspct.40hpw.20s.incomeGenderT[ , 1] / sum(machine.op.inspct.40hpw.20s.incomeGenderT[ , 1])  
## male  (95.8% make less than $50K/year)
machine.op.inspct.40hpw.20s.incomeGenderT[ , 2]
machine.op.inspct.40hpw.20s.incomeGenderT[ , 2] / sum(machine.op.inspct.40hpw.20s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
machine.op.inspct.40hpw.20s.incomeGenderT
fisher.test(machine.op.inspct.40hpw.20s.incomeGenderT)  # diff. not stat. sig.

### comparing individuals in their 30s
## female (96.4% making less than $50K/year)
machine.op.inspct.40hpw.30s.incomeGenderT[ , 1]
machine.op.inspct.40hpw.30s.incomeGenderT[ , 1] / sum(machine.op.inspct.40hpw.30s.incomeGenderT[ , 1])  
## male (82.7% making less than $50K/year)
machine.op.inspct.40hpw.30s.incomeGenderT[ , 2]
machine.op.inspct.40hpw.30s.incomeGenderT[ , 2] / sum(machine.op.inspct.40hpw.30s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
machine.op.inspct.40hpw.30s.incomeGenderT
fisher.test(machine.op.inspct.40hpw.30s.incomeGenderT)  # diff. stat. sig.

### comparing individuals in their 40s
## female (92.6% making less than $50K/year)
machine.op.inspct.40hpw.40s.incomeGenderT[ , 1]
machine.op.inspct.40hpw.40s.incomeGenderT[ , 1] / sum(machine.op.inspct.40hpw.40s.incomeGenderT[ , 1])  
## male (81.7% making less than $50K/year)
machine.op.inspct.40hpw.40s.incomeGenderT[ , 2]
machine.op.inspct.40hpw.40s.incomeGenderT[ , 2] / sum(machine.op.inspct.40hpw.40s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
machine.op.inspct.40hpw.40s.incomeGenderT
chisq.test(machine.op.inspct.40hpw.40s.incomeGenderT)  # diff. stat. sig.
cramersV(machine.op.inspct.40hpw.40s.incomeGenderT)  # Cramer's V effect size: 0.144


##### 'Other-service'
ggplot(other.service.40hpw, aes(x=age_group, fill=sex)) +
  geom_histogram(position='dodge')

### subsetting 20s, 30s, and 40s age groups
other.service.40hpw.20s <- subset(other.service.40hpw, age_group=='[20,30)')
other.service.40hpw.30s <- subset(other.service.40hpw, age_group=='[30,40)')
other.service.40hpw.40s <- subset(other.service.40hpw, age_group=='[40,50)')

### creating gender-income tables
other.service.40hpw.20s.incomeGenderT <- table(other.service.40hpw.20s$income, 
                                               other.service.40hpw.20s$sex)
other.service.40hpw.30s.incomeGenderT <- table(other.service.40hpw.30s$income, 
                                               other.service.40hpw.30s$sex)
other.service.40hpw.40s.incomeGenderT <- table(other.service.40hpw.40s$income, 
                                               other.service.40hpw.40s$sex)
other.service.40hpw.20s.incomeGenderT
other.service.40hpw.30s.incomeGenderT
other.service.40hpw.40s.incomeGenderT

### comparing individuals in their 20s
## female (97.6% making less than $50K/year)
other.service.40hpw.20s.incomeGenderT[ , 1]
other.service.40hpw.20s.incomeGenderT[ , 1] / sum(other.service.40hpw.20s.incomeGenderT[ , 1])  
## male (96.7% making less than $50K/year)
other.service.40hpw.20s.incomeGenderT[ , 2]
other.service.40hpw.20s.incomeGenderT[ , 2] / sum(other.service.40hpw.20s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
other.service.40hpw.20s.incomeGenderT
fisher.test(other.service.40hpw.20s.incomeGenderT)  # diff. not stat. sig.

### comparing individuals in their 30s
## female (96% making less than $50K/year)
other.service.40hpw.30s.incomeGenderT[ , 1]
other.service.40hpw.30s.incomeGenderT[ , 1] / sum(other.service.40hpw.30s.incomeGenderT[ , 1])  
## male (94.3% making less than $50K/year)
other.service.40hpw.30s.incomeGenderT[ , 2]
other.service.40hpw.30s.incomeGenderT[ , 2] / sum(other.service.40hpw.30s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
other.service.40hpw.30s.incomeGenderT
fisher.test(other.service.40hpw.30s.incomeGenderT)  # diff. not stat. sig.

### comparing individuals in their 40s
## female (97.4% making less than $50K/year)
other.service.40hpw.40s.incomeGenderT[ , 1]
other.service.40hpw.40s.incomeGenderT[ , 1] / sum(other.service.40hpw.40s.incomeGenderT[ , 1])  
## male (90.9% making less than $50K/year)
other.service.40hpw.40s.incomeGenderT[ , 2]
other.service.40hpw.40s.incomeGenderT[ , 2] / sum(other.service.40hpw.40s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
other.service.40hpw.40s.incomeGenderT
fisher.test(other.service.40hpw.40s.incomeGenderT)  # diff. not stat. sig.


##### 'Prof-specialty'
ggplot(prof.specialty.40hpw, aes(x=age_group, fill=sex)) +
  geom_histogram(position='dodge')

### subsetting 30s and 40s age groups
prof.specialty.40hpw.30s <- subset(prof.specialty.40hpw, age_group=='[30,40)')
prof.specialty.40hpw.40s <- subset(prof.specialty.40hpw, age_group=='[40,50)')

### creating gender-income tables
prof.specialty.40hpw.30s.incomeGenderT <- table(prof.specialty.40hpw.30s$income, 
                                                prof.specialty.40hpw.30s$sex)
prof.specialty.40hpw.40s.incomeGenderT <- table(prof.specialty.40hpw.40s$income, 
                                                prof.specialty.40hpw.40s$sex)
prof.specialty.40hpw.30s.incomeGenderT
prof.specialty.40hpw.40s.incomeGenderT

### comparing individuals in their 30s
## female (80.8% making less than $50K/year)
prof.specialty.40hpw.30s.incomeGenderT[ , 1]
prof.specialty.40hpw.30s.incomeGenderT[ , 1] / sum(prof.specialty.40hpw.30s.incomeGenderT[ , 1])  
## male (50% making less than $50K/year)
prof.specialty.40hpw.30s.incomeGenderT[ , 2]
prof.specialty.40hpw.30s.incomeGenderT[ , 2] / sum(prof.specialty.40hpw.30s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
prof.specialty.40hpw.30s.incomeGenderT
chisq.test(prof.specialty.40hpw.30s.incomeGenderT)  # diff. stat. sig.
cramersV(prof.specialty.40hpw.30s.incomeGenderT)  # Cramer's V effect size: 0.319

### comparing individuals in their 40s
## female (72.7% making less than $50K/year)
prof.specialty.40hpw.40s.incomeGenderT[ , 1]
prof.specialty.40hpw.40s.incomeGenderT[ , 1] / sum(prof.specialty.40hpw.40s.incomeGenderT[ , 1])  
## male (32% making less than $50K/year)
prof.specialty.40hpw.40s.incomeGenderT[ , 2]
prof.specialty.40hpw.40s.incomeGenderT[ , 2] / sum(prof.specialty.40hpw.40s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
prof.specialty.40hpw.40s.incomeGenderT
chisq.test(prof.specialty.40hpw.40s.incomeGenderT)  # diff. stat. sig.
cramersV(prof.specialty.40hpw.40s.incomeGenderT)  # Cramer's V effect size: 0.397


##### 'Protective-serv'
ggplot(protective.serv.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')
# skip this occupation category since there are too few female samples


##### 'Sales'
ggplot(sales.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')

### subsetting 20s and 30s
sales.40hpw.20s <- subset(sales.40hpw, age_group=='[20,30)')
sales.40hpw.30s <- subset(sales.40hpw, age_group=='[30,40)')

### creating gender-income tables
sales.40hpw.20s.incomeGenderT <- table(sales.40hpw.20s$income, sales.40hpw.20s$sex)
sales.40hpw.30s.incomeGenderT <- table(sales.40hpw.30s$income, sales.40hpw.30s$sex)
sales.40hpw.20s.incomeGenderT
sales.40hpw.30s.incomeGenderT

### comparing individuals in their 20s
## female (97.3% making less than $50K/year)
sales.40hpw.20s.incomeGenderT[ , 1]
sales.40hpw.20s.incomeGenderT[ , 1] / sum(sales.40hpw.20s.incomeGenderT[ , 1])  
## male (93% making less than $50K/year)
sales.40hpw.20s.incomeGenderT[ , 2]
sales.40hpw.20s.incomeGenderT[ , 2] / sum(sales.40hpw.20s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
sales.40hpw.20s.incomeGenderT
fisher.test(sales.40hpw.20s.incomeGenderT)  # diff. not stat. sig.

### comparing individuals in their 30s
## female (93.1% making less than $50K/year)
sales.40hpw.30s.incomeGenderT[ , 1]
sales.40hpw.30s.incomeGenderT[ , 1] / sum(sales.40hpw.30s.incomeGenderT[ , 1])  
## male (61.1% making less than $50K/year)
sales.40hpw.30s.incomeGenderT[ , 2]
sales.40hpw.30s.incomeGenderT[ , 2] / sum(sales.40hpw.30s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
sales.40hpw.30s.incomeGenderT
chisq.test(sales.40hpw.30s.incomeGenderT)  # diff. stat. sig.
cramersV(sales.40hpw.30s.incomeGenderT)  # Cramer's V effect size: 0.378


##### 'Tech-support'
ggplot(tech.support.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')

### subsetting 20s, 30s, and 40s age groups
tech.support.40hpw.20s <- subset(tech.support.40hpw, age_group=='[20,30)')
tech.support.40hpw.30s <- subset(tech.support.40hpw, age_group=='[30,40)')
tech.support.40hpw.40s <- subset(tech.support.40hpw, age_group=='[40,50)')

### creating gender-income tables
tech.support.40hpw.20s.incomeGenderT <- table(tech.support.40hpw.20s$income, 
                                              tech.support.40hpw.20s$sex)
tech.support.40hpw.30s.incomeGenderT <- table(tech.support.40hpw.30s$income, 
                                              tech.support.40hpw.30s$sex)
tech.support.40hpw.40s.incomeGenderT <- table(tech.support.40hpw.40s$income, 
                                              tech.support.40hpw.40s$sex)
tech.support.40hpw.20s.incomeGenderT
tech.support.40hpw.30s.incomeGenderT
tech.support.40hpw.40s.incomeGenderT

### comparing individuals in their 20s
## female (96.8% making less than $50K/year)
tech.support.40hpw.20s.incomeGenderT[ , 1]
tech.support.40hpw.20s.incomeGenderT[ , 1] / sum(tech.support.40hpw.20s.incomeGenderT[ , 1])  
## male (83.3% making less than $50K/year)
tech.support.40hpw.20s.incomeGenderT[ , 2]
tech.support.40hpw.20s.incomeGenderT[ , 2] / sum(tech.support.40hpw.20s.incomeGenderT[ , 2])  
## Fisher's Exact test to see if income difference by gender is stat. sig.
tech.support.40hpw.20s.incomeGenderT
fisher.test(tech.support.40hpw.20s.incomeGenderT)  # diff. stat. sig.
cramersV(tech.support.40hpw.30s.incomeGenderT)  # Cramer's V effect size: 0.31

### comparing individuals in their 30s
## female (87.9% making less than $50K/year)
tech.support.40hpw.30s.incomeGenderT[ , 1]
tech.support.40hpw.30s.incomeGenderT[ , 1] / sum(tech.support.40hpw.30s.incomeGenderT[ , 1])  
## male (64.1% making less than $50K/year)
tech.support.40hpw.30s.incomeGenderT[ , 2]
tech.support.40hpw.30s.incomeGenderT[ , 2] / sum(tech.support.40hpw.30s.incomeGenderT[ , 2])  
## Chi-square test to see if income difference by gender is stat. sig.
tech.support.40hpw.30s.incomeGenderT
chisq.test(tech.support.40hpw.30s.incomeGenderT)  # diff. stat. sig.
cramersV(tech.support.40hpw.30s.incomeGenderT)  # Cramer's V effect size: 0.258

### comparing individuals in their 40s
## female (83.8% making less than $50K/year)
tech.support.40hpw.40s.incomeGenderT[ , 1]
tech.support.40hpw.40s.incomeGenderT[ , 1] / sum(tech.support.40hpw.40s.incomeGenderT[ , 1])  
## male (53.5% making less than $50K/year)
tech.support.40hpw.40s.incomeGenderT[ , 2]
tech.support.40hpw.40s.incomeGenderT[ , 2] / sum(tech.support.40hpw.40s.incomeGenderT[ , 2])  
## Chi-square Exact test to see if income difference by gender is stat. sig.
tech.support.40hpw.40s.incomeGenderT
chisq.test(tech.support.40hpw.40s.incomeGenderT)  # diff. stat. sig.
cramersV(tech.support.40hpw.40s.incomeGenderT)  # Cramer's V effect size: 0.296


##### 'Transport-moving'
ggplot(transport.moving.40hpw, aes(x=age_group, fill=sex)) + 
  geom_histogram(position='dodge')
# skip this occupation category since there are too few female samples


###### By now, it is clear that fixing occupation field, number of work hours per week, 
###### and age group does not eliminate the discrepancy in the income variable between
###### male and female. 

###### In general (without controling for any other variable), do men and women in the
###### dataset have the same education level?
ggplot(data, aes(x=education, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(data, aes(x=education_num, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=0, hjust=1))

### At an initial glance, there doesn't seem to be any BIG difference in educational
### background between male and female.

#### Chi-square test 
educationGenderT <- table(data$education, data$sex)
educationGenderT
cramersV(educationGenderT)

#### Independent t-test (comparing education_num for men and women)
mean(male$education_num); mean(female$education_num)
median(male$education_num); median(female$education_num)

### checking for roughly the equal sample sizes
nrow(male) == nrow(female)

### checking for normal distribution
hist(male$education_num)  # normal enough?
hist(female$education_num)  # normal enough?
describe(male$education_num)  # skew < 3; kurtosis < 10; normal enough
describe(female$education_num)  # skew < 3; kurtosis < 10; normal enough

### checking for homogeneity of variance
leveneTest(data$education_num ~ data$sex)
var(male$education_num); var(female$education_num)

### conducting a non-parametric comparison test (Whitney-Mann U test)
wilcox.test(male$education_num, female$education_num)  # diff. not stat. sig.


###### In general (without controling for any other variable), do men and women in the
###### dataset have the same marital status?
ggplot(data, aes(x=marital_status, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=45, hjust = 1))

# individuals in same-sex marriage (there may have been more)
subset(male, relationship=='Wife')
subset(female, relationship=='Husband')

##### PARTIALLY CORRECT BUT WRONG ANALYSIS 
# We can see that there exist strong discrepancies between male and female in their 
# martial statuses. Since there same-sex marriages were virtually non-existant in the 
# dataset, we should have seen roughly the same number of males and females in each
# marital status category. For example, assuming a marriage between a man and a woman,
# one case of divorce would increment the divorce individual count by one for BOTH
# male and female. The same idea applies to the 'Married-civ-spouse' category. Since marriage
# takes place between one man and one woman, the correct sample should have generated roughly
# the equal number of 'Married-civ-spouse' cases for both men and women. 
# 
# Hence, this discrepancy may suggest that our dataset is not the most representative 
# sample of the whole population. Let's resample the original data (incomeData) so that
# there are not only equal number of men and women (9782 each) but also there are (roughly)
# the same number of male and female for 'Divorced', 'Married-civ-spouse', and 'Never-married'
# marital status categories.
#
# It is important to note that we are ONLY doing this for marital status variable and NOT 
# for any other variables. In other variables, there could be natural skew in a variable for
# one gender over another that is NOT caused by any sampling error. For example, men tend to 
# work longer hours than women. It's a natural phenomenon and we can create a fix for that 
# after we note the phenomenon (rather than creating a fix from the beginning). However, 
# in case of marital status, we know for a fact that there should be roughly equivalent 
# number of cases in 'Divorced', 'Married-civ-spouse', and 'Never-married' (especially the 
# number of individuals divorced) since we can essentially assume one-male-case-equates-to-
# one-female-case scenario unlike other variables that may be skewed by gender.
##### PARTIALLY CORRECT BUT WRONG ANALYSIS 

##### CORRECTION 
# Assuming low number of same-sex marriages, at any given time, there should be equal 
# number of men and women married. However, we cannot say that there should be roughly
# equal number of men and women in the 'Divorced' or 'Never-married' categories. 
# Consider the following case: a man and woman marries and undergoes divorce. Let's 
# say the man goes to find marry someone while the woman remains divorced. This will
# increment divorce count for women but not for men. In case the man marries a woman 
# who have never been married, this will also decrement the count for women in 
# the 'Never-married' category and unaffecting the count for male in the 'Never-married'
# category.
#
# Hence, we should adjust our sample selection to have equal number of males and 
# females in the 'Married-civ-spouse' category (the largest category) and perhaps not
# for other marital status categories.
##### CORRECTION

ggplot(data, aes(x=marital_status, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggplot(incomeData, aes(x=marital_status, fill=sex)) + 
  geom_bar(position='dodge') + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

##### ADJUSTMENT
# But because there are so much more available individuals in the 'Never-married' category, 
# we will just fix for the category and not 'Married-civ-spouse' category.
##### ADJUSTMENT 

###### Equal number of males and females who have never married
# selecing all male and female who have never married 
m.never_married <- subset(incomeData, sex=='Male' & marital_status=='Never-married')
f.never_married <- subset(incomeData, sex=='Female' & marital_status=='Never-married')
n.m.never_married <- nrow(m.never_married)
n.f.never_married <- nrow(f.never_married)
n.m.never_married
n.f.never_married

# reducing the number of male individuals who have never married to equate to females
# who have never married
set.seed(1234)
selectCond <- sample(n.m.never_married, n.f.never_married)
m.never_married <- m.never_married[selectCond, ]
n.m.never_married <- nrow(m.never_married)
n.m.never_married == n.f.never_married

# combing data frames of male and female who have never married
mf.never_married <- rbind(m.never_married, f.never_married)


###### Round 2
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
ggplot(mf.exec.20s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.exec.20s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.exec.20s.40hpw.sc.never_married$income, mf.exec.20s.40hpw.sc.never_married$sex)
mf.exec.20s.40hpw.sc.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.exec.20s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.exec.20s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.exec.20s.40hpw.hs.never_married$income, mf.exec.20s.40hpw.hs.never_married$sex)
mf.exec.20s.40hpw.hs.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.exec.30s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.exec.30s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.exec.30s.40hpw.sc.never_married$income, mf.exec.30s.40hpw.sc.never_married$sex)
mf.exec.30s.40hpw.sc.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.exec.30s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.exec.30s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.exec.30s.40hpw.hs.never_married$income, mf.exec.30s.40hpw.hs.never_married$sex)
mf.exec.30s.40hpw.hs.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.other.20s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.other.20s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.other.20s.40hpw.sc.never_married$income, mf.other.20s.40hpw.sc.never_married$sex)
mf.other.20s.40hpw.sc.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.other.20s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.other.20s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.other.20s.40hpw.hs.never_married$income, mf.other.20s.40hpw.hs.never_married$sex)
mf.other.20s.40hpw.hs.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.other.30s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.other.30s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.other.30s.40hpw.sc.never_married$income, mf.other.30s.40hpw.sc.never_married$sex)
mf.other.30s.40hpw.sc.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.other.30s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.other.30s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.other.30s.40hpw.hs.never_married$income, mf.other.30s.40hpw.hs.never_married$sex)
mf.other.30s.40hpw.hs.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.specialty.20s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.specialty.20s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.specialty.20s.40hpw.sc.never_married$income, mf.specialty.20s.40hpw.sc.never_married$sex)
mf.specialty.20s.40hpw.sc.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.specialty.20s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.specialty.20s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.specialty.20s.40hpw.hs.never_married$income, mf.specialty.20s.40hpw.hs.never_married$sex)
mf.specialty.20s.40hpw.hs.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.specialty.30s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.specialty.30s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.specialty.30s.40hpw.sc.never_married$income, mf.specialty.30s.40hpw.sc.never_married$sex)
mf.specialty.30s.40hpw.sc.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.specialty.30s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.specialty.30s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.specialty.30s.40hpw.hs.never_married$income, mf.specialty.30s.40hpw.hs.never_married$sex)
mf.specialty.30s.40hpw.hs.never_married.incomeGenderT

# Fisher's Exact test
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

ggplot(mf.sales.20s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

mf.sales.20s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.sales.20s.40hpw.sc.never_married$income, mf.sales.20s.40hpw.sc.never_married$sex)
mf.sales.20s.40hpw.sc.never_married.incomeGenderT

# Fisher's Exact Test
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
ggplot(mf.sales.20s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.sales.20s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.sales.20s.40hpw.hs.never_married$income, mf.sales.20s.40hpw.hs.never_married$sex)
mf.sales.20s.40hpw.hs.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.sales.30s.40hpw.sc.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# gender-income table
mf.sales.30s.40hpw.sc.never_married.incomeGenderT <- 
  table(mf.sales.30s.40hpw.sc.never_married$income, mf.sales.30s.40hpw.sc.never_married$sex)
mf.sales.30s.40hpw.sc.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.sales.30s.40hpw.hs.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# gender-income table
mf.sales.30s.40hpw.hs.never_married.incomeGenderT <- 
  table(mf.sales.30s.40hpw.hs.never_married$income, mf.sales.30s.40hpw.hs.never_married$sex)
mf.sales.30s.40hpw.hs.never_married.incomeGenderT

# Fisher's Exact test
fisher.test(mf.sales.30s.40hpw.hs.never_married.incomeGenderT)  # not stat. sig.


###### Round 3
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
ggplot(mf.exec.20s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.exec.20s.40hpw.never_married.incomeGenderT <- 
  table(mf.exec.20s.40hpw.never_married$income, mf.exec.20s.40hpw.never_married$sex)
mf.exec.20s.40hpw.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.exec.30s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.exec.30s.40hpw.never_married.incomeGenderT <- 
  table(mf.exec.30s.40hpw.never_married$income, mf.exec.30s.40hpw.never_married$sex)
mf.exec.30s.40hpw.never_married.incomeGenderT

# Fisher's Exact test
fisher.test(mf.exec.30s.40hpw.never_married.incomeGenderT)  # not stat. sig.


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
ggplot(mf.other.20s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.other.20s.40hpw.never_married.incomeGenderT <- 
  table(mf.other.20s.40hpw.never_married$income, mf.other.20s.40hpw.never_married$sex)
mf.other.20s.40hpw.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.other.30s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.other.30s.40hpw.never_married.incomeGenderT <- 
  table(mf.other.30s.40hpw.never_married$income, mf.other.30s.40hpw.never_married$sex)
mf.other.30s.40hpw.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.specialty.20s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.specialty.20s.40hpw.never_married.incomeGenderT <- 
  table(mf.specialty.20s.40hpw.never_married$income, mf.specialty.20s.40hpw.never_married$sex)
mf.specialty.20s.40hpw.never_married.incomeGenderT

# Fisher's Exact test
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
ggplot(mf.specialty.30s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# income-gender table
mf.specialty.30s.40hpw.never_married.incomeGenderT <- 
  table(mf.specialty.30s.40hpw.never_married$income, mf.specialty.30s.40hpw.never_married$sex)
mf.specialty.30s.40hpw.never_married.incomeGenderT

# Fisher's Exact test
fisher.test(mf.specialty.30s.40hpw.never_married.incomeGenderT)  # not stat. sig.


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

ggplot(mf.sales.20s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

mf.sales.20s.40hpw.never_married.incomeGenderT <- 
  table(mf.sales.20s.40hpw.never_married$income, mf.sales.20s.40hpw.never_married$sex)
mf.sales.20s.40hpw.never_married.incomeGenderT

# Fisher's Exact Test
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
ggplot(mf.sales.30s.40hpw.never_married, aes(x=income, fill=sex)) +
  geom_bar(position='dodge')

# gender-income table
mf.sales.30s.40hpw.never_married.incomeGenderT <- 
  table(mf.sales.30s.40hpw.never_married$income, mf.sales.30s.40hpw.never_married$sex)
mf.sales.30s.40hpw.never_married.incomeGenderT

# Fisher's Exact test
fisher.test(mf.sales.30s.40hpw.never_married.incomeGenderT)  # not stat. sig.


###### ROUGH ANALYSIS SCRIPT END HERE; BELOW CODE EXTRANEOUS


###### Round 2 test results
# ADM 20s not stat. sig.
# ADM 30s stat. sig.
# ADM 40s stat. sig.
#
# Craft 30s not stat. sig.
# Craft 40s stat. sig.
#
# Execs 30s stat. sig.
# Execs 40s stat. sig.
#
# Farming skipped
# 
# Handlers 30s not stat. sig.
#
# Machine 20s not stat. sig.
# Machine 30s stat. sig.
# Machine 40s stat. sig.
#
# Other service 20s not stat. sig.
# Other service 30s not stat. sig.
# Other service 40s not stat. sig.
#
# Prof specialty 30s stat. sig.
# Prof specialty 20s stat. sig.
#
# Protective skipped
#
# Sales 20s not stat. sig.
# Sales 30s stat. sig.
#
# Tech support 20s stat. sig.
# Tech support 30s stat. sig.
# Tech support 40s stat. sig.
#
# Transport moving skipped

##### Comparing education difference across gender in 'Adm-clerical'
##### Comparing education difference across gender in 'Craft-repair'
##### Comparing education difference across gender in 'Exec-managerial'
##### Comparing education difference across gender in 'Farming-fishing'
##### Comparing education difference across gender in 'Handlers-cleaners'
##### Comparing education difference across gender in 'Machine-op-inspct'
##### Comparing education difference across gender in 'Other-service'
##### Comparing education difference across gender in 'Priv-house-serv'
##### Comparing education difference across gender in 'Prof-specialty'
##### Comparing education difference across gender in 'Protective-serv'
##### Comparing education difference across gender in 'Sales'
##### Comparing education difference across gender in 'Tech-support'
##### Comparing education difference across gender in 'Transport-moving'


###### We saw a VERY interesting result in which comparing women and men in the same
###### age group, number of hours worked and occupational field yielded a biased favor
###### for men in their income. 



###### Are women less represented among executive and managerial positions?
execs.mngs <- subset(data, occupation=='Exec-managerial')

# reducing factor levels to just one
head(execs.mngs$occupation)  # 14 factor levels
execs.mngs$occupation <- as.factor(as.character(execs.mngs$occupation))
head(execs.mngs$occupation)  # 1 factor level

ggplot(execs.mngs, aes(x=occupation, fill=sex)) + 
  geom_bar(position='dodge') + 
  labs(title='Number of Executives or Managers by Gender')

observed <- table(execs.mngs$occupation, execs.mngs$sex); observed
chisq.test(observed)  # Yes, the difference is stat. sig.

# There are more male who are executive/managerial positions than are female, and the 
# difference is stat. sig.




###### Which group of women is more likely to make over $50K/year: married women or
###### women without spouses?
ggplot(female, aes(x=marital_status)) + 
  geom_bar(aes(y=..count..)) + 
  theme(axis.text.x = element_text(angle=45, hjust=1))

marital <- subset(female, 
                  marital_status == 'Divorced' |
                    marital_status == 'Married-civ-spouse' |
                    marital_status == 'Never-married')

female.divorced <- subset(female, marital_status=='Divorced')
female.married_civ <- subset(female, marital_status=='Married-civ-spouse')
female.never_married <- subset(female, marital_status=='Never-married')

table(female.divorced$income)
table(female.married_civ$income)
table(female.never_married$income)

#### Comparison via Chi-square test
observed <- table(marital$income, marital$marital_status); observed  
# 0 cell counts for non-subsetted marital categories

## Reducing 5 factors to 3
marital$marital_status <- as.factor(as.character(marital$marital_status))
head(marital$marital_status)

observed <- table(marital$income, marital$marital_status); observed
chisq.test(observed)  # p << 0.05



###### Do married women work less hours?

## Are the differences in hours per week means statistically significant?
mean(female.divorced$hours_per_week)  # 39.89
mean(female.married_civ$hours_per_week)  # 37.47
mean(female.never_married$hours_per_week)  #35.34

## Considering factorial ANOVA
leveneTest(marital$hours_per_week ~ marital$marital_status)  # no homogeneity of variance 

## Considering Friedman two-way ANOVA (non-parametric version of factorial ANOVA)
kruskal.test(marital$hours_per_week ~ marital$marital_status)  # p-value lower than 0.05
# Since p-value is (much) less than 0.05, I conclude that the means of the three marital 
# status groups ('Divorced', 'Married-civ-spouse' and 'Never-married') are NOT statistically
# equal

#### Post-hoc test to see which means difference (if not both) is stat. sig.
# XXXXXX




###### 