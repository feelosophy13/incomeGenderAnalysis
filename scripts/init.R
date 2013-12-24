###### Setting working directory
getwd()
setwd('/Users/hawooksong/Desktop/programming_projects/incomeDataAnalysis/genderAnalysis')


###### Loading R packages
# install.packages('arules')
library(arules)


###### Loading data
data('AdultUCI')
head(AdultUCI)

incomeData <- AdultUCI
head(incomeData)
dim(incomeData)


###### Checking for rows with missing values
?complete.cases
table(complete.cases(incomeData))  # 18680 rows with one or more incomplete cell data; 30162 rows of complete data

dim(incomeData)  # dataframe dimension BEFORE taking out rows with missing value(s)
completeCasesCond <- complete.cases(incomeData)
incomeData <- incomeData[completeCasesCond, ]
dim(incomeData)  # dataframe dimension AFTER taking out rows with missing value(s)
head(incomeData)

table(complete.cases(incomeData))  # every row should now be complete cases
table(is.na(incomeData$income))  # there should be no missing values in the 'income' column


###### Renaming column names
names(incomeData)  # replace '-' to '_' for later 
names(incomeData) <- c("age", "workclass", "fnlwgt", "education", "education_num", 
                       "marital_status", "occupation", "relationship", "race", "sex",
                       "capital_gain", "capital_loss", "hours_per_week", "native_country",
                       "income")


###### Creating 'age_group' column
max(incomeData$age)
min(incomeData$age)
incomeData$age_group <- cut(incomeData$age, seq(10, 90, by=10), right=F)


###### Saving incomeData R object to RData file
save(incomeData, file='./data/incomeData.RData') 


