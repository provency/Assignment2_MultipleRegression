#Loading SPSS Data 
library(tidyverse)
library(haven)
my.data <- read_spss("fall_2016_mrdata.sav")
glimpse(my.data) #To view variables only

#Create APA correlation matrix
library(apaTables)
apa.cor.table(my.data)
apa.cor.table(my.data, filename="Table1_my.dataAPA.doc",table.number = 1)
#Pick the single best predictor using the correlation matrix

#Looking for any curvilinear relationships
psych::pairs.panels(as.data.frame(my.data))
#No curvilinear relationship

#First Multiple Regression Analysis (Variance of "grade" accounted by "study" and "sleep")
myfirst.regression <-lm(grade ~ study + sleep, data=my.data)
print(myfirst.regression) #to see brief output
summary(myfirst.regression) #to see extended output
# 44% of the variability in "grade" can be explained by "study" and "sleep"

#Find variance of "grade" uniquely predicted by "sleep" using sr2
apa.reg.table(myfirst.regression, filename = "myfirstRegressionTable.doc")
#7% of variability in grade is predicted by sleep only

#Second Multiple Regression Analysis (Variance of "grade" accounted by "study" and "practice")
mysecond.regression <-lm(grade ~ study + practice, data=my.data)
print(mysecond.regression) #to see brief output
summary(mysecond.regression) #to see extended output
# 37% of the variability in "grade" can be explained by "study" and "sleep"

#Find variance of "grade" uniquely predicted by "practice" using sr2
apa.reg.table(mysecond.regression, filename = "mysecondRegressionTable.doc")
# 0% of variability in "grade" is predicted by "practice" only

