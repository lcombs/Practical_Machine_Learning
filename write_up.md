# Predictive Assignment Writeup
lcombs  
02/11/2016  

# Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

# Data

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 

The training data for this project are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

# Goal

The goal for this project is to predict the manner in which an individual did an exercise; *i.e. how well they did this certain activity*. This is the "classe" variable in the training set.  The report includes a description of how the model was built, how cross validation was used, what the expected out of sample error is, and how choices were made along the way.

# Reading the data


```r
library(magrittr)

library(RCurl)

content <- 
        getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")

train <- 
        read.csv(text = content,
                 na.strings = c("NA", "#DIV/0!", ""))

content <- 
        getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

test <- 
        read.csv(text = content,
                 na.strings = c("NA", "#DIV/0!", ""))

# str(train)
# names(train)

library(data.table)

train <- 
        as.data.table(train)

test <- 
        as.data.table(test)

rm(content)
```

# Examine the classe variable. 

The "classe" variable in the dataset is a factor variable with levels A-E, indicators for the manner in which an individual conducted a particular exercise.


```r
train[, classe]  %>% class()
```

```
## [1] "factor"
```

```r
train[, classe] %>% head(5)
```

```
## [1] A A A A A
## Levels: A B C D E
```

```r
barplot(train[, classe] %>% table(), 
        main = "Frequency of Classe in Training")
```

![](write_up_files/figure-html/unnamed-chunk-2-1.png) 

# Clean the data.

Since most machine learning algorithms do not handle NAs well, we remove all the columns that contain NAs. We also remove the time-series variables.



```r
NAs <- 
        apply(train, 
              2, 
              function(x) 
                      sum(is.na(train[, x]))) 
NAs <- 
        NAs[(NAs > 0)] %>%
        names() 

train <- 
        train[, !NAs, 
              with = FALSE] 

# rm time series variables
train <- 
        train[, -c(1:7), 
              with = FALSE]


# remove the same columns of test
test <- 
        test[, !NAs, 
              with = FALSE]

test <- 
        test[, -c(1:7), 
              with = FALSE]
```

# Building Models - Accuracy Check

In order to show a test of the accuracy of the model, we will part the original training data into our own training and testing sets.


```r
library(caret)

set.seed(2)
partition <- 
        createDataPartition(y = train$classe, 
                            p = .75, list = FALSE)

my_train <- train[partition, ]
my_test <- train[-partition,]

my_model <- 
        train(classe ~ ., 
              data = my_train, 
              method = "rf", 
              ntree = 5,
              type = "class")

my_predictions <- 
        predict(my_model, newdata = my_test)

confusionMatrix(my_predictions, my_test[, classe])
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1387   10    1    1    1
##          B    8  924   12    2    5
##          C    0   10  840   13    0
##          D    0    2    2  786    4
##          E    0    3    0    2  891
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9845          
##                  95% CI : (0.9806, 0.9878)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9804          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9943   0.9737   0.9825   0.9776   0.9889
## Specificity            0.9963   0.9932   0.9943   0.9980   0.9988
## Pos Pred Value         0.9907   0.9716   0.9733   0.9899   0.9944
## Neg Pred Value         0.9977   0.9937   0.9963   0.9956   0.9975
## Prevalence             0.2845   0.1935   0.1743   0.1639   0.1837
## Detection Rate         0.2828   0.1884   0.1713   0.1603   0.1817
## Detection Prevalence   0.2855   0.1939   0.1760   0.1619   0.1827
## Balanced Accuracy      0.9953   0.9834   0.9884   0.9878   0.9938
```

This confusion matrix shows the model is very accurate. We can expect to accurately predict how these activities were done using the other vairables in the dataset.

# Error

The out-of-sample error is: 1.5497553%. 

# Building Models - Prediction with Test Data

Now, we remake the model with all the training data so that we can make sure to answer the quiz questions correctly. Here, we use cross validation training the random forest on the original training data and only testing it on the original test data. The test data is not used for anything besides the final step.


```r
set.seed(2)

# predict using any of the other variables
rf_model <- 
        train(classe ~ ., 
              data = train, 
              method = "rf", 
              ntree = 5,
              type = "class")

rf_pred <- 
        predict(rf_model, 
                test)
```

The predicted values for the 20 test problems are B, A, B, A, A, E, D, B, A, A, B, C, B, A, E, E, A, B, B, B.


*** Note: These documents are available at: https://github.com/lcombs/Practical_Machine_Learning ***
