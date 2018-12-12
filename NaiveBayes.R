# Libraries
library(naivebayes) # http://github.com/majkamichal/naivebayes
library(dplyr) # https://dplyr.tidyverse.org/reference/index.html
library(ggplot2) # https://ggplot2.tidyverse.org/reference/index.html
library(psych) # https://www.rdocumentation.org/packages/psych/versions/1.0-17
#https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/histdens.html#density-plots
#https://www.mathsisfun.com/data/standard-deviation.html
#https://www.rdocumentation.org/packages/raster/versions/2.8-4/topics/predict
#https://www.slideshare.net/hadooring/data-discretization
#https://paginas.fe.up.pt/~ec/files_1112/week_03_Data_Preparation.pdf

# Data
data <- read.csv(file.choose(), header = T) # input data file
str(data) # convert data to string
xtabs(~admit+rank, data = data) # build table of data with names of subtable
data$rank <- as.factor(data$rank) # extract data with characteristic of RANK and order them by converting to vector of characters
data$admit <- as.factor(data$admit)

# Visualization
pairs.panels(data[-1]) # sketch splots without 1st element

# create box plot
data %>% # pass data for sketching
         ggplot(aes(x=admit, y=gpa, fill = admit)) + # sketch space with x coordinate is ADMIT and y coordinate is GPA. Fill color to box based on ADMIT
         geom_boxplot() + # sketch box plot with data
         ggtitle("Box Plot") # title of "BOX PLOT"

# create density plot
data %>% # pass data for sketching
         ggplot(aes(x=gpa, fill = admit)) + # sketch space with x coordinate is GPA and fill color based on ADMIT
         geom_density(alpha=0.8, color= 'black') + # with outter line of black and 80% ocpacity solid color filled
         ggtitle("Density Plot") # title of "DENSITY PLOT"

# Data Partition
set.seed(1234) # random seed set for keep random set fixed all generations
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2)) # index with 2 row of data, replace T for True and set 80% of data is training and 20% is testing
train <- data[ind == 1,] # 1st row with 80% data
test <- data[ind == 2,] # 2nd row with 20% data

# Naive Bayes Model
model <- naive_bayes(admit ~ ., data = train, usekernel = T) # response var is ADMIT("." means all) and data from TRAIN
model # run model
# A priori probabilities:
#         0         1
# 0.6861538 0.3138462
#           31.3% admit
# Calculations of mean and standard deviation for each table of data
# rank
# P(rank = 1 | admit = 0) = 0.103
# P(rank = 1 | admit = 1) = 0.245

# Filtering by admit with value 1 by connected to TRAIN and connecting to GRE 
# train %>%
#          filter(admit == "1") %>%
#          summarise(mean(gre), sd(gre))
# return 622.9412
# if == "0" -> 578.6547

# sketch model
# green is admited and red is not
plot(model)

# Predict
p <- predict(model, train, type = 'prob') # predict with probability, gre, gpa, rank and admit or not
head(cbind(p, train)) # cout-ing first vector and binding p according to TRAIN

# Confusion Matrix - train data
p1 <- predict(model, train) # store predictions in p1
# create confusion matrix
(tab1 <- table(p1, train$admit)) # () for printing TAB1 when run
# p1    0  1
#   0 196 69
#   1  27 33
# 196 is number of not admit and 33 is admitted
1 - sum(diag(tab1)) / sum(tab1) # diag() for take only diagonal
# miss classification is almost 30%
# 
# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$admit))
1 - sum(diag(tab2)) / sum(tab2)
