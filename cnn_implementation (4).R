#scaling in 0 to 1

df[is.na(df)] <- 0
df2 <- (df- min(df))/(max(df)-min(df))
train_array <- data.matrix(df2)
train_array <- t(train_array)

dim(train.x) <- c(1024,12,1, ncol(train.x))





dfq <- read.csv(paste("F:/minor 2018 final/testing_dataset/testing.csv" ))
df21 <- (dfq- min(dfq))/(max(dfq)-min(dfq))
test_array <- data.matrix(df21)
test.x <- t(test_array)
labb <-read.csv(paste("F:/minor 2018 final/labels.csv" ))

labb <- factor(labb)


labb$vec = as.numeric(factor(labb$vec,
                             levels = c('belly_pain', 'burping', 'cant_say','cold/hot','discomfort','hungry','lonely','scared','tired'),
                             labels = c(1, 2, 3,4,5,6,7,8,9)))


trainl <- read.csv("C:\Users\rakshita singh\Desktop\EXCEL SHEET major\training dataset\training labels.csv")

train_y <- data.matrix( trainl)



testl <- read.csv("C:\Users\rakshita singh\Desktop\EXCEL SHEET major\testing dataset\testing labels.csv")
#152  currently 560th obs included 2 times
test_y <- testl

table(test_y)
table(train_y)

library(reshape2)
# install.packages("reshape2")
# install.packages("reshape")
library(reshape)
library(mlbench)
library(keras)

#install_tensorflow()
library(tensorflow)

# mx.set.seed(0)
# require(devtools)
# install_version("DiagrammeR", version = "0.9.0", repos = "http://cran.us.r-project.org")
# require(DiagrammeR)
library(mxnet)


# input
data <- mx.symbol.Variable('data')
# first conv
conv1 <- mx.symbol.Convolution(data=data, kernel=c(108,108), num_filter=512)
sig1 <- mx.symbol.Activation(data=conv1, act_type="sigmoid")
pool1 <- mx.symbol.Pooling(data=sig1, pool_type="sum",
                           kernel=c(2,2), stride=c(2,2))
# second conv
conv2 <- mx.symbol.Convolution(data=pool1, kernel=c(108,108), num_filter=1200)
sig2 <- mx.symbol.Activation(data=conv2, act_type="sigmoid")
pool2 <- mx.symbol.Pooling(data=sig2, pool_type="sum",
                           kernel=c(2,2), stride=c(2,2))
# first fullc
flatten <- mx.symbol.Flatten(data=pool2)
fc1 <- mx.symbol.FullyConnected(data=flatten, num_hidden=500)
sig3 <- mx.symbol.Activation(data=fc1, act_type="sigmoid")
# second fullc
fc2 <- mx.symbol.FullyConnected(data=sig3, num_hidden=9)
# loss
lenet <- mx.symbol.SoftmaxOutput(data=fc2,multi.output=TRUE)



## Set seed for reproducibility
mx.set.seed(100)

## Device used. Sadly not the GPU :-(
device <- mx.cpu()

## Train on 1200 samples
train_array <- t(train_array)

train_iter = mx.io.arrayiter(data = (train_array), label = (train_y))
model <- mx.model.FeedForward.create(lenet, X=train_iter,
                                     ctx=mx.cpu(), num.round=50, array.batch.size=20,
                                     learning.rate=2e-6, momentum=0.9)
model <- mx.model.FeedForward.create(lenet, X = train.x, y = train_y,
                                     ctx = device,
                                     num.round = 30,
                                     array.batch.size = 56,
                                     array.layout="colmajor",
                                     learning.rate = 0.05,
                                     momentum = 0.9,
                                     wd = 0.00001,
                                     eval.metric = mx.metric.accuracy,
                                     epoch.end.callback = mx.callback.log.train.metric(100))
