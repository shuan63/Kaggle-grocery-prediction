library(tree)
library(ISLR)
library(readxl)
library(gbm)

#read the data
store2 = read_excel("Store2-1.xlsx")
store3 = read_excel("Store3-1.xlsx")
store4 = read_excel("Store4-1.xlsx")
store5 = read_excel("Store5-1.xlsx")
store6 = read_excel("Store6-1.xlsx")
store7 = read_excel("Store7-1.xlsx")
store8 = read_excel("Store8-1.xlsx")
store9 = read_excel("Store9-1.xlsx")

#store 2
x_train_2 = subset(store2, Random == "Train")[, 3:98]
x_test_2 = subset(store2, Random == "Test")[, 3:98]

y_train_2 = subset(store2, Random == "Train")[, 99:122]
y_test_2 = subset(store2, Random == "Test")[, 99:122]

#store 3
x_train_3 = subset(store3, Random == "Train")[, 3:98]
x_test_3 = subset(store3, Random == "Test")[, 3:98]

y_train_3 = subset(store3, Random == "Train")[, 99:122]
y_test_3 = subset(store3, Random == "Test")[, 99:122]

#store 4
x_train_4 = subset(store4, Random == "Train")[, 3:98]
x_test_4 = subset(store4, Random == "Test")[, 3:98]

y_train_4 = subset(store4, Random == "Train")[, 99:122]
y_test_4 = subset(store4, Random == "Test")[, 99:122]

#store 5
x_train_5 = subset(store5, Random == "Train")[, 3:98]
x_test_5 = subset(store5, Random == "Test")[, 3:98]

y_train_5 = subset(store5, Random == "Train")[, 99:122]
y_test_5 = subset(store5, Random == "Test")[, 99:122]

#store 6
x_train_6 = subset(store6, Random == "Train")[, 3:98]
x_test_6 = subset(store6, Random == "Test")[, 3:98]

y_train_6 = subset(store6, Random == "Train")[, 99:122]
y_test_6 = subset(store6, Random == "Test")[, 99:122]

#store 7
x_train_7 = subset(store7, Random == "Train")[, 3:98]
x_test_7 = subset(store7, Random == "Test")[, 3:98]

y_train_7 = subset(store7, Random == "Train")[, 99:122]
y_test_7 = subset(store7, Random == "Test")[, 99:122]

#store 8
x_train_8 = subset(store8, Random == "Train")[, 3:98]
x_test_8 = subset(store8, Random == "Test")[, 3:98]

y_train_8 = subset(store8, Random == "Train")[, 99:122]
y_test_8 = subset(store8, Random == "Test")[, 99:122]

#store 9
x_train_9 = subset(store9, Random == "Train")[, 3:98]
x_test_9 = subset(store9, Random == "Test")[, 3:98]

y_train_9 = subset(store9, Random == "Train")[, 99:122]
y_test_9 = subset(store9, Random == "Test")[, 99:122]

#boost tree store_2
set.seed(1)
bt_train_2 = list()
bt_test_2 = list()
bt_rmse_2 = list()
for (i in 1:24){
  boost.bt = gbm(unlist(y_train_2[[i]])~., data = x_train_2, distribution = "gaussian", n.trees=5000,interaction.depth=4)
  bt_train_2[[i]] = predict(boost.bt, newdata = x_test_2,n.trees=5000)
  bt_test_2[[i]] = y_test_2[[i]]
  bt_rmse_2[[i]] = mean((bt_train_2[[i]] - y_test_2[[i]])^2)^0.5
}


#boost tree store_3
set.seed(1)
bt_train_3 = list()
bt_test_3 = list()
bt_rmse_3 = list()
for (i in 1:24 ){
  boost.bt = gbm(unlist(y_train_3[[i]])~., data = x_train_3, distribution = "gaussian", n.trees=5000,interaction.depth=4)
  bt_train_3[[i]] = predict(boost.bt, newdata = x_test_3,n.trees=5000)
  bt_test_3[[i]] = y_test_3[[i]]
  bt_rmse_3[[i]] = mean((bt_train_3[[i]] - y_test_3[[i]])^2)^0.5
}

#boost tree store_4
set.seed(1)
bt_train_4 = list()
bt_test_4 = list()
bt_rmse_4 = list()
for (i in 1:24){
  boost.bt = gbm(unlist(y_train_4[[i]])~., data = x_train_4, distribution = "gaussian", n.trees=5000,interaction.depth=4)
  bt_train_4[[i]] = predict(boost.bt, newdata = x_test_4,n.trees=5000)
  bt_test_4[[i]] = y_test_4[[i]]
  bt_rmse_4[[i]] = mean((bt_train_4[[i]] - y_test_4[[i]])^2)^0.5
}

#boost tree store_5
set.seed(1)
bt_train_5 = list()
bt_test_5 = list()
bt_rmse_5 = list()
for (i in 1:24){
  boost.bt = gbm(unlist(y_train_5[[i]])~., data = x_train_5, distribution = "gaussian", n.trees=5000,interaction.depth=4)
  bt_train_5[[i]] = predict(boost.bt, newdata = x_test_5,n.trees=5000)
  bt_test_5[[i]] = y_test_5[[i]]
  bt_rmse_5[[i]] = mean((bt_train_5[[i]] - y_test_5[[i]])^2)^0.5
}

#boost tree store_6
set.seed(1)
bt_train_6 = list()
bt_test_6 = list()
bt_rmse_6 = list()
for (i in 1:24){
  boost.bt = gbm(unlist(y_train_6[[i]])~., data = x_train_6, distribution = "gaussian", n.trees=5000,interaction.depth=4)
  bt_train_6[[i]] = predict(boost.bt, newdata = x_test_6,n.trees=5000)
  bt_test_6[[i]] = y_test_6[[i]]
  bt_rmse_6[[i]] = mean((bt_train_6[[i]] - y_test_6[[i]])^2)^0.5
}

#boost tree store_7
set.seed(1)
bt_train_7 = list()
bt_test_7 = list()
bt_rmse_7 = list()
for (i in 1:24){
  boost.bt = gbm(unlist(y_train_7[[i]])~., data = x_train_7, distribution = "gaussian", n.trees=5000,interaction.depth=4)
  bt_train_7[[i]] = predict(boost.bt, newdata = x_test_7,n.trees=5000)
  bt_test_7[[i]] = y_test_7[[i]]
  bt_rmse_7[[i]] = mean((bt_train_7[[i]] - y_test_7[[i]])^2)^0.5
}

#boost tree store_8
set.seed(1)
bt_train_8 = list()
bt_test_8 = list()
bt_rmse_8 = list()
for (i in 1:24){
  boost.bt = gbm(unlist(y_train_8[[i]])~., data = x_train_8, distribution = "gaussian", n.trees=5000,interaction.depth=4)
  bt_train_8[[i]] = predict(boost.bt, newdata = x_test_8,n.trees=5000)
  bt_test_8[[i]] = y_test_8[[i]]
  bt_rmse_8[[i]] = mean((bt_train_8[[i]] - y_test_8[[i]])^2)^0.5
}

#boost tree store_9
set.seed(1)
bt_train_9 = list()
bt_test_9 = list()
bt_rmse_9 = list()
for (i in 1:24){
  boost.bt = gbm(unlist(y_train_9[[i]])~., data = x_train_9, distribution = "gaussian", n.trees=5000,interaction.depth=4)
  bt_train_9[[i]] = predict(boost.bt, newdata = x_test_9,n.trees=5000)
  bt_test_9[[i]] = y_test_9[[i]]
  bt_rmse_9[[i]] = mean((bt_train_9[[i]] - y_test_9[[i]])^2)^0.5
}

bt_rmse = do.call(rbind, Map(cbind, bt_rmse_2, bt_rmse_3, bt_rmse_4, bt_rmse_5, bt_rmse_6, bt_rmse_7, bt_rmse_8, bt_rmse_9))
bt_rmse