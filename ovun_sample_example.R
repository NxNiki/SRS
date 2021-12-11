data(hacide)

# imbalance on training set
table(hacide.train$cls)

# balanced data set with both over and under sampling
data.balanced.ou <- ovun.sample(cls~., data=hacide.train,
                                N=nrow(hacide.train), p=0.5, 
                                seed=1, method="both")$data

table(data.balanced.ou$cls)

# balanced data set with over-sampling
data.balanced.over <- ovun.sample(cls~., data=hacide.train, 
                                  p=0.5, seed=1, 
                                  method="over")$data

table(data.balanced.over$cls)


library(dplyr)

d1 <- data_frame(
    x = letters[1:3],
    y = LETTERS[1:3],
    a = rnorm(3)
)

d2 <- data_frame(
    x2 = letters[3:1],
    y2 = LETTERS[3:1],
    b = rnorm(3)
)

left_join(d1, d2, by = c("x" = "x2", "y" = "y2"))


# colname of output of predict cannot be refined while assigning to dataframe:

library(glmnet)

x=matrix(rnorm(100*20),100,20)
y=rnorm(100)
cv.fit = cv.glmnet(x, y)
x=matrix(rnorm(100*20),100,20)
yhat = predict(cv.fit, s=cv.fit$lambda.1se, newx=x, type="response")

df = data.frame(ypred = yhat, y = y)
head(df)

df = data.frame(y_test = y, y_pred = as.vector(yhat))
head(df)


df$yhat = yhat
