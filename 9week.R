### 예제 1
x <- cbind(c(175, 160, 182, 165), c(68,55,85,72))
x <- cbind(1,x)
y <- c(22.2, 21.5, 25.6, 26.4)

xtx <- t(x) %*% x
xty <- t(x) %*% y

betahat <- solve(xtx) %*% xty
betahat


### 예제 2
# SSR, SSE, SST 확인
# y, mean(y), yhat

yhat <- x %*% betahat
yhat

ss <- function(a, b) {
  sum((a-b)^2)
}
ssr <- ss(yhat, mean(y))
sse <- ss(y, yhat)
sst <- ss(y, mean(y))
sst <- ssr + sse

sss <- c(ssr, sse)
df <- c(3-1, 4-3)
mss <- sss/df
f0 <- mss[1]/mss[2]

qf(0.05, df[1], df[2], lower.tail = F)

rsq <- sss[1]/sum(sss)
adjrsq <- 1-(3)/(4-3)*(1-rsq)

### 예제3
xnew <- c(1, 170,60)
ynew <- xnew %*%betahat

# sigsq = mse
ci <- qt(0.025, df=4-3, lower.tail = F) * sqrt(mss[2] * t(xnew) %*% solve(xtx) %*% xnew)
ynew + ci
ynew - ci

