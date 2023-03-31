#  download tobit_minimal.R and place it in your getwd() path
# Install marginaleffects and AER

source("tobit_minimal.R")

# Example 1
data("Affairs")
model <-
  tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
        data = Affairs)

# 1) duplicating tobit model
model_custom <- model
# assign "tobitK" class to the new object
class(model_custom) <- c("tobitK", class(model))

## you can work with marginaleffects in the usual way

# as stata's margin ... predict(ystar())
slopes(model_custom, newdata = "mean")

# as stata's margin ... predict(e())
slopes(model_custom, newdata = "mean", type="link")

# Example 2
set.seed(13)
x <- rnorm(15)
y <- x + rnorm(15)
ystar <- ifelse(y > 0, y, 0)
dat <- data.frame(ystar = ystar,x=x)
model2 <- tobit(ystar ~ x +I(x^2), data=dat)

slopes(model2, newdata = "mean")

model2K <- model2
class(model2K) <- c("tobitK", class(model2))

slopes(model2K, newdata = "mean")
slopes(model2K)



