library(marginaleffects)
library(AER)
#library(data.table)
library(censReg) # for internal check, see at the bottom
# Affairs{AER}
data("Affairs")

# example from help to tobit{AER}
model <-
  tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,
        data = Affairs)

options("marginaleffects_model_classes" = "tobitK")

model_custom <- model

class(model_custom) <- c("tobitK", class(model))

?tobit
get_predict.tobitK <-
  function(model, newdata, type, ...) {
    #print(newdata) # to see non-standard means with 'newdata = "mean"'
    # for slopes()
    limits_extract <- function(mod) {
      
      # to compute predictions
      # with arbitrary tobit limits
      ll <- model$call$left 
      if (is.null(ll)) ll <- 0
      rl <- model$call$right
      if (is.null(rl)) rl <- Inf
      
      return(list(ll,rl))
    }
    
    bounds <- limits_extract(model)
    ll <- bounds[[1]]
    if (!is.numeric(ll)) ll <- as.numeric(deparse1(ll))
    rl <- bounds[[2]]
    
    
    xbeta <- predict(model, newdata)
        sigma <- model$scale
    
    if (type == "response") {
      if (ll == 0 && rl == Inf) {
      z <- xbeta / sigma
      Yhat <-
        pnorm(z) * xbeta + sigma * dnorm(z)
      } else {
        F1 <- pnorm((ll-xbeta)/sigma)
        F2 <- pnorm((rl-xbeta)/sigma)
        f1 <- dnorm((ll-xbeta)/sigma)
        f2 <- dnorm((rl-xbeta)/sigma)
        Yhat <-
          F1*ll + xbeta*(F2 - F1) + sigma*(f1 - f2) + (1 - F2)*rl
      }
    }
    if (type == "link") {
     
      if (ll == 0 && rl == Inf) {
        z <- xbeta / sigma
      Yhat <- xbeta + sigma*dnorm(z)/pnorm(z)
      } else {
        F1 <- pnorm((ll-xbeta)/sigma)
        F2 <- pnorm((rl-xbeta)/sigma)
        f1 <- dnorm((ll-xbeta)/sigma)
        f2 <- dnorm((rl-xbeta)/sigma)
        Yhat <- xbeta + sigma*(f1 - f2)/(F2-F1)
      }
      
    }
    
    out <- data.frame(rowid = length(Yhat),
                      estimate = Yhat)
    return(out)
  }



newda <- Affairs |>
  dplyr::select(age, yearsmarried, religiousness, occupation, rating) |>
  dplyr::summarise_all(.funs = mean)
newda

means
means <- c(1,as.numeric(Affairs |>
                          dplyr::select(age, yearsmarried, religiousness, occupation, rating) |>
                          dplyr::summarise_all(.funs = mean)))

# compute analytic dY/dX
b <- as.numeric(coef(model_custom))
b[-1]*pnorm(sum(means*b)/model_custom$scale)


# model_aux <-
#   tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,left = -1,
#         right = 12, data = Affairs)
# class(model_aux) <- c("tobitK", class(model))

#options(marginaleffects_numDeriv = list(method = "Richardson", method.args = list(eps = 1e-5)))
#options(marginaleffects_numDeriv = NULL)


slopes(model_custom, newdata = "mean")
slopes(model_custom, newdata = newda)

# the comparison of 'newdata = "mean"' and 'newdata = newda'
# shows that values of religiousness, occupation, and rating
# with 'newdata = "mean"' selected as discrete, but numbers
# 3.0004,  4.0004, indicate that derivatives computed as
# continuous ones
# to see this, uncomment 'print(newdata)' in get_predict.tobitK

# numbers with 'newdata = newda' are consistant with margEff{censReg}
# At the moment I do not know why NA

slopes(model_custom, newdata = newda, type = "link")




# small R-internal check


c.tobit <-
  censReg(affairs ~ age + yearsmarried + religiousness + occupation + rating,
          data = Affairs)
summary( margEff(c.tobit) )

# with type = "response" to check against stata, predict(ystar())
# is needed in margins, type = "link" <-> predict(e())

class(model_aux$call)

model_aux$call$right
model_aux <-
  tobit(affairs ~ age + yearsmarried + religiousness + occupation + rating,left = 0,
        right = 12, data = Affairs)
class(model_aux) <- c("tobitK", class(model))
slopes(model_aux, newdata = newda)
slopes(model_aux, newdata = newda,type = "link")


lm_manual <- function(f, data, ...) {
  # design matrix
  X <- model.matrix(f, data = data)
  # response matrix
  Y <- data[[as.character(f[2])]]
  # coefficients
  b <- solve(crossprod(X)) %*% crossprod(X, Y)
  Yhat <- X %*% b
  # variance-covariance matrix
  e <- Y - Yhat
  df <- nrow(X) - ncol(X)
  s2 <- sum(e^2) / df
  V <- s2 * solve(crossprod(X))
  # model object
  out <- list(
    d = data,
    f = f,
    X = X,
    Y = Y,
    V = V,
    b = b)
  # class name: lm_manual
  class(out) <- c("lm_manual", "list")
  return(out)
}

get_coef.lm_manual <- function(model, ...) {
  b <- model$b
  b <- setNames(as.vector(b), row.names(b))
  return(b)
}

set_coef.lm_manual <- function(model, coefs, ...) {
  out <- model
  out$b <- coefs
  return(out)
}

get_vcov.lm_manual <- function(model, ...) {
  return(model$V)
}

get_predict.lm_manual <- function(model, newdata, ...) {
  newX <- model.matrix(model$f, data = newdata)
  print()
  Yhat <- newX %*% model$b
  out <- data.frame(
    rowid = seq_len(nrow(Yhat)),
    estimate = as.vector(Yhat))
  return(out)
}
mtcars
model <- lm_manual(mpg ~ hp + drat, data = mtcars)
newdat <- data.frame(hp=100+1i,drat=3,mpg=1)
get_predict.lm_manual(model,newdat)


model.matrix


methods(model.matrix)
methods(print)
model.matrix.lm

model.matrix.default
model.matrix.lm

model.matrix.default




