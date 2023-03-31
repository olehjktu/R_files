# This file extends AER::tobit with standard prediction methods
# which correspond to predict(ystar(())) and predict(e(())) in
# stata margin with R marginaleffects package
# tobit_me_examples.R contains examples

# Install marginaleffects and AER
library(marginaleffects)
library(AER)

options("marginaleffects_model_classes" = "tobitK")
get_predict.tobitK <-
  function(model, newdata, type, ...) {
        limits_extract <- function() {
      
      # to compute predictions
      # with arbitrary tobit limits
      ll <- model$call$left 
      if (is.null(ll)) ll <- 0
      rl <- model$call$right
      if (is.null(rl)) rl <- Inf
      
      return(list(ll,rl))
    }
    
    bounds <- limits_extract()
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










