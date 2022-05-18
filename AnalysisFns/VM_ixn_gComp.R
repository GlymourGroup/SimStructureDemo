function(analysisOptions=NULL){
  f <- function(d){
    model_m <- lm(M~V,d)
    model_y <- lm(Y~V*M,d)

    #Pure indirect effect	  (Y0M1−Y0M0)
    d_0 <- d
    d_1 <- d

    d_0[["V"]] <- 0
    d_1[["V"]] <- 1
    d_0$M <- predict(model_m, newdata=d_0)
    d_1$M <- predict(model_m, newdata=d_1)

    d_0$V <- 0
    d_1$V <- 0

    d_0$Y <- predict(model_y, newdata=d_0)
    d_1$Y <- predict(model_y, newdata=d_1)
    est_PIE <- mean(d_1$Y - d_0$Y)

    #Total direct effect	  (Y1M1−Y0M1)
    d_0 <- d
    d_1 <- d

    d_0[["V"]] <- 1
    d_1[["V"]] <- 1
    d_0$M <- predict(model_m, newdata=d_0)
    d_1$M <- predict(model_m, newdata=d_1)

    d_0$V <- 0
    d_1$V <- 1

    d_0$Y <- predict(model_y, newdata=d_0)
    d_1$Y <- predict(model_y, newdata=d_1)
    est_TDE <- mean(d_1$Y - d_0$Y)

    #Total indirect effect	(Y1M1−Y1M0)
    d_0 <- d
    d_1 <- d

    d_0[["V"]] <- 0
    d_1[["V"]] <- 1
    d_0$M <- predict(model_m, newdata=d_0)
    d_1$M <- predict(model_m, newdata=d_1)

    d_0$V <- 1
    d_1$V <- 1

    d_0$Y <- predict(model_y, newdata=d_0)
    d_1$Y <- predict(model_y, newdata=d_1)
    est_TIE <- mean(d_1$Y - d_0$Y)

    #Pure direct effect	    (Y1M0−Y0M0)
    d_0 <- d
    d_1 <- d

    d_0[["V"]] <- 0
    d_1[["V"]] <- 0
    d_0$M <- predict(model_m, newdata=d_0)
    d_1$M <- predict(model_m, newdata=d_1)

    d_0$V <- 0
    d_1$V <- 1

    d_0$Y <- predict(model_y, newdata=d_0)
    d_1$Y <- predict(model_y, newdata=d_1)
    est_PDE <- mean(d_1$Y - d_0$Y)

    est_TotalEffect <- est_PIE+est_TDE
    est_TotalEffect2 <- est_TIE+est_PDE
    print(model_m)
    print(model_y)
    return(list(est=est_TotalEffect,
                est_TotalEffect2=est_TotalEffect2,
                est_TIE=est_TIE,
                est_PIE=est_PIE,
                est_TDE=est_TDE,
                est_PDE=est_PDE))
  }
  return(f)
}