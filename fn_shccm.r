# --- Summary regression table with Heteroscedasticity-consistent standard errors ---

shccm <- function(model, type=c("hc0", "hc1", "hc2", "hc3", "hc4")){
  
  # R-code (www.r-project.org) for computing
  # HC standard errors for a linear model (lm).
  
  # The arguments of the function are:
  # model = a model fitted with lm()
  # type  = one of "hc0" to "hc4", refer to package hccm in the car library for a description
  
  # Example: shccm(my.lm.model, "hc0")
  
  if (!require(car)) stop("Required car package is missing.")
  type <- match.arg(type)
  V <- hccm(model, type=type)
  sumry <- summary(model)
  table <- coef(sumry)
  table[,2] <- sqrt(diag(V))
  table[,3] <- table[,1]/table[,2]
  table[,4] <- 2*pt(abs(table[,3]), df.residual(model), lower.tail=FALSE)
  sumry$coefficients <- table
  p <- nrow(table)
  hyp <- if (names(model$coef)[1]=="(Intercept)") cbind(0, diag(p - 1)) else diag(p)
  sumry$fstatistic[1] <- linearHypothesis(model, hyp, white.adjust=type)[2,"F"]
  print(sumry)
  cat("Note: Heteroscedasticity-consistent standard errors using adjustment", type, "\n") 
  return(invisible(sumry))
  }


 