require(ineq)

summary_stats <- function(x, y, name1, name2) {
  
  result = data.frame(V0 = c("Mean", "Median", "IQR", "Coefficient of variation", "Gini index", 
                              "Theil index"),
                      V1 = c(round(mean(x),2), round(median(x), 2), round(IQR(x),2),
                             round(var.coeff(x),2), round(Gini(x),2), round(Theil(x),2)), 
                      V2 = c(round(mean(y),2), round(median(y), 2), round(IQR(y),2),
                             round(var.coeff(y),2), round(Gini(y),2), round(Theil(y),2)))
  colnames(result) <- c("geo", name1, name2)
  
  return(result)
}
