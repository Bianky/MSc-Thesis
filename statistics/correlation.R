
# CORRELATION ------------------------------------------------------------------
# ------------------------------------------------------------------------------

num_data <- data[sapply(data, is.numeric)]
result <- rcorr(as.matrix(num_data), type = "spearman")

r <- result$r
p <- result$P

r_sig <- r
r_sig[p >= 0.05] <- NA

corrplot(
  r_sig,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  na.label = " "
)

