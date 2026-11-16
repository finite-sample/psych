## Regression test: kurtosi(type = 1) on a data frame or matrix with missing values
## The Rd (skew.Rd, describe.Rd): "Type 1 finds skewness and kurtosis by
##   g_1 = m_3/(m_2)^{3/2} and g_2 = m_4/(m_2)^2 - 3", with m_k the moments of the observed (non-missing) values.
## The bug: for a matrix or data frame, type 1 multiplied by the number of
##   rows (missing ones included) instead of the number of non-missing values,
##   so g2 + 3 was inflated by N / n. describe(type = 1) inherits this. On
##   psychTools::bfi$education (223 of 2800 missing), describe(type = 1)
##   reported -0.09 where e1071::kurtosis(type = 1) and moments give -0.32.
##   kurtosi() on the same column as a vector was already right.
## Identity: g2 by hand on the non-missing values, and equality with the
##   vector method.
library(psych)
x <- sat.act[, c("ACT", "SATQ")] # SATQ has 13 missing values
v <- na.omit(sat.act$SATQ)
d <- v - mean(v)
g2 <- length(v) * sum(d^4) / sum(d^2)^2 - 3
stopifnot(
  "vector method is g2" = isTRUE(all.equal(kurtosi(sat.act$SATQ, type = 1), g2)),
  "data frame method is g2" = isTRUE(all.equal(unname(kurtosi(x, type = 1)["SATQ"]), g2)),
  "matrix method is g2" = isTRUE(all.equal(unname(kurtosi(as.matrix(x), type = 1)[2]), g2)),
  "describe(type = 1) is g2" = isTRUE(all.equal(describe(x, type = 1)["SATQ", "kurtosis"], g2)),
  "complete column unchanged" = isTRUE(all.equal(unname(kurtosi(x, type = 1)["ACT"]),
                                               kurtosi(sat.act$ACT, type = 1)))
)
