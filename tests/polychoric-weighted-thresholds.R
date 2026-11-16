## Regression test: polychoric(weight =) uses weighted thresholds
## The Rd (tetrachor.Rd): weight is "A vector of length of the number of
##   observations that specifies the weights to apply to each case."
## The bug: with weights, each pairwise table was weighted, but the global
##   thresholds tau (global = TRUE, the default) were found from unweighted
##   category counts. The reported tau ignored the weights entirely and rho
##   was fitted to a weighted table with unweighted cut points. On 500 rows of
##   psychTools::bfi A1-A5 with men weighted 3 and women 1, tau for A1 was
##   -0.41 0.30 0.79 1.21 1.85 (the unweighted values) against
##   -0.55 0.17 0.67 1.07 1.70 with each man's row entered three times, and
##   rho(A1, A3) was -0.337 against -0.326. tetrachoric() already weighted tau.
## Identity: an integer weight w on a case is the same as entering the case
##   w times, so tau and rho must equal the unweighted fit to duplicated rows.
library(psych)
set.seed(17)
n <- 400
f <- rnorm(n)
z <- sapply(1:4, function(j) .6 * f + rnorm(n, sd = .8))
x <- apply(z, 2, function(v) cut(v, c(-Inf, -1, -.3, .4, 1.1, Inf), labels = FALSE))
colnames(x) <- paste0("V", 1:4)
w <- ifelse(f > 0, 3, 1) # weights related to the trait, as survey weights often are
xd <- x[rep(seq_len(n), w), ]

pw <- polychoric(x, weight = w)
pd <- polychoric(xd)
tau_w <- qnorm(sapply(1:4, function(k) sum(w[x[, 1] <= k]) / sum(w)))
stopifnot(
  "weighted tau equals weighted cumulative proportions" =
    isTRUE(all.equal(unname(pw$tau[1, ]), tau_w)),
  "weighted tau equals tau of duplicated rows" =
    isTRUE(all.equal(unname(pw$tau), unname(pd$tau))),
  "weighted rho equals rho of duplicated rows" =
    max(abs(pw$rho - pd$rho)) < 1e-4,
  "unit weights equal no weights" =
    max(abs(polychoric(x, weight = rep(1, n))$rho - polychoric(x)$rho)) < 1e-8
)
