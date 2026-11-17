## Regression test: corr.test(normal = FALSE) honours use = "complete"
## The Rd: 'use="complete" will select just complete cases.'
## The bug: with normal = FALSE (rank-based p-values for Spearman and
##   Kendall) r and p both came from cor.test() pair by pair on all available
##   cases, whatever `use` said, and n was the pairwise count. On sat.act
##   with ACT missing in rows 1:20 and SATV in rows 21:40, Spearman
##   r(ACT, SATQ) was 0.6143 (p 1.9e-70, n 667) where the 647 complete cases
##   give 0.6241 (p 3.8e-71).
## Identity: on complete cases, r and p must equal stats::cor.test(method =,
##   exact = FALSE) on na.omit(x); with the default use = "pairwise" they must
##   equal cor.test on each pair's available cases, as before.
library(psych)
x <- sat.act[, c("ACT", "SATV", "SATQ")]
x$ACT[1:20] <- NA
x$SATV[21:40] <- NA
cc <- na.omit(x)
for (m in c("spearman", "kendall")) {
  ct <- suppressWarnings(corr.test(x, use = "complete", method = m, normal = FALSE, adjust = "none"))
  o <- suppressWarnings(cor.test(cc$ACT, cc$SATQ, method = m, exact = FALSE))
  stopifnot(
    "r is the complete-case r" = isTRUE(all.equal(ct$r["SATQ", "ACT"], unname(o$estimate))),
    "p matches cor.test on complete cases" = abs(ct$p["SATQ", "ACT"] / o$p.value - 1) < 1e-8,
    "n is the number of complete cases" = all(ct$n == nrow(cc))
  )
  cp <- suppressWarnings(corr.test(x, method = m, normal = FALSE, adjust = "none"))
  op <- suppressWarnings(cor.test(x$ACT, x$SATQ, method = m, exact = FALSE))
  stopifnot("pairwise (default) p unchanged" = abs(cp$p["SATQ", "ACT"] / op$p.value - 1) < 1e-8)
}
