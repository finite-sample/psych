## Regression test: corr.test(use = "complete") must use the complete-case n
## The Rd: 'use="complete" will select just complete cases.'
## The bug: the correlations came from the complete cases, but n was set to
##   the smallest pairwise count, which is larger whenever different
##   variables are missing in different rows. On psychTools::bfi[, 1:25]
##   psych reported n = 2739 against 2436 complete cases, so every t, p and
##   confidence interval was computed as if there were 303 more people.
## Identity: on complete cases, each p-value and Fisher-z interval must equal
##   stats::cor.test() on the same rows.
library(psych)
x <- sat.act[, c("ACT", "SATV", "SATQ")]
x$ACT[1:20] <- NA # SATQ is already missing in 13 other rows
x$SATV[21:40] <- NA
cc <- na.omit(x)
stopifnot(nrow(cc) < min(crossprod(!is.na(x))))

ct <- corr.test(x, use = "complete", adjust = "none")
o <- cor.test(cc$ACT, cc$SATQ)
stopifnot(
  "n is the number of complete cases" = ct$n == nrow(cc),
  "r is the complete-case r" = isTRUE(all.equal(ct$r["SATQ", "ACT"], unname(o$estimate))),
  "p matches cor.test on complete cases" =
    abs(ct$p["SATQ", "ACT"] / o$p.value - 1) < 1e-8,
  "CI matches cor.test on complete cases" =
    isTRUE(all.equal(unlist(ct$ci["ACT-SATQ", c("lower", "upper")]),
                     o$conf.int[1:2], check.attributes = FALSE))
)

## the x, y form and R's own spelling "complete.obs"
cxy <- corr.test(x[, "ACT", drop = FALSE], x[, c("SATV", "SATQ")],
                 use = "complete.obs", adjust = "none")
stopifnot(
  "x, y form uses the complete-case n" = all(cxy$n == nrow(cc)),
  "x, y p matches cor.test" = abs(cxy$p[1, "SATQ"] / o$p.value - 1) < 1e-8
)

## pairwise deletion (the default) is unchanged: n stays a matrix of counts
cp <- corr.test(x)
stopifnot("pairwise n unchanged" = identical(cp$n, crossprod(!is.na(x))))
