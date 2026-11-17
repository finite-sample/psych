## Regression test: setCor() passes alpha on to lmCor()
## The Rd (lmCor.Rd, which documents setCor with the same arguments):
##   alpha is the "p value of the confidence intervals for the beta
##   coefficients".
## The bug: the deprecated wrapper setCor() accepted alpha but called lmCor()
##   without it, so the intervals were always 95%. setCor(SATQ ~ ACT + age,
##   data = sat.act, alpha = .01) reported the 95% interval for ACT.
## Identity: setCor(alpha = a) must equal lmCor(alpha = a), and with
##   std = FALSE its interval must equal confint(lm(), level = 1 - a).
library(psych)
d <- na.omit(sat.act)
s <- suppressWarnings(setCor(SATQ ~ ACT + age, data = d, std = FALSE, alpha = .01, plot = FALSE))
l <- lmCor(SATQ ~ ACT + age, data = d, std = FALSE, alpha = .01, plot = FALSE)
ci <- confint(lm(SATQ ~ ACT + age, data = d), level = .99)
stopifnot(
  "setCor equals lmCor with the same alpha" = isTRUE(all.equal(s$ci, l$ci)),
  "99% interval equals confint(lm)" =
    isTRUE(all.equal(unname(l$ci["ACT", ]), unname(ci["ACT", ]))),
  "setCor 99% interval equals confint(lm)" =
    isTRUE(all.equal(unname(s$ci["ACT", ]), unname(ci["ACT", ])))
)
