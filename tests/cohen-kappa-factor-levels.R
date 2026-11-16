## Regression test: cohen.kappa() weights factor input by the factor's level order
## The Rd: x is "a two by n data with categorical values from 1 to p", and
##   weights grow with the distance between categories; `levels` fixes the
##   category set. Factor input is not described, so this is a proposal.
## The bug: factor input was converted to character and re-levelled
##   alphabetically, so weighted kappa measured distances in the wrong order.
##   Ordered labels never < rarely < sometimes < often < always were weighted
##   as always, never, often, rarely, sometimes: weighted kappa 0.718 instead
##   of 0.885 on the data below. Codes 1..10 stored as a factor were ordered
##   1, 10, 2, ..., 9: 0.700 instead of 0.966. Unweighted kappa is unaffected.
## Identity: a factor whose levels are in the scale's order must give the same
##   weighted kappa as the integer codes 1..p, and as passing levels = .
library(psych)
set.seed(6)
r1 <- sample(1:5, 200, TRUE, prob = c(.1, .2, .3, .25, .15))
r2 <- pmin(5, pmax(1, r1 + sample(-1:1, 200, TRUE, prob = c(.2, .6, .2))))
lab <- c("never", "rarely", "sometimes", "often", "always")
num <- cohen.kappa(cbind(r1, r2))
fac <- data.frame(a = factor(lab[r1], levels = lab), b = factor(lab[r2], levels = lab))
kf <- cohen.kappa(fac)
stopifnot(
  "ordered labels: weighted kappa equals integer codes" =
    isTRUE(all.equal(kf$weighted.kappa, num$weighted.kappa)),
  "ordered labels: unweighted kappa unchanged" =
    isTRUE(all.equal(kf$kappa, num$kappa)),
  "ordered labels: same as levels =" =
    isTRUE(all.equal(kf$weighted.kappa,
                     cohen.kappa(fac, levels = lab)$weighted.kappa)),
  "ordered labels: confidence bounds equal integer codes" =
    isTRUE(all.equal(unname(kf$confid), unname(num$confid)))
)

set.seed(7)
a <- sample(1:10, 300, TRUE)
b <- pmin(10, pmax(1, a + sample(-1:1, 300, TRUE)))
stopifnot(
  "codes 1..10 as factors: weighted kappa equals the numbers" =
    isTRUE(all.equal(cohen.kappa(data.frame(a = factor(a), b = factor(b)))$weighted.kappa,
                     cohen.kappa(cbind(a, b))$weighted.kappa))
)

## character input keeps its current (alphabetical) behaviour
ch <- data.frame(a = lab[r1], b = lab[r2], stringsAsFactors = FALSE)
stopifnot(
  "character input unchanged" =
    isTRUE(all.equal(cohen.kappa(ch)$weighted.kappa,
                     cohen.kappa(ch, levels = sort(lab))$weighted.kappa))
)
