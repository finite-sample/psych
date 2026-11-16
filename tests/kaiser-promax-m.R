## Regression test: kaiser(rotate = "Promax", m =) uses m
## The Rd: m is "a parameter to pass to Promax"; pro.m is "A redundant
##   parameter, which is used to replace m in calls to Promax". The Rd's own
##   example is kaiser(fa(Thurstone, 3, rotate = "none"), rotate = "Promax", m = 3).
## The bug: m was never read; Promax always got pro.m (default 4), so the
##   example's m = 3 was silently a power-4 Promax.
## Identity: kaiser normalisation by hand (rows scaled by 1/sqrt(h2), rotated,
##   scaled back) with Promax(m = 3) must equal kaiser(..., m = 3); pro.m, when
##   given, still replaces m.
library(psych)
f <- fa(Thurstone, 3, rotate = "none")
L <- unclass(f$loadings)
h <- sqrt(rowSums(L^2))
by_hand <- function(m) unclass(Promax(L / h, m = m)$loadings) * h
k3 <- kaiser(f, rotate = "Promax", m = 3)
stopifnot(
  "m = 3 gives a power-3 Promax" = isTRUE(all.equal(unclass(k3$loadings), by_hand(3), check.attributes = FALSE)),
  "default (m = 4) unchanged" = isTRUE(all.equal(unclass(kaiser(f, rotate = "Promax")$loadings), by_hand(4), check.attributes = FALSE)),
  "pro.m replaces m when given" = isTRUE(all.equal(unclass(kaiser(f, rotate = "Promax", m = 3, pro.m = 2)$loadings), by_hand(2), check.attributes = FALSE))
)
