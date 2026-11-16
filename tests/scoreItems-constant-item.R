## Regression test: scoreItems() with an item that has no variance
## The documented behaviour (scoreItems' own warning, delete = TRUE by
##   default): the item "had no variance and was deleted from the data and
##   the keys."
## The bug: the constant item was removed from items and keys, and later the
##   same column index was removed from items a second time, which dropped a
##   different, valid item. From 2.2.9 to 2.6.9 scoreItems() stops with
##   "subscript out of bounds" (here, and on 50 complete rows of
##   psychTools::bfi[, 1:10] with A3 set to 5 for everyone). In 2.0.12 it ran
##   but silently left the reverse-keyed V4 out of item.cor.
## Identity: scoring with the constant item present must equal scoring the
##   same data with that item removed from the data and the keys.
library(psych)
set.seed(23)
n <- 80
f <- matrix(rnorm(n * 2), n)
x <- sapply(1:8, function(j) pmin(6, pmax(1, round(3.5 + f[, (j > 4) + 1] + rnorm(n)))))
colnames(x) <- paste0("V", 1:8)
x[, "V3"] <- 5 # everyone gave the same answer
keys <- list(one = c("V1", "V2", "V3", "-V4"), two = c("V5", "V6", "-V7", "V8"))
keys_without <- list(one = c("V1", "V2", "-V4"), two = c("V5", "V6", "-V7", "V8"))

with_item <- suppressWarnings(scoreItems(keys, x))
without <- scoreItems(keys_without, x[, -3])
stopifnot(
  "scores equal scoring without the constant item" =
    isTRUE(all.equal(unname(with_item$scores), unname(without$scores))),
  "alpha equals alpha without the constant item" =
    isTRUE(all.equal(c(with_item$alpha), c(without$alpha))),
  "item.cor rows are the remaining items" =
    identical(rownames(with_item$item.cor), colnames(x)[-3]),
  "item.cor equals scoring without the constant item" =
    isTRUE(all.equal(unname(with_item$item.cor), unname(without$item.cor))),
  "item.cor is cor(item, score)" =
    isTRUE(all.equal(unname(with_item$item.cor["V1", "one"]),
                     cor(x[, "V1"], with_item$scores[, "one"])))
)
