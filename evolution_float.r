random_vec <- function(length) runif(length)
random_pop <- function(size, length) matrix(runif(length * size), nrow = size)
crossover <- function(a, b, cut) c(a[1:cut], b[(cut + 1):length(a)])
mutate <- function(a, cut) c(if (cut > 1) a[1:(cut - 1)] else NULL, runif(1), if (cut < length(a)) a[(cut + 1):length(a)] else NULL)
best <- function(pop) pop[which.min(rowSums(pop)), ]

next_gen <- function(pop) {
  children <- t(apply(pop, 1, function(row) crossover(row, pop[sample(seq_len(nrow(pop)), 1), ], sample(length(row) - 1, 1))))
  mutants <- t(apply(children, 1, function(row) mutate(row, sample(length(row), 1))))
  result <- t(mapply(function(i) best(rbind(pop[i, ], children[i, ], mutants[i, ])), seq_len(nrow(pop))))

  return(result)
}

population <- random_pop(20, 20)
result <- Reduce(function(x, ...) next_gen(x), 1:50, init = population)

print(round(best(result), 0))
