random_vec <- function(length) round(runif(n = length, min = 0, max = 20), 0)
random_pop <- function(size, length) matrix(round(runif(n = length * size, min = 0, max = 20), 0), nrow = size)
crossover <- function(a, b, cut) c(a[1:cut], b[(cut + 1):length(a)])
mutate <- function(a, cut) c(if (cut > 1) a[1:(cut - 1)] else NULL, sample(21, 1) - 1, if (cut < length(a)) a[(cut + 1):length(a)] else NULL)
best <- function(pop) pop[which.min(rowSums(pop)), ]

next_gen <- function(pop) {
  children <- t(apply(pop, 1, function(row) crossover(row, pop[sample(seq_len(nrow(pop)), 1), ], sample(5, 1))))
  mutants <- t(apply(children, 1, function(row) mutate(row, sample(6, 1))))
  result <- t(mapply(function(i) best(rbind(pop[i, ], children[i, ], mutants[i, ])), seq_len(nrow(pop))))

  return(result)
}

population <- random_pop(10, 6)
result <- Reduce(function(x, ...) next_gen(x), 1:100, init = population)

print(population)
print(result)
print(best(result))
