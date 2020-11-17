# Exercise 1
31 * 78
697 / 41

x = 39
y = 22
z = x / y
z

log2(sqrt(2345))

my_name = "Nicole"
nchar(my_name)
substr(x = my_name, start = 1, stop = 1)

# Exercise 2 
# Making vectors and doing vectorized math
some.numbers = c(2, 5, 8, 12, 16)
number.range = 5:9
some.numbers - number.range

# Vector creating functions
number.series = seq(from = 2, by = 3, length.out = 100)
number.series
number.series = 1000 * number.series
number.series
rep(c("WT", "K01", "K02", "K03"), each = 25, length.out = 100)

# Statistical functions to create and test vectors
set.seed(5756800)
normal.numbers = rnorm(20)
t.test(normal.numbers) # not statistically significant
normal.numbers = rnorm(20, mean = 1)
t.test(normal.numbers) # statistically significant

# Time permitting
medD = number.series - median(number.series)
mean(medD) # mean = 0
sd(medD) # sd = 8,7034.48
vec1 = rnorm(5000, mean = 10, sd = 2)
vec2 = rnorm(5000, mean = 10.1, sd = 2)
t.test(vec1, vec2)

# Exercise 3
library(tidyverse)

# Reading a small file
setwd("C:/Users/kpkb060/Desktop/R Training/R_tidyverse_intro_data")
small_file = read_tsv("small_file.txt")
median(log2(small_file$Length))

# Reading a larger file
cv = read_csv("Child_Variants.csv")
View(cv)
mean(cv$MutantReadPercent)
sd(cv$MutantReadPercent)