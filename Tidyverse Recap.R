library(tidyverse)
setwd("C:/Users/kpkb060/Desktop/R Training/R_tidyverse_intro_data")

# Trumpton
trumpton = read_tsv("trumpton.txt")
# As you get older your weight increases, but it's not a very linear relationship.
trumpton %>% ggplot(aes(x = Age, y = Weight)) + geom_point()

trumpton %>% filter(Weight > 100) %>% select(FirstName, LastName)

trumpton %>% 
  ggplot(aes(x = LastName, y = Age)) + 
  geom_col(fill = "magenta3", color = "black") + 
  ggtitle("Age of Each Person by Last Name") + 
  xlab("Last Name") + ylab("Age")

# Child Variants
cv = read_csv("Child_Variants.csv")
chrx = cv %>% filter(CHR == "X" & POS <= 5000000) 

# Poor quality calls have low mutant reads and coverage
chrx %>% ggplot(aes(x = MutantReads, y = COVERAGE)) + 
  geom_point(aes(color = QUAL))

cv %>% filter(CHR == "1" & dbSNP != ".") %>% 
  ggplot(aes(x = POS, y = COVERAGE)) + 
  geom_line(color = "gray", size = 1)

cv %>% 
  filter(CHR == "1" & dbSNP != "." & COVERAGE <= 200) %>% 
  ggplot(aes(x = POS, y = COVERAGE)) + 
  geom_line(color = "gray", size = 1)