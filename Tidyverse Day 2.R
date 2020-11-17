library(tidyverse)
setwd("C:/Users/kpkb060/Desktop/R Training/R_tidyverse_intro_data")

# Exercise 4
small_file = read_tsv("small_file.txt")
cv = read_csv("Child_Variants.csv")

# Small file data
filter(small_file, Category == "A")
filter(small_file, Length > 80)
select(small_file, -Sample)

# Child variants data
filter(cv, CHR == "MT")
filter(cv, MutantReadPercent >= 70)
filter(cv, QUAL == 200)
filter(cv, GENE == "IGFN1")
select(cv, -ENST, -dbSNP)

# Exercise 5
# Quality filtering
colnames(cv)
cv %>% filter(QUAL == 200) %>% 
  filter(COVERAGE > 50) %>% 
  filter(MutantReadPercent > 70)

# Positional filtering
cv %>% filter(CHR != "X") %>%
  filter(CHR != "Y") %>%
  filter(CHR != "MT")

# Annotation filtering
cv %>% filter(dbSNP != ".") %>%
  select(CHR, POS)

# Transformation filtering
cv %>% filter(nchar(REF) > 1)
cv %>% filter(substr(GENE, 1, 1) == "Q")

# Exercise 6
# Your first ggplot plot
# The extinct animals tend to be outliers.
bb = read_tsv("brain_bodyweight.txt")
bb %>% ggplot(aes(x = log.brain, y = log.body)) + 
  geom_point(aes(color = Category), size = 3)

# Time permitting
bb %>% filter(Category != "Extinct") %>% 
  ggplot(aes(x = log.brain, y = log.body)) + 
  geom_point(aes(color = Category), size = 3) + 
  geom_text(aes(label = Species), vjust = 1, hjust = 0.5, 
            check_overlap = TRUE) # some labels don't appear

# Exercise 7
# More plotting
small_file %>% filter(Category == "A") %>%
  ggplot(aes(x = Sample, y = Length)) + geom_col() + 
  ggtitle("Length v. Sample") + xlab("Sample") + ylab("Length")

cv %>% ggplot(aes(x = MutantReadPercent)) + 
  geom_density(color = "lightgray", fill = "lightgreen") + 
  ggtitle("Distribution of Mutant Read Percent") + 
  xlab("Mutant Read Percent") + ylab("Density")

cv %>% filter(nchar(REF) == 1) %>% 
  ggplot(aes(x = REF)) + geom_bar() + 
  ggtitle("Bar Plot of REF > 1") + xlab("Reference") + 
  ylab("Count")

# Time permitting
pval = cor.test(bb$log.brain, bb$log.body)$p.value
lm = lm(log.body ~ log.brain, data = bb)
coefs = lm$coefficients
coefs

bb %>% 
  ggplot(aes(x = log.brain, y = log.body)) + 
  geom_point(aes(color = Category), size = 3) + 
  geom_text(aes(label = Species), vjust = 1.2, size = 3) + 
  geom_abline(slope = coefs[2], intercept = coefs[1]) + 
  ggtitle(paste("Relationship between Brain Weight and Body Weight (p = ", 
                signif(pval, 3), ")")) + 
  xlab("Brain weight (log2 g)") + ylab("Body weight (log2 kg)")