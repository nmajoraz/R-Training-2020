library(tidyverse)
setwd("C:/Users/kpkb060/Desktop/R Training/Advanced_R_Data")

# Exercise 5
cancer = read_csv("cancer_stats.csv")
cancer = cancer %>% 
         mutate(Cases = `Male Cases` + `Female Cases`, 
                Deaths = `Male Deaths` + `Female Deaths`)

child = read_csv(file = "Child_Variants.csv", 
                 col_types = cols(CHR = col_character()))
child = child %>% 
        mutate(
          Type = if_else(nchar(REF) & nchar(ALT) == 1, "SNP", "INDEL")
        )

small = read_tsv(file = "small_file.txt", comment = "#")
small %>% 
  group_by(Category) %>% 
  summarize(LengthMean = mean(Length), LengthSD = sd(Length)) %>% 
  ungroup()

child %>% 
  filter(Type == "SNP" & dbSNP == ".") %>% 
  group_by(GENE) %>% 
  summarize(CoverageMean = mean(COVERAGE), Counts = n()) %>% 
  ungroup() %>% 
  filter(Counts >= 3) %>% 
  arrange(desc(CoverageMean))

# time permitting
t2 = read_csv(file = "tidy_data2.csv")

# to long format (from Advanced R Day 1)
t2 = t2 %>% 
  pivot_longer(cols = A:E, names_to = "Sample", 
               values_to = "Value")

t2 %>% 
  group_by(Chr, Sample) %>% 
  summarize(Mean = mean(Value)) %>% 
  ungroup()

cancer %>% 
  mutate(CasesDiff = abs(`Male Cases` - `Female Cases`)) %>% 
  select(Class, Site, `Male Cases`, `Female Cases`, CasesDiff) %>% 
  arrange(CasesDiff) %>% 
  slice(1)

cancer %>% 
  filter(!is.na(`Male Cases`) & !is.na(`Female Cases`)) %>% 
  mutate(SurvivalRateMale = `Male Deaths`/`Male Cases`,
         SurvivalRateFemale = `Female Deaths` / `Female Cases`,
         SurvivalRateDiff = abs(SurvivalRateMale - SurvivalRateFemale)) %>% 
  arrange(desc(SurvivalRateDiff)) %>% 
  select(Class, Site, SurvivalRateMale, SurvivalRateFemale, SurvivalRateDiff) %>% 
  slice(1)

cancer %>% 
  mutate(SurvivalRateOverall = (Cases - Deaths)/Cases) %>% 
  arrange(desc(SurvivalRateOverall)) %>% 
  group_by(Class) %>% 
  slice(1) %>% 
  select(Class, Site, SurvivalRateOverall) %>% ungroup()

child %>% 
  group_by(GENE, CHR) %>% 
  summarize(Variants = n()) %>% 
  arrange(desc(Variants)) %>% 
  group_by(CHR) %>% 
  slice(1)

# Exercise 6
dna_meth = read_csv(file = "dna_methylation.csv")
meth_annotation = read_tsv(file = "methylation_annotation.txt")

dna_meth %>% 
  pivot_wider(names_from = State, values_from = Count) %>% 
  mutate(MethPercent = Meth / (Meth + Unmeth) * 100) %>% 
  group_by(Gene, Group) %>% 
  summarize(MethMean = mean(MethPercent)) %>% 
  ungroup() %>% 
  rename(Gene_name = Gene) %>% 
  left_join(meth_annotation)

# time permitting
child %>% 
  mutate(Variant = str_c(REF, ">" , ALT)) %>% 
  group_by(Variant) %>%
  summarize(Count = n()) %>% 
  ungroup() %>% 
  arrange(desc(Count)) 

small %>% 
  group_by(Category) %>% 
  summarize(min_length = min(Length)) %>% 
  right_join(small) %>% 
  mutate(Normalized_Length = Length - min_length) %>% 
  ggplot(aes(x = Category, y = Normalized_Length)) + 
    geom_boxplot() + ylab("Normalized Length")

# Exercise 7
lowestQuality = function(gene = "AGRN"){
  child %>% 
    filter(GENE == gene) %>% 
    arrange(QUAL) %>% 
    select(GENE, QUAL) %>% 
    slice(1)
}

lowestQuality()

lowestQuality = function(tibble){
  tibble %>% 
    arrange(QUAL) %>% 
    select(GENE, QUAL) %>% 
    slice(1)
}

child %>% lowestQuality()

child %>% 
  group_by(GENE) %>% 
  lowestQuality() # do(lowestQuality(.))
