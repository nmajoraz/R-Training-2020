library(tidyverse)
setwd("C:/Users/kpkb060/Desktop/R Training/Advanced_R_Data")

# Exercise 1
small = read_tsv(file = "small_file.txt", comment = "#")
smallF = read_tsv(file = "small_file.txt", comment = "#", 
                  col_types = cols(Category = col_factor()))

# CHR is being read as a double when it should be a character
child = read_csv(file = "Child_Variants.csv", 
                 col_types = cols(CHR = col_character()))

child %>% ggplot(aes(x = CHR, y = COVERAGE)) + geom_point() + 
  xlab("Chromosome") + ylab("Coverage") + 
  ggtitle("Coverage by Chromosome")

# Exercise 2
genomes = read_csv(file = "genomes.csv")
genomes %>% filter(Chromosomes > 40)
genomes %>% filter(Plasmids > 0 & Chromosomes > 1)
genomes %>% arrange(desc(Size)) %>% slice(1:10)
genomes %>% select(-Groups)
genomes %>% select(starts_with("O"))
genomes %>% distinct(Groups) %>% nrow()

# time permitting
genomes %>% filter(Chromosomes != 0) %>% arrange(Size) %>% 
  distinct(Chromosomes, .keep_all = TRUE) %>% 
  ggplot(aes(x = Chromosomes, y = log(Size))) + geom_point() + 
  ggtitle("Chrosomes by log(Size)")

# Exercise 3
cancer = read_csv("cancer_stats.csv")
cancer %>% filter(Class == "Digestive System" & 
                    `Female Cases` > `Male Cases`)
cancer %>% 
  filter(is.na(`Male Cases`) & is.na(`Male Deaths`)) %>% 
  select(Class, Site)
cancer %>% 
  filter(is.na(`Female Cases`) & is.na(`Female Deaths`)) %>% 
  select(Class, Site)
cancer %>% arrange(`Male Deaths` / `Male Cases`) %>% 
  select(-starts_with("Female")) %>% slice(1)
cancer %>% filter(str_detect(Site, "acute"))

tissues = c("tongue", "kidney", "breast", "pancreas")
cancer %>% 
  filter(Class == "soft tissue" & tolower(Site) %in% tissues)

# time permitting
cancer %>% filter(nchar(Site) <=4)
cancer %>% filter(endsWith(Site, "y"))

# Exercise 4
t1 = read_csv(file = "tidy_data1.csv")
t1 = t1 %>% pivot_longer(cols = everything(), names_to = "Sample", 
             values_to = "Value") %>% filter(!is.na(Value))

t2 = read_csv(file = "tidy_data2.csv")
t2md = t2 %>% select(ID:Strand) %>% 
  distinct(ID, .keep_all = TRUE)
t2 = t2 %>% select(-(Chr:Strand)) %>% 
  pivot_longer(cols = A:E, names_to = "Sample", 
                         values_to = "Value")

t3 = read_csv(file = "tidy_data3.csv")
t3md = t3 %>% select(Probe_ID:Symbol) %>% 
  distinct(Probe_ID, .keep_all = TRUE)
t3 = t3 %>% select(-Symbol) %>% 
  pivot_longer(cols = WT_1:KO_3, names_to = "Sample", 
               values_to = "Values") %>% 
  separate(col = Sample, into = c("Genotype", "Replication"), 
           convert = TRUE, sep = "_")

# time permitting
genomes %>% 
  separate(col = Groups, 
           into = c("Domain", "Kingdom", "Class"), sep = ";") %>% 
  filter(!str_detect(Organism, "'") & Kingdom != "Other" & 
           Class != "Other")
