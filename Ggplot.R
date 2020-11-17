library(tidyverse)
library(ggrepel)
setwd("C:/Users/kpkb060/Desktop/R Training/ggplot_data_files")
theme_set(theme_bw(base_size = 12))

# Exercise 1
weightChart = read_tsv("weight_chart.txt")
weightChart %>% 
  ggplot(aes(x = Age, y = Weight)) + 
    geom_point(color = "blue2", size = 3) + 
    geom_line() + ggtitle("Weight vs. Age")

chromosomePosition = read_tsv("chromosome_position_data.txt")
chromosomePosition %>% 
  pivot_longer(cols = Mut1:WT, names_to = "Sample", 
               values_to = "Value") %>% 
  ggplot(aes(x = Position, y = Value, color = Sample)) + 
    geom_line(size = 0.75) + 
    ggtitle("Value vs. Position by Sample")

genomes = read_csv("genomes.csv")
genomes %>% separate(Groups, c("Domain", "Kingdom", "Class"), 
                     sep = ";") %>% 
            ggplot(aes(x = log10(Size), y = Chromosomes, color = Domain)) + 
              geom_point() + 
              xlab("Log10 Size") + 
              ggtitle("Number of Chromosomes vs. Log of Size by Domain")

# Exercise 2
small = read_tsv(file = "small_file.txt", comment = "#")

small %>% 
  filter(Category == "A") %>% 
  ggplot(aes(x = Sample, y = Length)) + geom_col() + 
    ggtitle("Length vs. Sample for Category A")

small %>% 
  ggplot(aes(x = Category, y = Length, color = Category)) + 
    geom_jitter(height = 0, width = 0.3, show.legend = FALSE) + 
    ggtitle("Length vs. Category Jitter Plot")

expression = read_tsv(file = "expression.txt")
expression %>% 
  ggplot(aes(x = Expression)) + 
    geom_density(color = "gray", fill = "lightblue") + 
    ggtitle("Distribution of Expression") + ylab("Density")

cancer = read_csv(file = "cancer_stats.csv")
cancer %>% 
  select(Site, `Male Deaths`) %>% 
  arrange(desc(`Male Deaths`)) %>% 
  slice(1:5) %>% 
  ggplot(aes(x = Site, y = `Male Deaths`)) + geom_col() + 
  ggtitle("Sites of Top 5 Cancers Lethal to Men")

# time permitting
child = read_csv(file = "Child_Variants.csv", 
                 col_types = cols(CHR = col_character()))
child = child %>% 
  mutate(Good = if_else(QUAL == 200, "GOOD", "BAD"))

child %>% 
  ggplot(aes(x = Good, y = log(MutantReads))) + geom_violin() + 
  ggtitle("Log of MutantReads vs. Quality Classification") + 
  xlab("Quality Classification") + ylab("Log MutantReads")

# Exercise 3
cancer %>% 
  filter(!is.na(`Male Deaths`)) %>% 
  select(Site, `Male Deaths`) %>% 
  ggplot(aes(x = reorder(Site, `Male Deaths`), y = `Male Deaths`)) + geom_col() + 
  coord_flip() + ggtitle("Site of Cancer vs. Male Deaths")

bb = read_tsv(file = "brain_bodyweight.txt")
bb %>% 
  mutate(Category = 
           factor(Category, levels = c("Domesticated", "Wild", "Extinct"))) %>% 
  ggplot(aes(x = brain, y = body, color = Category)) + 
  geom_point() + scale_color_brewer(palette = "Set1") + 
  scale_x_log10() + scale_y_log10() + 
  xlab("Brain weight (log g)") + ylab("Body weight (log kg)") + 
  ggtitle("Body weight vs. Brain weight")

bb %>% 
  ggplot(aes(x = Species, y = log(brain), fill = log(body))) + 
    geom_bar(stat = "summary", fun = "sum") + xlab("Log Brain") + 
    scale_fill_distiller(palette = "YlGnBu") + coord_flip()

# Exercise 4
td1 = read_csv(file = "tidy_data1.csv")
td1 = td1 %>% pivot_longer(cols = everything(), names_to = "Sample", 
                               values_to = "Value") %>% 
      filter(!is.na(Value))

td1 %>% 
  ggplot(aes(x = Sample, y = Value, color = Sample)) + 
    geom_boxplot(size = 1, color = "lightgray") + 
    geom_jitter(size = 1) + ggtitle("Value vs. Condition") + 
    xlab("Condition") + scale_color_brewer(palette = "Set2")

td1 %>% 
  ggplot(aes(x = Sample, y = Value)) + 
    geom_bar(stat = "summary") + 
    stat_summary(geom = "errorbar", fun.data = mean_se)

td1Grouped = td1 %>% 
  group_by(Sample) %>% 
  summarize(mean = mean(Value), sd = sd(Value)) %>% 
  ungroup()

td1Grouped %>% 
  ggplot(aes(x = Sample, y = mean)) + geom_col() + 
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd))

# Exercise 5
ude = read_tsv(file = "up_down_expression.txt")

ude %>% 
  filter(Condition1 > -1 & Condition2 > -1 & 
           abs(Condition1 - Condition2) > 3) -> labels

ude %>% 
  ggplot(aes(x = Condition1, y = Condition2, color = State, label = Gene)) + 
    geom_point() + geom_abline(slope = 1, intercept = 0) + 
    geom_text_repel(data = labels, color = "black", hjust = 1) + 
    scale_color_manual(values = c("up" = "red", 
                                  "unchanging" = "gray", 
                                  "down" = "blue")) + 
    theme(legend.position = "none")

df = read_csv("DownloadFestival.csv")
df %>% 
  pivot_longer(cols = day1:day3, names_to = "Day", 
               values_to = "Value") %>% 
  filter(!is.na(Value)) %>% 
  mutate(Day = substr(Day, 4, nchar(Day))) -> df 

df %>% 
  ggplot(aes(x = gender, y = Value, color = gender)) + 
    geom_point() + 
    scale_color_manual(values = c("Male" = "red", 
                                  "Female" = "blue")) + 
    facet_grid(cols = vars(Day)) + 
    stat_summary(geom = "errorbar", fun = mean, 
                 fun.max = mean, fun.min = mean, color = "gray")

# time permitting
df %>% 
  group_by(ticknumb) %>% 
  count() %>% 
  right_join(df) %>% 
  rename(attendance = n) %>% 
  ggplot(aes(x = gender, y = Value, color = gender)) + 
    geom_point() + 
    scale_color_manual(values = c("Male" = "red", 
                                "Female" = "blue")) + 
    facet_grid(cols = vars(Day), 
               rows = vars(attendance)) + 
    stat_summary(geom = "errorbar", fun = mean, 
               fun.max = mean, fun.min = mean, color = "gray")

  
