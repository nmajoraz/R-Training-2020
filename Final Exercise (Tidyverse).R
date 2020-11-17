library(tidyverse)
setwd("C:/Users/kpkb060/Desktop/R Training/ggplot_data_files")
theme_set(theme_minimal())

# Section 1
meth = read_tsv("methylation.txt")
expression = read_tsv("expression.txt")
final.annot = read_tsv("final_annotation.txt", 
                       col_types = cols(Chromosome = col_character()))

# Combine into a single tibble
meth %>% 
  pivot_wider(names_from = "Region", 
              values_from = "Methylation") %>% 
  rename(Gene_body_meth = Gene_body, 
         Promoter_meth = Promoter) -> meth

expression %>% 
  rename(Probe = Gene) %>% 
  inner_join(meth) %>% left_join(final.annot) %>% 
  mutate(ExpressionCat = if_else(Expression > 0, "High", "Low")) -> merged.data

# Data cleaning
merged.data %>% 
  filter(End - Start >= 10 * 1000) %>% 
  filter(Promoter_meth != -1) %>% 
  filter(substr(tolower(Probe), 1, 2) != "gm") -> final

# Summarizations
final %>% 
  group_by(Chromosome) %>% 
  summarize(avgExpression = mean(Expression), 
            avgGeneBody = mean(Gene_body_meth), 
            avgPromoter = mean(Promoter_meth),
            geneCount = n()) %>% 
  ungroup() -> table1

final %>% 
  group_by(Chromosome, Strand) %>% 
  summarize(Genes = n()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = "Strand", values_from = "Genes") %>% 
  mutate(Difference = `+` - `-`) -> table2


final %>% 
  group_by(ExpressionCat) %>% 
  summarize(medianGeneBody = median(Gene_body_meth),
            medianPromoter = median(Promoter_meth)) -> table3

# Section 2

# Distributions
meth %>% 
  pivot_longer(cols = Gene_body_meth:Promoter_meth, 
               names_to = "Meth", values_to = "Value") %>% 
  ggplot(aes(x = Value)) + 
    geom_density(color = "lightblue4", fill = "lightblue") + 
    facet_wrap(vars(Meth)) + 
    ylab("Density") + ggtitle("Distributions of Meth by Region")

expression %>% 
  ggplot(aes(x = Expression)) + 
    geom_density(color = "red4", fill = "pink") + 
    ylab("Density") + ggtitle("Distribution of Expression") +
    geom_vline(xintercept = 0)

# Summaries
merged.data %>% 
  group_by(Chromosome) %>% 
  summarize(meanGBM = mean(Gene_body_meth)) %>% 
  ungroup() %>% 
  ggplot(aes(x = reorder(Chromosome, meanGBM), y = meanGBM)) + geom_col() + coord_flip()

# Creates a strip chart split by Chromosome of the promoter meth
# levels in either the 500 highest or lowest values. 
# Specified by he variable (valid values are High and Low)
stripChart = function(tibble, he){
  tibble %>% 
    filter(ExpressionCat == he) -> temp
    
    if (he == "High"){
      temp %>% 
        arrange(desc(Expression)) %>% 
        slice(1:500) %>% 
        ggplot(aes(x = Chromosome, y = Promoter_meth)) + 
          geom_point() + 
          ggtitle("Promoter Meth Levels by Chromosome", 
                  subtitle = "500 Highest Expressed Genes") + 
          ylab("Promoter Meth")
    }
  
    else if (he == "Low"){
      temp %>% 
        arrange(Expression) %>% 
        slice(1:500) %>% 
        ggplot(aes(x = Chromosome, y = Promoter_meth)) + 
          geom_point() + 
          ggtitle("Promoter Meth Levels by Chromosome", 
                  subtitle = "500 Lowest Expressed Genes") + 
          ylab("Promoter Meth")
    }
  
    else {
      print("Invalid value for ExpressionCat")
    }
}

merged.data %>% 
  stripChart("High")

merged.data %>% 
  stripChart("Low")

# Comparisons
head(final)
final %>% 
  ggplot(aes(x = Promoter_meth, y = Gene_body_meth)) + 
    geom_point(aes(color = Expression)) + 
    scale_color_distiller(palette = "RdBu", direction = -1) + 
    ggtitle("Gene body meth vs. Promoter meth by Expression") + 
    xlab("Promoter meth") + ylab("Gene body meth")

final %>% 
  ggplot(aes(x = Expression, y = Gene_body_meth)) + 
    geom_point() + geom_density_2d() + 
    ggtitle("Gene body meth vs. Expression") + 
    ylab("Gene body meth")

final %>% 
  ggplot(aes(x = ExpressionCat, y = Gene_body_meth)) + 
    geom_violin(fill = "salmon", color = "salmon3") + 
    ggtitle("Violin Plot of Gene Body Meth by Expression") + 
    xlab("Expression") + ylab("Gene Body Meth")

final %>% 
  group_by(ExpressionCat) %>% 
  summarize(meanGBM = mean(Gene_body_meth), 
            sdGBM = sd(Gene_body_meth)) %>% 
  ggplot(aes(x = ExpressionCat, y = meanGBM)) + 
    geom_col() + 
    geom_errorbar(aes(ymin = meanGBM - sdGBM, 
                      ymax = meanGBM + sdGBM)) + 
    ggtitle("Mean Gene Body Meth for Expression Groups") + 
    xlab("Expression") + ylab("Mean Gene Body Meth")