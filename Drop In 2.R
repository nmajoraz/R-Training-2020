library(tidyverse)

# Merging Datasets Function
ex1 = tibble("gene" = c("ABC1", "DEF1", "GHI1"), count = c(10, 20, 30))
ex2 = tibble("gene_name" = c("ABC1", "DEF1", "GHI1"), day = c("Monday", "Tuesday", "Wednesday"))

join_data = function(data1, data2, fromcol, tocol){
  join2 = tocol
  names(join2) = fromcol
  
  print(join2)
  full_join(data1, data2, by = join2)
}

join_data(ex1, ex2, "gene", "gene_name")

# Intersect function - output is which columns appear in both tibbles.
intersect(colnames(ex1), colnames(ex2)) 

# Lag and Lead functions
x = 1:10
y = c(5, 7, 2, 4)
t = tibble(behind = lag(x), at = x, ahead = lead(x))

tibble(at = x) %>% ``
  add_row() %>% 
  mutate(behind = lag(at))

# Saving an R Object. Only R readable. 
saveRDS() # (object, file = "...")

# Saving an image
t %>% 
  ggplot(aes(x = behind, y = ahead)) + geom_point() -> useless

ggsave(filename = "useless.png", plot = useless, device = "png") # method 1
