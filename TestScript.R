library(tidyverse)
library(scholar)
library(ggraph)
library(tidygraph)

Ids <- c("AjOjxAsAAAAJ&hl", "GVb9deIAAAAJ", "dtfZ0wcAAAAJ")


coauthor_network <- Ids %>% 
  purrr::map(get_coauthors) %>% 
  reduce(bind_rows)

plot_coauthors(coauthor_network) + ggtitle("")
