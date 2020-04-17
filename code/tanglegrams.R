library(ape)
library(phytools)
library(tidyverse)
library(dendextend)


#load trees into DendList, untangles them againist eachother 
makeDendList <- function(tree1,tree2, outgroup){
  d1 <- tree1 %>% root(outgroup,resolve.root = TRUE) %>% force.ultrametric(method = "extend") %>% as.dendrogram()
  d2 <- tree2 %>% root(outgroup,resolve.root = TRUE) %>% force.ultrametric(method = "extend")%>% as.dendrogram()
  myDends <- dendlist(d1,d2)%>%dendextend::untangle(method = "step2side")
  return(myDends)
  #
}

#input is 2 trees, their common outgroup and the title of the plot
my_tanglegram_addBL <- function(tree1, tree2,outgroup, title){
  n_BL = length(tree1$edge)
  tree1$edge.length <- rep(1,n_BL)
  tree2$edge.length <- rep(1,n_BL)
  dends <- makeDendList(tree1, tree2, outgroup)
  dends %>% plot(main = title,
                 margin_inner = 10, #space for names
                 highlight_distinct_edges  = T,
                 common_subtrees_color_branches = TRUE,
                 highlight_branches_lwd = FALSE,
                 columns_width = c(10,3,10))
}