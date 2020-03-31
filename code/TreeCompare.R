library(ape)
library(phytools)
library(tidyverse)



tree_reorder <- function(ref_tree, reorder_tree){
  if(!setequal(t1$tip.label,t2$tip.label)){
    print("Trees dont conatain same tips")
    return(NA)
  }
  
}

plot_BL <- function(treeX, treeY){
  bl_table <- tibble(treeX_bl = treeX$edge.length,
                     treeY_bl = treeY$edge.length)
  #return(bl_table)
  bl_lm = lm(treeY_bl~treeX_bl+0, bl_table)
  r2 = summary(bl_lm)$r.squared
  
  treeX_name = "Tree X"
  if(!is.null(treeX$name)){
    treeX_name = treeX$name
  }
  
  treeY_name = "Tree Y"
  if(!is.null(treeY$name)){
    treeY_name = treeY$name
  }
  
  plot_title = paste(treeX_name, "vs.", treeY_name)
  lm_function = paste0("y = ", round(bl_lm$coefficients, 3),"x, R^2=",round(r2,3))
  
  bl_plot <- bl_table %>%
    ggplot(aes(treeX_bl, treeY_bl))+
    geom_point()+
    geom_abline(color = "red")+
    labs(title = plot_title,
         subtitle = lm_function,
         x = paste(treeX_name, "Branch Lenghts"),
         y = paste(treeY_name, "Branch Lenghts"))
  return(list("table" = bl_table, "lm" = bl_lm, "plot" = bl_plot))
}

#load trees as DendList, [1]= ML ,[2]= NJ1, untangles them againist eachother 
makeDendList <- function(tree1,tree2, outgroup){
  d1 <- tree1 %>% root(outgroup,resolve.root = TRUE) %>% force.ultrametric(method = "extend") %>% as.dendrogram()
  d2 <- tree2 %>% root(outgroup,resolve.root = TRUE) %>% force.ultrametric(method = "extend")%>% as.dendrogram()
  myDends <- dendlist(d1,d2)%>%dendextend::untangle(method = "step2side")
  return(myDends)
  #
}


my_tanglegram <- function(tree1, tree2,outgroup, title){
  dends <- makeDendList(tree1, tree2, outgroup)
  dends %>% plot(main = title,
                 margin_inner = 10, #space for names
                 label_cex = .3, #fontSize
                 highlight_distinct_edges  = T,
                 common_subtrees_color_branches = TRUE,
                 highlight_branches_lwd = FALSE,
                 columns_width = c(10,3,10))
}

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
