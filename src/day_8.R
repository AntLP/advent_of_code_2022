library(tidyverse)


input_test <- readLines("./data/day_8_test.txt")
input <- readLines("./data/day_8_input.txt")

process_input <- function(input){
  input %>% 
    str_split("", simplify = T) %>% 
    apply(c(1, 2), as.numeric)
  
}

is_tallest <- function(vector){
  # Tree is visible if
  #   It is the tallest from the given direction and
  #   the second tallest is strictly smaller
  return(vector[1] == max(vector) & length(vector[vector == max(vector)]) == 1)
  
}

is_visible_vector <- function(id, tree_vector){
  dir1 <- tree_vector[1:id]
  dir2 <- tree_vector[id:length(tree_vector)]
  
  return(is_tallest(rev(dir1))|is_tallest(dir2))
}


is_visible <- function(row, col, tree_matrix){
  tree_row <- tree_matrix[row,]
  tree_col <- tree_matrix[,col]
  
  return(is_visible_vector(col, tree_row)|is_visible_vector(row, tree_col))
}


determine_visible <- function(input){
  processed_input <- process_input(input)
  
  vis_matrix <- processed_input
  
  for(i in 1:nrow(processed_input)){
    for(j in 1:ncol(processed_input)){
      vis_matrix[i, j] <- is_visible(i, j, processed_input)
    }
  }
  
  vis_matrix
  
}

count_visible_tree <- function(input){
  determine_visible(input) %>% 
    sum()
}


assertthat::are_equal(
  count_visible_tree(input_test),
  21
)




count_visible_tree(input)



# part 2 ----


calc_scen_dist <- function(tree_height, vec_from_tree){
  size_comp <- vec_from_tree < tree_height
  return(min(sum(cumprod(size_comp)) + 1, length(vec_from_tree)))
}

scen_dist_vect <- function(id, tree_vector){
  
  if(id == 1 | id == length(tree_vector)){
    return(0)
  }
  
  dir1 <- rev(tree_vector[1:(id - 1)])
  dir2 <- tree_vector[(id + 1):length(tree_vector)]
  
  return(c(calc_scen_dist(tree_vector[id], dir1), calc_scen_dist(tree_vector[id], dir2)))
}


scen_dist <- function(row, col, tree_matrix){
  tree_row <- tree_matrix[row,]
  tree_col <- tree_matrix[,col]
  
  return(prod(scen_dist_vect(col, tree_row), scen_dist_vect(row, tree_col)))
}




calc_all_scen_dit <- function(input){
  processed_input <- process_input(input)
  
  vis_matrix <- processed_input
  
  for(i in 1:nrow(processed_input)){
    for(j in 1:ncol(processed_input)){
      vis_matrix[i, j] <- scen_dist(i, j, processed_input)
    }
  }
  
  vis_matrix
  
}

maximum_scen_dist <- function(input){
  calc_all_scen_dit(input) %>% 
    max()
}




assertthat::are_equal(
  maximum_scen_dist(input_test),
  8
)

maximum_scen_dist(input)






















