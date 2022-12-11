library(tidyverse)


input_test <- readLines("./data/day_9_test.txt")
input <- readLines("./data/day_9_input.txt")


print_matrix <- function(curr_head, curr_tail){
  mat <- matrix(nrow = 5, ncol = 6, data = rep("", 30))
  
  mat[5 - curr_head[1], curr_head[2] + 1] <- "H"
  mat[5 - curr_tail[1], curr_tail[2] + 1] <- "T"
  
  print(mat)
}

update_positions <- function(dir, n, curr_head, curr_tail){
  
  visited_positions <- c()
  
  for(i in 1:n){
    curr_head <- update_head_position(dir, curr_head)
    curr_tail <- update_tail_position(curr_head, curr_tail)
    visited_positions <- c(visited_positions, paste(curr_tail, collapse = "x"))
    
    # print_matrix(curr_head, curr_tail)
  }
  
  return(list(
    curr_head = curr_head,
    curr_tail = curr_tail,
    visited_positions = visited_positions
  ))
}


update_head_position <- function(dir, curr_head){
  dir_add <- case_when(
    dir == "U" ~ c(1, 0),
    dir == "D" ~ c(-1, 0),
    dir == "R" ~ c(0, 1),
    dir == "L" ~ c(0, -1)
  )
  
  # return(pmax(pmin(curr_head + dir_add, 5), 0))
  return(curr_head + dir_add)
}


update_tail_position <- function(curr_head, curr_tail){
  
  if(max(abs(curr_head - curr_tail)) < 2){
    new_position <- curr_tail
  } else {
    new_position <- mapply(
      function(x, y){ifelse(y >= 0, ceiling(x), floor(x))}, 
      x = (curr_head + curr_tail) / 2, 
      y = sign(curr_head - curr_tail)
    )
  }
  
  
  new_position
}


process_input <- function(instructions){
  tibble(instructions = instructions) %>% 
    separate(instructions, c("dir", "n"), sep = " ") %>% 
    mutate(n = as.integer(n))
}


count_all_positions <- function(instructions){
  
  curr_head <- c(0, 0)
  curr_tail <- c(0, 0)
  visited_positions <- c("0x0")
  
  instructions <- process_input(instructions)
  
  for(i in 1:nrow(instructions)){
    # print(instructions$dir[i])
    new_positions <- update_positions(instructions$dir[i], instructions$n[i], curr_head, curr_tail)
    
    curr_head <- new_positions$curr_head
    curr_tail <- new_positions$curr_tail
    visited_positions <- c(visited_positions, new_positions$visited_positions)
  }
  
  
  return(length(unique(visited_positions)))
}

assertthat::are_equal(
  count_all_positions(input_test),
  13
)


count_all_positions(input)










# part 2 ----



update_positions_10 <- function(dir, n, curr_positions){
  
  visited_positions <- c()
  
  for(i in 1:n){
    curr_positions$head <- update_head_position(dir, curr_positions$head)
    curr_positions$p1 <- update_tail_position(curr_positions$head, curr_positions$p1)
    curr_positions$p2 <- update_tail_position(curr_positions$p1, curr_positions$p2)
    curr_positions$p3 <- update_tail_position(curr_positions$p2, curr_positions$p3)
    curr_positions$p4 <- update_tail_position(curr_positions$p3, curr_positions$p4)
    curr_positions$p5 <- update_tail_position(curr_positions$p4, curr_positions$p5)
    curr_positions$p6 <- update_tail_position(curr_positions$p5, curr_positions$p6)
    curr_positions$p7 <- update_tail_position(curr_positions$p6, curr_positions$p7)
    curr_positions$p8 <- update_tail_position(curr_positions$p7, curr_positions$p8)
    curr_positions$tail <- update_tail_position(curr_positions$p8, curr_positions$tail)
    
    visited_positions <- c(visited_positions, paste(curr_positions$tail, collapse = "x"))
    
  }
  
  return(list(
    curr_positions = curr_positions,
    visited_positions = visited_positions
  ))
}


count_all_positions_10 <- function(instructions){
  
  curr_positions <- list()
  
  curr_positions$head <- c(0, 0)
  curr_positions$p1 <- c(0, 0)
  curr_positions$p2 <- c(0, 0)
  curr_positions$p3 <- c(0, 0)
  curr_positions$p4 <- c(0, 0)
  curr_positions$p5 <- c(0, 0)
  curr_positions$p6 <- c(0, 0)
  curr_positions$p7 <- c(0, 0)
  curr_positions$p8 <- c(0, 0)
  curr_positions$tail <- c(0, 0)
  
  visited_positions <- c("0x0")
  
  instructions <- process_input(instructions)
  
  for(i in 1:nrow(instructions)){
    new_positions <- update_positions_10(instructions$dir[i], instructions$n[i], curr_positions)
    
    curr_positions <- new_positions$curr_positions
    visited_positions <- c(visited_positions, new_positions$visited_positions)
    
  }
  
  
  return(length(unique(visited_positions)))
}



input_test2 <- readLines("./data/day_9_test2.txt")

assertthat::are_equal(
  count_all_positions_10(input_test2),
  36
)

count_all_positions_10(input)


