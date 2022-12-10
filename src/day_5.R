library(tidyverse)
library(stringr)
library(glue)


input_test <- list()
input <- list()


input_test$operations <- readLines("./data/day_5_test_operations.txt")
input_test$crates <- readLines("./data/day_5_test_crates.txt")
input$operations <- readLines("./data/day_5_input_operations.txt")
input$crates <- readLines("./data/day_5_input_crates.txt")


get_stacks <- function(stacks_input){
  
  n_stacks <- (nchar(stacks_input[1]) + 1) / 4
  stacks <- list()
  
  for(stack in 1:n_stacks){
    stacks[[glue("stack_{stack}")]] <- c()
    for(line in length(stacks_input):1){
      stacks[[glue("stack_{stack}")]] <- c(stacks[[glue("stack_{stack}")]], str_sub(stacks_input[line], (stack - 1) * 4 + 1, (stack - 1) * 4 + 1 + 2))
    }
  }
  
  stacks <- lapply(stacks, function(x){x <- x[!grepl(" ", x)]})
  
  stacks
}

parse_oper <- function(oper_input){
  
  tibble(oper = oper_input) %>% 
    separate(oper, c("action", "count", "from", "from_stack", "to", "to_stack"), " ") %>% 
    select(count, from_stack, to_stack) %>% 
    mutate(
      across(
        everything(),
        ~as.numeric(.x)
      )
    )
  
}


move_box <- function(stacks, count, from, to){
  moved_boxes <- stacks[[from]][(length(stacks[[from]]) - count + 1):length(stacks[[from]])]
  
  if(count == length(stacks[[from]])){
    stacks[[from]] <- character(0)
  } else {
    stacks[[from]] <- stacks[[from]][1:(length(stacks[[from]]) - count)]
  }
  
  stacks[[to]] <- c(stacks[[to]], rev(moved_boxes))
  
  stacks
}

apply_all_oper <- function(stacks, operations, move_box_fn = move_box){
  curr_stacks <- stacks
  mapply(function(count, from, to){
    curr_stacks <<- move_box_fn(curr_stacks, count, from, to)
  },
  count = operations$count,
  from = operations$from_stack,
  to = operations$to_stack)
  
  curr_stacks
}

get_top_crates <- function(stacks){
  sapply(stacks, function(x){x[length(x)]}) %>% 
    str_remove_all("[\\[\\]]") %>% 
    paste0(collapse = "")
}



stacks_test <- get_stacks(input_test$crates)
opers_test <- parse_oper(input_test$operations)


assertthat::are_equal(
  apply_all_oper(stacks_test, opers_test) %>% get_top_crates(),
  "CMZ"
)






stacks <- get_stacks(input$crates)
opers <- parse_oper(input$operations)

apply_all_oper(stacks, opers) %>% get_top_crates()





# part 2 ----


move_box_multiple <- function(stacks, count, from, to){
  moved_boxes <- stacks[[from]][(length(stacks[[from]]) - count + 1):length(stacks[[from]])]
  
  if(count == length(stacks[[from]])){
    stacks[[from]] <- character(0)
  } else {
    stacks[[from]] <- stacks[[from]][1:(length(stacks[[from]]) - count)]
  }
  
  stacks[[to]] <- c(stacks[[to]], moved_boxes)
  
  stacks
}



assertthat::are_equal(
  apply_all_oper(stacks_test, opers_test, move_box_multiple) %>% get_top_crates(),
  "MCD"
)




apply_all_oper(stacks, opers, move_box_multiple) %>% get_top_crates()















