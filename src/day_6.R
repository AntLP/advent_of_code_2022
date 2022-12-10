library(tidyverse)


input_test <- readLines("./data/day_6_test.txt") %>% str_split("", simplify = T)
input <- readLines("./data/day_6_input.txt") %>% str_split("", simplify = T)

find_buffer <- function(input, unique_len = 4){
  for(i in unique_len:length(input)){
    if(length(unique(input[(i - unique_len + 1):i])) == unique_len){
      return(i)
    }
  }
}

assertthat::are_equal(
  find_buffer(input_test),
  7
)

find_buffer(input)



# part 2 ----


assertthat::are_equal(
  find_buffer(input_test, 14),
  19
)


find_buffer(input, 14)
