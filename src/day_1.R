library(tidyverse)


input_test <- readLines("./data/day_1_test.txt")
input <- readLines("./data/day_1_input.txt")


process_input <- function(input, top_n = 1){
  
  tibble(n_cal = as.integer(input),
         elf_id = cumsum(is.na(n_cal))) %>% 
    na.omit() %>% 
    group_by(elf_id) %>% 
    summarise(n_cal = sum(n_cal)) %>% 
    slice_max(n_cal, n = top_n) %>% 
    pull(n_cal)
  
}

# part 1 ----
assertthat::are_equal(process_input(input_test), 24000)
# Correct



process_input(input)



# part 2 ----
assertthat::are_equal(
  sum(process_input(input_test, top_n = 3)), 
  45000
)
# Correct

sum(process_input(input, top_n = 3))


