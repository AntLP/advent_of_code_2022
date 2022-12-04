library(tidyverse)


input_test <- readLines("./data/day_4_test.txt")
input <- readLines("./data/day_4_input.txt")

process_input <- function(input){
  tibble(raw_input = input) %>% 
    separate(raw_input, c("elf_1", "elf_2"), sep = ",") %>% 
    separate(elf_1, c("elf_1_start", "elf_1_end"), "-") %>% 
    separate(elf_2, c("elf_2_start", "elf_2_end"), "-") %>% 
    mutate(
      elf_1_range = mapply(seq, elf_1_start, elf_1_end),
      elf_2_range = mapply(seq, elf_2_start, elf_2_end),
      overlap = mapply(function(x, y) {sum(x %in% y)}, x = elf_1_range, y = elf_2_range),
      max_len = pmin(sapply(elf_1_range, length), sapply(elf_2_range, length))
    )
}



# part 1 ----

count_pt_1 <- function(input){
  process_input(input) %>% 
    filter(overlap == max_len) %>% 
    nrow()
}


assertthat::are_equal(
  count_pt_1(input_test),
  2
)

count_pt_1(input)



# part 2 ----

count_pt_2 <- function(input){
  process_input(input) %>% 
    filter(overlap > 0) %>% 
    nrow()
}


assertthat::are_equal(
  count_pt_2(input_test),
  4
)


count_pt_2(input)

