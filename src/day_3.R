library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

input_test <- readLines("./data/day_3_test.txt")
input <- readLines("./data/day_3_input.txt")

process_input <- function(input){
  
  tibble(raw_input = input,
         len = nchar(raw_input)) %>% 
    mutate(
      cpt1 = str_sub(raw_input, 1, len/2),
      cpt2 = str_sub(raw_input, len/2 + 1, len),
      cpt1_char = str_split(cpt1, ""),
      cpt2_char = str_split(cpt2, ""),
      common = map2_chr(cpt1_char, cpt2_char, ~unique(.x[which(.x %in% .y)]))
      
    )
  
}

calc_score <- function(data, letter){
  data %>% 
    mutate(
      score = sapply({{letter}}, utf8ToInt) - 96,
      score = ifelse(sign(score) <= 0, sapply({{letter}}, utf8ToInt) - 38, score)
    )
}

calc_total <- function(data){
  process_input(data) %>% 
    calc_score(common) %>% 
    pull(score) %>% 
    sum()
}

assertthat::are_equal(
  calc_total(input_test),
  157
)


calc_total(input)



# part 2 ----


process_input(input_test)

process_input_2 <- function(input){
  
  tibble(raw_input = input,
         team_id = (1:length(input) - 1) %/% 3,
         elf_id = rep(1:3, length(input)/3)) %>% 
    pivot_wider(team_id, values_from = raw_input, names_from = elf_id, names_prefix = "elf_") %>% 
    mutate(
      across(c(elf_1, elf_2, elf_3), ~str_split(.x, "")),
      common = map2(elf_1, elf_2, ~unique(.x[which(.x %in% .y)])),
      common = map2_chr(common, elf_3, ~unique(.x[which(.x %in% .y)]))
    )
  
}

calc_total_2 <- function(data){
  process_input_2(data) %>%
    calc_score(common) %>% 
    pull(score) %>% 
    sum()
}


assertthat::are_equal(
  calc_total_2(input_test),
  70
)


calc_total_2(input)







