library(tidyverse)

input_mini <- readLines("./data/day_10_mini.txt")
input_test <- readLines("./data/day_10_test.txt")
input <- readLines("./data/day_10_input.txt")


process_input <- function(input){
  tibble(input = input) %>% 
    separate("input", c("oper", "n"), " ") %>% 
    mutate(n = coalesce(as.integer(n), 0),
           cum_n = 1 + cumsum(n),
           oper_len = ifelse(oper == "addx", 2, 1),
           cum_time = cumsum(oper_len),
           lead_time = coalesce(lead(cum_time), max(cum_time, na.rm = T) + ifelse(oper[length(oper)] == "addx", 2, 0)),
           times = map2(cum_time, lead_time, ~seq(.x, .y - 1))
    ) %>% 
    unnest(times) %>% 
    mutate(during_time = times + 1)
}


select_multiples_40 <- function(input){
  process_input(input) %>% 
    select(during_time, cum_n) %>% 
    filter((during_time - 20) %% 40 == 0) %>% 
    mutate(strength = cum_n * during_time)
}


process_input(input_mini)

assertthat::are_equal(
  select_multiples_40(input_test)$strength %>% sum(),
  13140
)

  

select_multiples_40(input)$strength %>% sum()







# part 2 ----


process_further <- function(input){
  process_input(input) %>% 
    mutate(sprite_pixels = map2_lgl(cum_n, times %% 40, ~.y %in% seq(.x-1, .x+1)),
           sprite = ifelse(sprite_pixels, "#", "")) %>% 
    pull(sprite) %>%
    c("", "", .) %>%
    '['(1:240) %>% 
    matrix(ncol = 40, byrow = T)
}


process_further(input) %>% view()
#LOL








