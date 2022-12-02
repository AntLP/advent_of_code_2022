library(tidyverse)


input_test <- readLines("./data/day_2_test.txt")
input <- readLines("./data/day_2_input.txt")


score_conv <- tribble(
  ~p2, ~p1, ~shape_score, ~shape_id,
  "X", "A", 1, 0,
  "Y", "B", 2, 1,
  "Z", "C", 3, 2
)

score_table <- tribble(
  ~p1, ~p2, ~score,
  "A", "X",     3L,
  "A", "Y",     6L,
  "A", "Z",     0L,
  "B", "X",     0L,
  "B", "Y",     3L,
  "B", "Z",     6L,
  "C", "X",     6L,
  "C", "Y",     0L,
  "C", "Z",     3L
  )





# part 1 ----
score_rps <- function(input){
  
  tibble(raw_input = input) %>% 
    separate(raw_input, c("p1", "p2")) %>% 
    left_join(score_table, by = c("p1", "p2")) %>% 
    left_join(score_conv, by = c("p2")) %>% 
    mutate(total_score = score + shape_score) %>% 
    pull(total_score) %>% 
    sum()
  
}

assertthat::are_equal(
  score_rps(input_test),
  15
)
# correct


score_rps(input)


# part 2 ----
find_played <- function(p1_played, result){
  
  desired_score = case_when(result == "X" ~ 0,
                    result == "Y" ~ 3,
                    result == "Z" ~ 6)
  
  score_table %>% 
    filter(score == desired_score & p1 == p1_played) %>% 
    pull(p2)
}


score_rps_2 <- function(input){
  
  
  tibble(raw_input = input) %>% 
    separate(raw_input, c("p1", "p2")) %>% 
    mutate(
      p2 = mapply(find_played, p1_played = p1, result = p2)
    ) %>% 
    left_join(score_table, by = c("p1", "p2")) %>%
    left_join(score_conv, by = c("p2")) %>%
    mutate(total_score = score + shape_score) %>%
    pull(total_score) %>%
    sum()
  
}

assertthat::are_equal(
  score_rps_2(input_test),
  12
)
# correct



score_rps_2(input)









