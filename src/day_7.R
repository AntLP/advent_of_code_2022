library(tidyverse)
library(xml2)
library(glue)

input_test <- readLines("./data/day_7_test.txt")
input <- readLines("./data/day_7_input.txt")


parse_cd <- function(cd_seq){
  dirs <- xml_new_root(.value = ".")
  paths <- c("/.")
  
  
  for(i in 2:length(cd_seq)){
    if(grepl("\\.\\.", cd_seq[i])){
      dirs <- dirs %>% xml_parent()
    } else {
      dirs %>% xml_add_child(cd_seq[i])
      dirs <- dirs %>% xml_find_first(glue("./{ch}", ch = cd_seq[i]))
      paths <- c(paths, dirs %>% xml_path())
    }
  }
  
  paths
}

parse_input <- function(input){
  # cd <- input[grepl("\\$ cd", input) & !grepl("\\.\\.", input)] %>% str_remove_all("\\$ cd ")
  cd <- input[grepl("\\$ cd", input)] %>% str_remove_all("\\$ cd ") %>% parse_cd()
  ls <- input[grepl("\\$ ls", input)]
  ls_id <- which(grepl("\\$ ls", input))
  command_id <- which(grepl("\\$ ", input))
  
  ls_res <- lapply(1:(length(ls_id)), function(x){
        next_command_id <- ifelse(length(command_id[command_id > ls_id[x]]) == 0, length(input), min(command_id[command_id > ls_id[x]]) - 1)
    
    input[(ls_id[x] + 1):next_command_id]
  })
  
  names(ls_res) <- cd
  
  
  lapply(ls_res, function(x){
    subdirs <- c()
    dir_size <- 0
    for(i in 1:length(x)){
      if(grepl("dir ", x[i])){
        subdirs <- c(subdirs, str_remove(x[i], "dir "))
      } else {
        dir_size <- dir_size + as.integer(str_extract(x[i], "[0-9]*"))
      }
    }
    
    return(list(
      subdirs = subdirs,
      dir_size = dir_size
    ))
  })
  

}

compute_total_size <- function(input){
  
  parsed_input <- parse_input(input)
  
  get_size <- function(dir_name){
    if(is.null(parsed_input[[dir_name]]$subdirs)){
      return(parsed_input[[dir_name]]$dir_size)
    } else {
      
      subdir_names <- sapply(parsed_input[[dir_name]]$subdirs, function(x){paste(dir_name, x, sep = "/")})
      
      return(sum(parsed_input[[dir_name]]$dir_size, sapply(subdir_names, get_size)))
    }
  }
  
  total_sizes <- lapply(names(parsed_input), get_size)
  names(total_sizes) <- names(parsed_input)
  total_sizes %>% unlist()
}


get_small_total <- function(dir_sizes, max_size){
  sum(dir_sizes[dir_sizes <= max_size])
}

assertthat::are_equal(
  compute_total_size(input_test) %>% get_small_total(100000),
  95437
)

compute_total_size(input) %>% get_small_total(100000)




# part 2 ----


get_delete_size <- function(input){
  sizes <- compute_total_size(input)
  
  req_space <- 30000000 - (70000000 - sizes[1])
  min(sizes[sizes >= req_space])
  
}


assertthat::are_equal(
  get_delete_size(input_test),
  24933642
)


get_delete_size(input)













