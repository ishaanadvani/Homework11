#Ishaan_problem1.R
copy_dir <- function(dirname, prefix_string)
{
  prefix_dir_name <- paste0(prefix_string, dirname)
  old_dir <- getwd()
  setwd(dirname)
  new_dir <- getwd()
  setwd("..")
  system(paste("mkdir", prefix_dir_name))
  dirname1 <- paste0(dirname, "/*")
  prefix_dir_name1 <- paste0(prefix_dir_name, "/")
  system(paste("cp",dirname1, prefix_dir_name1))
  setwd(prefix_dir_name)
  list <- list.files()
  
  for (k in 1:length(list))
  {
    cur_file_name <- list[k]
    new_file_name <- paste0(prefix_string, cur_file_name)
    system(paste("mv", cur_file_name, new_file_name))
  }
  
  setwd("..")
}

copy_dir2 <- function(dirname, prefix_string)
{
  cur_dir <- getwd()
  dir.create(paste0(prefix_string,dirname))
  newdirname <- paste0(prefix_string,dirname)
  list_of_files <- list.files(dirname)
  file.copy(file.path(dirname,list_of_files), newdirname)
  setwd(newdirname)
  count <- length(list_of_files)
  
  for (i in 1:count)
  {
    cur_file <- list_of_files[i]
    cur_new_file <- paste0(prefix_string, list_of_files[i])
    file.rename(cur_file, cur_new_file)
  }
  
  setwd("..")
}