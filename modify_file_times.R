# Claude, write code to recursively scan a selected folder and change the file date of
# any file dated in the future to the date of the most recent file that is not
# dated in the future. Show the names of the files that were changed.
  

library(tidyverse)
library(fs)
library(lubridate)

DEBUG <- FALSE
dir_path <- "C:\\Users\\Apste\\Documents\\Amiga"

files <- dir_ls(dir_path, recurse = TRUE)
files_info <- file_info(files)

current_time <- now()

# Add directory column
files_info <- files_info %>%
  mutate(directory = path_dir(path))

# Calculate latest valid time for each directory
latest_valid_times <- files_info %>%
  filter(modification_time <= current_time) %>%
  group_by(directory) %>%
  summarize(latest_valid_time = max(modification_time))

# Process each directory
for(dir in unique(files_info$directory)) {
  dir_files <- files_info %>% 
    filter(directory == dir)
  
  future_files <- dir_files %>%
    filter(modification_time > current_time)
  
  if(nrow(future_files) > 0) {
    latest_valid_time <- latest_valid_times %>%
      filter(directory == dir) %>%
      pull(latest_valid_time)
    
    if(!is.null(latest_valid_time) && !DEBUG) {
      Sys.setFileTime(future_files$path,latest_valid_time)
    }
  
    cat("\nDirectory:", dir, "\n")
    # report if no files were modified
    cat("Modified files:\n")
    print(basename(future_files$path))
    } else {
      cat("No files modified in ", dir, ".\n")
    }
  }

temp <- files_info %>%
  group_by(directory) %>%
  mutate(max_date = max(modification_time)) %>%
  filter(modification_time == max_date) %>%
  mutate(new_name = paste0(directory, "/latest_date_", basename(path)))


%>%
  {if(!DEBUG) rename(.$path, .$new_name) else .} %>%
  select(path, new_name)
