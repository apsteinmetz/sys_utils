# pet shop boys dvd renamer
library(tidyverse)
target_dir <- "C:\\Users\\Apste\\Videos\\PSB"
target_files <- tibble(old_name = dir(target_dir))
track_names <- read_csv("data/psb_dvd_38.csv", 
                       col_names = FALSE) |> 
  select(X2)

target_files <- target_files %>% 
  mutate(order = str_remove(old_name,"Psb-8-1-")) |> 
  mutate(order = str_remove(order,".mp4")) |> 
  # mutate(new_name = if_else(nchar(new_name)==1,paste0("0",new_name),new_name))
  mutate(order = str_pad(order,2,pad = "0")) |> 
  arrange(order) |> 
  bind_cols(track_names)


target_files_2 <- target_files |> 
  rename(new_name = X2) |> 
  mutate(new_name = paste0(order," - Pet Shop Boys - ",new_name,".mp4")) |> 
  # escape all special characters in new name
  mutate(new_name = str_replace_all(new_name,"\\?","")) |> 
  mutate(new_name = str_replace_all(new_name,"\\/","-"))
target_files_2

# rename old name to new name
for (n in 1:nrow(target_files_2)) {
  old_name <- target_files_2$old_name[n]
  new_name <- paste0(target_files_2$new_name[n])
  print(paste0("Renaming ",old_name," to ",new_name))
  file.copy(from = paste0(target_dir,"\\",old_name),
              to = paste0(target_dir,"\\",new_name))
}
