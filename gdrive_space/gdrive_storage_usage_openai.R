# crawl a google drive folder to get summary statistics, size and file counts and created dates.
library(googledrive)
library(tidyverse)

drive_auth(email = "apsteinmetz@gmail.com",
           token = Sys.getenv("GOOGLE_API_KEY"))
# drive_deauth()

show_files <- TRUE
choose_folder <- function() {
  current <- as_id("root")
  stack <- list()
  show_files <<- utils::select.list(c("Yes","No"),title = "Show Names of Files in Output?")
  if (show_files == "Yes") show_files <<- TRUE else show_files <<- FALSE
  repeat {
    cur <- drive_get(current)
    cur_name <- cur$name
    folders <- drive_ls(current, type = "folder")
    choices <- c("[Select this folder]", if (length(stack)) "[Go up]" else NULL, folders$name)
    sel <- utils::select.list(choices, title = paste0("Current: ", cur_name))
    if (sel == "") stop("Selection cancelled", call. = FALSE)
    if (sel == "[Select this folder]") return(list(id = cur$id, name = cur_name))
    if (sel == "[Go up]") {
      prev <- stack[[length(stack)]]
      stack <- stack[-length(stack)]
      current <- prev$id
      next
    }
    idx <- match(sel, folders$name)
    stack <- append(stack, list(list(id = cur$id, name = cur_name)))
    current <- folders$id[idx]
  }
}

summarise_folder <- function(folder_id) {
  cat("Getting: ",drive_get(folder_id)$name,"\n")
  
  files_meta_raw <- drive_ls(folder_id, recursive = TRUE)
  # add file size, file created date and file type to children
  files_meta <- files_meta_raw %>% 
    mutate(size = as.numeric(map_chr(drive_resource, "size", .default = NA))) |> 
    mutate(createdTime = as.POSIXct(map_chr(drive_resource, "createdTime", .default = NA))) |> 
    mutate(fileType = map_chr(drive_resource, "fileExtension", .default = NA_character_)) |> 
    mutate(parentId = unlist(map(drive_resource, "parents", .default = NA_character_))) |> 
    # add a second column for size that is more human readable using sprintf
    mutate(size_human = sprintf("%.2f MB", size / (1024 * 1024))) %>%
    select(name, fileType, parentId, size_human, createdTime,size, id)
  # filter out folders
  files_meta_summary <- files_meta |> 
    summarise(
      folder_name = drive_get(folder_id)$name,
      file_count = n(),
      total_size_bytes = sprintf("%.2f MB", sum(size, na.rm = TRUE) / (1024 * 1024)),
      median_created_time = as.Date(if (n() > 0) stats::median(createdTime, na.rm = TRUE) else as.POSIXct(NA)),
      folder_id = folder_id
    )
  return(files_meta_summary)
}

crawl <- function(folder_id) {
  here <- summarise_folder(folder_id)
  #subs <- drive_ls(folder_id, type = "folder", fields = "files(id,name,mimeType,parents)")
  #if (nrow(subs) == 0) return(here)
  #bind_rows(here, purrr::map_dfr(subs$id, crawl))
  return(here)
}

start <- choose_folder()

results <- crawl(as_id(start$id))
results
