# crawl a google drive folder to get summary statistics, size and file counts and created dates.
library(googledrive)
library(tidyverse)

drive_auth_configure(api_key = Sys.getenv("GOOGLE_API_KEY"))
drive_deauth()

choose_folder <- function() {
  current <- as_id("root")
  stack <- list()
  repeat {
    cur <- drive_get(current)
    cur_name <- cur$name
    folders <- drive_ls(current, type = "folder", fields = "files(id,name,mimeType,parents)")
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
  children <- drive_ls(folder_id, fields = "files(id,name,mimeType,parents,size,createdTime)")
  files <- children %>% filter(mime_type != "application/vnd.google-apps.folder")
  sizes <- files$drive_resource %>% purrr::map_chr("size", .default = NA_character_) %>% as.numeric()
  ctimes_chr <- files$drive_resource %>% purrr::map_chr("createdTime", .default = NA_character_)
  ctimes <- if (length(ctimes_chr)) as.POSIXct(ctimes_chr, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC") else as.POSIXct(character())
  tibble(
    folder_id = as.character(folder_id),
    folder_name = drive_get(folder_id)$name,
    file_count = nrow(files),
    total_size_bytes = sum(sizes, na.rm = TRUE),
    median_created_time = if (length(ctimes)) stats::median(ctimes, na.rm = TRUE) else as.POSIXct(NA)
  )
}

crawl <- function(folder_id) {
  here <- summarise_folder(folder_id)
  subs <- drive_ls(folder_id, type = "folder", fields = "files(id,name,mimeType,parents)")
  if (nrow(subs) == 0) return(here)
  bind_rows(here, purrr::map_dfr(subs$id, crawl))
}

start <- choose_folder()
results <- crawl(as_id(start$id))
results
