# Google Drive Folder Crawler - Interactive Navigation
# Allows drilling down through folders before starting the crawl

# Load required libraries
library(googledrive)
library(dplyr)
library(purrr)
library(lubridate)

# Authenticate with Google Drive using API key from environment
authenticate_drive <- function() {
  api_key <- Sys.getenv("GOOGLE_API_KEY")
  
  if (api_key == "") {
    stop("GOOGLE_DRIVE_API_KEY environment variable not found. 
         Please set your Google Drive API key in your environment variables.")
  }
  
  # Authenticate using the API key
  drive_auth(email = "apsteinmetz@gmail.com",
             token = Sys.getenv("GOOGLE_API_KEY"))
  cat("Successfully authenticated with Google Drive\n")
}

get_folder_contents <- function(folder_id, folders_only = FALSE) {
  tryCatch({
    contents <- if (folders_only) drive_ls(as_id(folder_id),type = "folder") else drive_ls(as_id(folderid))
    return(contents)
  }, error = function(e) {
    warning(sprintf("Error accessing folder %s: %s", folder_id, e$message))
    return(data.frame())
  })
}

# Function to display folder navigation menu

display_folder_menu <- function(folders, current_path = "Root") {
  cat("\n", rep("=",60), "\n")
  cat(sprintf("Current location: %s\n", current_path))
  cat(rep("=",60), "\n")
  
  if (nrow(folders) == 0) {
    cat("No subfolders found in this location.\n")
    return(0)
  }
  
  cat("Available folders:\n")
  for (i in 1:nrow(folders)) {
    cat(sprintf("%d. %s\n", i, folders$name[i]))
  }
  
  cat(sprintf("%d. [SCAN ALL FROM HERE] - Start crawling from current location\n", nrow(folders) + 1))
  cat(sprintf("%d. [GO BACK] - Return to parent folder\n", nrow(folders) + 2))
  cat(sprintf("%d. [EXIT] - Quit the program\n", nrow(folders) + 3))
  
  return(nrow(folders))
}

# Function to get user choice with validation
get_user_choice <- function(max_choice) {
  while (TRUE) {
    choice <- readline(prompt = "\nEnter your choice: ")
    choice_num <- suppressWarnings(as.integer(choice))
    
    if (is.na(choice_num) || choice_num < 1 || choice_num > (max_choice + 3)) {
      cat(sprintf("Invalid choice. Please enter a number between 1 and %d\n", max_choice + 3))
      next
    }
    
    return(choice_num)
  }
}

# Interactive folder navigation
navigate_folders <- function() {
  # Navigation stack to keep track of path
  nav_stack <- list()
  current_folder <- list(id = "root", name = "Root", path = "Root")
  
  while (TRUE) {
    cat(sprintf("\nFetching folders from: %s\n", current_folder$name))
    
    # Get subfolders only
    subfolders <- get_folder_contents(current_folder$id, folders_only = TRUE)
    
    # Display menu
    max_folders <- display_folder_menu(subfolders, current_folder$path)
    
    if (is.null(max_folders)) {
      # No subfolders, can only scan all, go back, or exit
      cat("\nOptions:\n")
      cat("1. [SCAN ALL FROM HERE] - Start crawling from current location\n")
      cat("2. [GO BACK] - Return to parent folder\n") 
      cat("3. [EXIT] - Quit the program\n")
      
      choice <- get_user_choice(0)
      choice <- choice + max_folders  # Adjust for the offset
      
      max_folders <- 0  # Set to 0 since there are no folders
    } else {
      # Get user choice
      choice <- get_user_choice(max_folders)
    }
    
    # Process user choice
    if (choice <= max_folders) {
      # User selected a subfolder to navigate into
      selected_folder <- subfolders[choice, ]
      
      # Push current folder to stack
      nav_stack <- append(nav_stack, list(current_folder))
      
      # Update current folder
      new_path <- if (current_folder$path == "Root") {
        selected_folder$name
      } else {
        paste(current_folder$path, selected_folder$name, sep = "/")
      }
      
      current_folder <- list(
        id = selected_folder$id,
        name = selected_folder$name,
        path = new_path
      )
      
    } else if (choice == (max_folders + 1)) {
      # Scan all from here
      cat(sprintf("\nStarting crawl from: %s\n", current_folder$path))
      return(current_folder)
      
    } else if (choice == (max_folders + 2)) {
      # Go back
      if (length(nav_stack) > 0) {
        current_folder <- nav_stack[[length(nav_stack)]]
        nav_stack <- nav_stack[-length(nav_stack)]
      } else {
        cat("Already at root level!\n")
      }
      
    } else if (choice == (max_folders + 3)) {
      # Exit
      cat("Exiting program.\n")
      return(NULL)
    }
  }
}

# Function to crawl a single folder and collect data
crawl_folder <- function(folder_id, folder_name, folder_path = "", depth = 0) {
  indent <- paste(rep("  ", depth), collapse = "")
  cat(sprintf("%sCrawling folder: %s\n", indent, folder_name))
  
  # Get folder contents
  contents <- get_folder_contents(folder_id)
  
  if (nrow(contents) == 0) {
    return(data.frame(
      folder_path = folder_path,
      folder_name = folder_name,
      folder_id = folder_id,
      file_count = 0,
      total_size_bytes = 0,
      total_size_mb = 0,
      median_creation_date = as.Date(NA),
      subfolder_count = 0,
      depth = depth
    ))
  }
  
  # Separate files and folders
  folder_mime_type <- "application/vnd.google-apps.folder"
  files <- contents[map_lgl(contents$drive_resource, ~ .x$mimeType != folder_mime_type), ]
  folders <- contents[map_lgl(contents$drive_resource, ~ .x$mimeType == folder_mime_type), ]
  
  # Calculate file statistics
  if (nrow(files) > 0) {
    # Extract file sizes (some files might not have size information)
    file_sizes <- map_dbl(files$drive_resource, ~ {
      size <- .x$size
      if (is.null(size)) return(0)
      as.numeric(size)
    })
    
    # Extract creation dates
    creation_dates <- map_chr(files$drive_resource, ~ {
      created <- .x$createdTime
      if (is.null(created)) return(NA_character_)
      created
    })
    
    creation_dates <- as.Date(creation_dates)
    creation_dates <- creation_dates[!is.na(creation_dates)]
    
    file_count <- nrow(files)
    total_size_bytes <- sum(file_sizes, na.rm = TRUE)
    total_size_mb <- total_size_bytes / (1024 * 1024)
    median_creation_date <- if (length(creation_dates) > 0) {
      median(creation_dates, na.rm = TRUE)
    } else {
      as.Date(NA)
    }
  } else {
    file_count <- 0
    total_size_bytes <- 0
    total_size_mb <- 0
    median_creation_date <- as.Date(NA)
  }
  
  # Current folder summary
  current_folder_data <- data.frame(
    folder_path = folder_path,
    folder_name = folder_name,
    folder_id = folder_id,
    file_count = file_count,
    total_size_bytes = total_size_bytes,
    total_size_mb = round(total_size_mb, 2),
    median_creation_date = median_creation_date,
    subfolder_count = nrow(folders),
    depth = depth
  )
  
  # Recursively crawl subfolders
  if (nrow(folders) > 0) {
    cat(sprintf("%s  Found %d subfolder(s)\n", indent, nrow(folders)))
        
        subfolder_data <- map_dfr(1:nrow(folders), ~ {
          subfolder <- folders[.x, ]
          subfolder_path <- if (folder_path == "") {
            folder_name
          } else {
            paste(folder_path, folder_name, sep = "/")
          }
          
          crawl_folder(
            subfolder$id,
            subfolder$name,
            subfolder_path,
            depth + 1
          )
        })
        
        # Combine current folder data with subfolder data
        all_data <- bind_rows(current_folder_data, subfolder_data)
  } else {
    all_data <- current_folder_data
  }
  
  return(all_data)
}

# Function to create summary statistics
create_summary <- function(crawl_results) {
  cat("\n", rep("=",60), "\n")
  cat("CRAWL SUMMARY\n")
  cat(rep("=",60), "\n")
  
  total_folders <- nrow(crawl_results)
  total_files <- sum(crawl_results$file_count, na.rm = TRUE)
  total_size_mb <- sum(crawl_results$total_size_mb, na.rm = TRUE)
  total_size_gb <- total_size_mb / 1024
  max_depth <- max(crawl_results$depth, na.rm = TRUE)
  
  # Overall median creation date (weighted by file count)
  dates_with_files <- crawl_results[crawl_results$file_count > 0 & !is.na(crawl_results$median_creation_date), ]
  if (nrow(dates_with_files) > 0) {
    overall_median_date <- median(dates_with_files$median_creation_date, na.rm = TRUE)
  } else {
    overall_median_date <- as.Date(NA)
  }
  
  cat(sprintf("Total folders analyzed: %d\n", total_folders))
  cat(sprintf("Maximum folder depth: %d\n", max_depth))
  cat(sprintf("Total files found: %d\n", total_files))
  cat(sprintf("Total size: %.2f MB (%.2f GB)\n", total_size_mb, total_size_gb))
  cat(sprintf("Overall median creation date: %s\n", 
              ifelse(is.na(overall_median_date), "N/A", as.character(overall_median_date))))
  
  # Top 10 largest folders by file count
  cat("\nTop 10 folders by file count:\n")
  top_by_files <- crawl_results %>%
    arrange(desc(file_count)) %>%
    head(10) %>%
    select(folder_name, folder_path, file_count, total_size_mb, depth)
  print(top_by_files)
  
  # Top 10 largest folders by size
  cat("\nTop 10 folders by size (MB):\n")
  top_by_size <- crawl_results %>%
    arrange(desc(total_size_mb)) %>%
    head(10) %>%
    select(folder_name, folder_path, total_size_mb, file_count, depth)
  print(top_by_size)
  
  return(list(
    total_folders = total_folders,
    total_files = total_files,
    total_size_mb = total_size_mb,
    total_size_gb = total_size_gb,
    overall_median_date = overall_median_date,
    max_depth = max_depth
  ))
}

# Main execution function
main <- function() {
  cat("Google Drive Folder Crawler - Interactive Navigation\n")
  cat("====================================================\n\n")
  
  tryCatch({
    # Authenticate
    authenticate_drive()
    
    # Navigate to select starting folder
    cat("\nNavigate to your desired starting folder:\n")
    cat("You can drill down through folders and select 'SCAN ALL' when ready.\n")
    
    selected_folder <- navigate_folders()
    
    if (is.null(selected_folder)) {
      cat("Operation cancelled by user.\n")
      return(NULL)
    }
    
    cat(sprintf("\nStarting recursive crawl from: %s\n", selected_folder$path))
    
    # Confirm before starting
    confirm <- readline(prompt = "Continue with crawl? (y/n): ")
    if (tolower(substr(confirm, 1, 1)) != "y") {
      cat("Crawl cancelled by user.\n")
      return(NULL)
    }
    
    # Start crawling
    start_time <- Sys.time()
    crawl_results <- crawl_folder(selected_folder$id, selected_folder$name)
    end_time <- Sys.time()
    
    cat(sprintf("\nCrawl completed in %.2f seconds\n", 
                as.numeric(difftime(end_time, start_time, units = "secs"))))
    
    # Create and display summary
    summary_stats <- create_summary(crawl_results)
    
    # Save results to CSV
    safe_folder_name <- gsub("[^A-Za-z0-9_-]", "_", selected_folder$name)
    output_file <- paste0("drive_crawl_", safe_folder_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    write.csv(crawl_results, output_file, row.names = FALSE)
    cat(sprintf("\nDetailed results saved to: %s\n", output_file))
    
    # Return results for further analysis
    return(list(
      crawl_data = crawl_results,
      summary = summary_stats,
      output_file = output_file,
      starting_folder = selected_folder
    ))
    
  }, error = function(e) {
    cat(sprintf("Error during execution: %s\n", e$message))
    cat("Please check your authentication and try again.\n")
  })
}

# Run the script
if (interactive()) {
  results <- main()
} else {
  cat("This script is designed to run interactively.\n")
  cat("Please run it in an interactive R session.\n")
}
