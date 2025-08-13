# Function to recursively scan folders and report sizes
scan_folder_sizes <- function(path, unit = "MB", show_files = FALSE) {
  
  # Check if path exists
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }
  
  # Helper function to convert bytes to specified unit
  convert_size <- function(bytes, unit) {
    switch(toupper(unit),
           "BYTES" = bytes,
           "KB" = bytes / 1024,
           "MB" = bytes / (1024^2),
           "GB" = bytes / (1024^3),
           bytes / (1024^2)  # default to MB
    )
  }
  
  # Helper function to format size with appropriate unit
  format_size <- function(size, unit) {
    paste(round(size, 2), unit)
  }
  
  # Function to get total size of a directory
  get_dir_size <- function(dir_path) {
    files <- list.files(dir_path, recursive = TRUE, full.names = TRUE, 
                       include.dirs = FALSE, all.files = TRUE)
    
    # Filter out files that don't exist or can't be accessed
    files <- files[file.exists(files)]
    
    if (length(files) == 0) {
      return(0)
    }
    
    # Get file sizes and sum them
    file_sizes <- file.info(files)$size
    file_sizes[is.na(file_sizes)] <- 0
    sum(file_sizes, na.rm = TRUE)
  }
  
  # Function to recursively process directories
  process_directory <- function(current_path, level = 0) {
    
    # Get all subdirectories
    subdirs <- list.dirs(current_path, recursive = FALSE, full.names = TRUE)
    
    # Get current directory size
    current_size <- get_dir_size(current_path)
    current_size_converted <- convert_size(current_size, unit)
    
    # Create indentation for tree structure
    indent <- paste(rep("  ", level), collapse = "")
    
    # Print current directory info
    cat(indent, basename(current_path), " - ", format_size(current_size_converted, unit), "\n")
    
    # If show_files is TRUE, list individual files in current directory
    if (show_files) {
      files <- list.files(current_path, full.names = TRUE, recursive = FALSE)
      files <- files[file.info(files)$isdir == FALSE | is.na(file.info(files)$isdir)]
      
      for (file in files) {
        if (file.exists(file)) {
          file_size <- file.info(file)$size
          if (!is.na(file_size)) {
            file_size_converted <- convert_size(file_size, unit)
            cat(indent, "  📄 ", basename(file), " - ", format_size(file_size_converted, unit), "\n")
          }
        }
      }
    }
    
    # Store result for summary
    result <- data.frame(
      Path = current_path,
      RelativePath = sub(paste0("^", gsub("\\\\", "/", normalizePath(path)), "/?"), "", 
                        gsub("\\\\", "/", normalizePath(current_path))),
      Size_Bytes = current_size,
      Size_Formatted = format_size(current_size_converted, unit),
      Level = level,
      stringsAsFactors = FALSE
    )
    
    # Process subdirectories
    subdir_results <- data.frame()
    if (length(subdirs) > 0) {
      for (subdir in subdirs) {
        subdir_result <- process_directory(subdir, level + 1)
        subdir_results <- rbind(subdir_results, subdir_result)
      }
    }
    
    # Combine results
    rbind(result, subdir_results)
  }
  
  cat("Scanning folder sizes for:", normalizePath(path), "\n")
  cat("Unit:", unit, "\n")
  cat(rep("=", 50), "\n", sep = "")
  
  # Process the directory tree
  results <- process_directory(path)
  
  cat(rep("=", 50), "\n", sep = "")
  cat("Scan completed!\n\n")
  
  # Return results invisibly so they can be captured if needed
  invisible(results)
}

# Enhanced version with summary statistics
scan_folder_sizes_with_summary <- function(path, unit = "MB", show_files = FALSE, top_n = 10) {
  
  # Get the detailed results
  results <- scan_folder_sizes(path, unit, show_files)
  
  # Calculate summary statistics
  cat("SUMMARY STATISTICS:\n")
  cat(rep("-", 30), "\n", sep = "")
  
  total_size <- sum(results$Size_Bytes[results$Level == 0])
  cat("Total size:", format_size(convert_size(total_size, unit), unit), "\n")
  cat("Number of directories scanned:", nrow(results), "\n")
  
  # Top largest directories
  cat("\nTop", top_n, "largest directories:\n")
  top_dirs <- results[order(results$Size_Bytes, decreasing = TRUE), ][1:min(top_n, nrow(results)), ]
  
  for (i in 1:nrow(top_dirs)) {
    cat(i, ". ", top_dirs$RelativePath[i], " - ", top_dirs$Size_Formatted[i], "\n")
  }
  
  return(results)
}

# Helper function to convert bytes to human readable format
convert_size <- function(bytes, unit) {
  switch(toupper(unit),
         "BYTES" = bytes,
         "KB" = bytes / 1024,
         "MB" = bytes / (1024^2),
         "GB" = bytes / (1024^3),
         bytes / (1024^2)  # default to MB
  )
}

format_size <- function(size, unit) {
  paste(round(size, 2), unit)
}

cat("Folder size scanning functions loaded successfully!\n")
cat("\nUsage examples:\n")
cat("1. Basic scan: scan_folder_sizes('C:/your/path')\n")
cat("2. With summary: scan_folder_sizes_with_summary('C:/your/path')\n")
cat("3. Show files: scan_folder_sizes('C:/your/path', show_files = TRUE)\n")
cat("4. Different unit: scan_folder_sizes('C:/your/path', unit = 'GB')\n")
