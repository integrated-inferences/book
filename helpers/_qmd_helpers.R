# Functions to change labels and cross references from Rmd to quarto

library(stringr)


# List all .Rmd files in the directory (including subdirectories)
# rmd_files <- list.files(book_dir, pattern = "\\.Rmd$", recursive = TRUE, full.names = TRUE)

# Function to convert snake_case options to kebab-case
convert_options <- function(options) {
  str_replace_all(options, "\\b([a-zA-Z0-9]+)_([a-zA-Z0-9]+)\\b", "\\1-\\2")
}

# Function to determine if a chunk produces a figure
prefix <- function(chunk_name) {
  if(length(chunk_name) == 0 ) return("")
  if(is.na(chunk_name)) return("")
  if(str_detect(chunk_name, "HJ-F")) return("fig-")
  if(str_detect(chunk_name, "HJ-T")) return("tbl-")
  return("")
}

process_content <- function(content) {
  # Regex pattern to capture chunk headers and options
  pattern <- "```\\{r\\s+([\\w\\-]+)([\\s\\S]*?)\\}"

  # Find all matches
  matches <- str_match_all(content, pattern)

  # Loop through matches and replace them
  for (i in seq_len(length(matches))) {
    match <- matches[i][[1]]
    if(length(matches[i][[1]][,1]) > 0){

    full_match <- match[, 1]   # Entire matched string
    chunk_name <- str_trim(match[,2]) # Chunk name
    options <- str_trim(match[,3])    # Options

    # Debugging: print captured groups
    print(paste("Chunk Name:", chunk_name))

    # Convert options to kebab-case
    kebab_options <- convert_options(options)

    # Determine prefix based on whether it's a figure chunk
    label_prefix <- prefix(chunk_name)

    # Construct the new Quarto chunk header
    new_header <- paste0(
      "```{r}\n#| label: ", label_prefix, chunk_name, "\n#| ", kebab_options
    )

    # Replace in content
    content <- str_replace(content, fixed(full_match), new_header)
    }
  }

  return(content)
}


convert_options <- function(options) {
  # Remove leading comma and whitespace
  options <- str_trim(str_remove(options, "^,"))

  # Convert snake_case to kebab-case
  kebab_options <- str_replace_all(options, "\\b([a-zA-Z0-9]+)_([a-zA-Z0-9]+)\\b", "\\1-\\2")

  # Add a newline after each option for better formatting in Quarto
  kebab_options <- str_replace_all(kebab_options, ",\\s*", "\n#| ")
  kebab_options <- str_replace_all(kebab_options, "=", ": ")

  return(kebab_options)
}

process_file <- function(file) {
  # Read the content of the file
  content <- readLines(file, encoding = "UTF-8")

  # Replace @ref(tab:...) with @tbl-... and @ref(fig:...) with @fig-...
  content <- str_replace_all(content, "\\\@ref\\(tab:(.*?)\\)", "@tbl-\\1")
  content <- str_replace_all(content, "\\\@ref\\(fig:(.*?)\\)", "@fig-\\1")

  content <- str_replace_all(content, "Table @", "@")
  content <- str_replace_all(content, "Figure @", "@")

  content <- str_replace_all(content, "HJC", "sec-HJC")
  content <- str_replace_all(content, "@ref\\((.*?)\\)", "@\\1")
  content <- str_replace_all(content, "sec-sec", "sec")
  content <- str_replace_all(content, "@sec-", "@sec-")

  content <- process_content(content)

  # Write the updated content back to the file
  writeLines(content, file, useBytes = TRUE)
}

process_file("16-evaluating-models.qmd")

# Apply the function to all .Rmd files
# lapply(rmd_files, process_file)


