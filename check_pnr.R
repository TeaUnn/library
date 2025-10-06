################################################################################
## functions for checking whether an input is a valid
## swedish personnummer or samordningsnummer (coordination number).
##
## check_pnr takes ONE value. check pnrs takes several.
##
## input can be either num or char (but is coerced to char).
##
## the functions check:
##  what separator is used ('-' is OK) and if any other characters are present,
##  the length (10 or 12 digits allowed),
##  what century it is and note if !20th or !21st,
##  if it is a coordination number (60 added to date),
##  if it is a valid date,
##  if the date is in the future and if so note this,
##  the Luhn checksum (https://sv.wikipedia.org/wiki/Luhn-algoritmen),
##  and if it indicates male or female.
##
## output:
## valid = TRUE or FALSE,
## sex = 'male' or 'female', or 'NA' if (!valid),
## coordination_number = TRUE or FALSE, or 'NA' if (!valid),
## reason_invalid = if (!valid) reasons else NA,
## note = if (length(notes) > 0) notes else NA
################################################################################

check_pnr <- function(pnr) {
  pnr <- as.character(pnr)  # Ensure input is treated as a string
  reasons <- c()
  notes <- c()
  
  # Check for non-digit separator
  separator_match <- regmatches(pnr, regexpr("[^0-9]", pnr))
  if (length(separator_match) > 0 && separator_match != "-") {
    notes <- c(notes, sprintf("Unexpected separator or character: '%s'", separator_match))
  }
  
  # Remove all non-digit characters
  pnr_clean <- gsub("[^0-9]", "", pnr)
  
  # Check length
  if (!(nchar(pnr_clean) == 10 || nchar(pnr_clean) == 12)) {
    reasons <- c(reasons, "Invalid length (should be 10 or 12 digits)")
    return(list(
      valid = FALSE, 
      sex = NA, 
      coordination_number = NA, 
      reason_invalid = paste(reasons, collapse = "; "),
      note = if (length(notes) > 0) paste(notes, collapse = "; ") else NA
    ))
  }
  
  # Extract century if present
  century_prefix <- NA
  if (nchar(pnr_clean) == 12) {
    century_prefix <- substr(pnr_clean, 1, 2)
    if (!(century_prefix %in% c("19", "20"))) {
      notes <- c(notes, sprintf("Unexpected century prefix: %s (expected '19' or '20')", century_prefix))
    }
    pnr_clean <- substr(pnr_clean, 3, 12)
  }
  
  # Extract date components
  year <- as.integer(substr(pnr_clean, 1, 2))
  month <- as.integer(substr(pnr_clean, 3, 4))
  day <- as.integer(substr(pnr_clean, 5, 6))
  serial <- substr(pnr_clean, 7, 9)
  checksum_digit <- as.integer(substr(pnr_clean, 10, 10))
  
  # Check for coordination number (day > 60)
  coordination_number <- FALSE
  if (day > 60) {
    coordination_number <- TRUE
    day <- day - 60
  }

  # Validate date
  valid_date <- FALSE
  if (!is.na(century_prefix)) {
    full_year <- as.integer(paste0(century_prefix, sprintf("%02d", year)))
    date_str <- sprintf("%04d-%02d-%02d", full_year, month, day)
    if (!is.na(as.Date(date_str, format = "%Y-%m-%d"))) {
      valid_date <- TRUE
    }
  } else {
    # fallback: try 1900 and 2000
    for (century in c(1900, 2000)) {
      full_year <- century + year
      date_str <- sprintf("%04d-%02d-%02d", full_year, month, day)
      if (!is.na(as.Date(date_str, format = "%Y-%m-%d"))) {
        valid_date <- TRUE
        break
      }
    }
  }
  if (!valid_date) {
    reasons <- c(reasons, "Invalid date")
  }
  
  # Check if the date hasn't happened yet...
  if (as.Date(date_str, format = "%Y-%m-%d") > as.Date(Sys.Date(), format = "%Y-%m-%d")) {
    notes <- c(notes, "Is born in the future")
  }
  
  # Luhn checksum (https://sv.wikipedia.org/wiki/Luhn-algoritmen)
  digits <- as.integer(strsplit(substr(pnr_clean, 1, 9), "")[[1]])
  weights <- rep(c(2, 1), length.out = 9)
  sum_digits <- sum(sapply(digits * weights, function(x) if (x > 9) x - 9 else x))
  expected_checksum <- (10 - (sum_digits %% 10)) %% 10
  if (expected_checksum != checksum_digit) {
    reasons <- c(reasons, "Invalid checksum")
  }
  
  # Sex detection (odd = male, even = female)
  sex_digit <- as.integer(substr(serial, 3, 3))
  sex <- ifelse(sex_digit %% 2 == 0, "female", "male")
  
  # Final result
  valid <- length(reasons) == 0
  return(list(
    valid = valid,
    sex = if (valid) sex else NA,
    coordination_number = if(valid) coordination_number else NA,
    reason_invalid = if (!valid) paste(reasons, collapse = "; ") else NA,
    note = if (length(notes) > 0) paste(notes, collapse = "; ") else NA
  ))
}

#####
check_pnrs <- function(pnrs) {
  results <- lapply(pnrs, check_pnr)
  df <- do.call(rbind, lapply(seq_along(results), function(i) {
    res <- results[[i]]
    data.frame(
      pnr = pnrs[i],
      valid = res$valid,
      sex = res$sex,
      coordination_number = res$coordination_number,
      reason_invalid = res$reason_invalid,
      note = res$note,
      stringsAsFactors = FALSE
    )
  }))
  
  return(df)
}
