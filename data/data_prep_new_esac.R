library(readr)
library(humdrumR)
library(dplyr)
library(purrr)
library(stringr)
library(tidyverse)
library(readxl)

#1.SET UP THE FOLDER:

#input work directory (modify for your folder path):
wd <- setwd("...\\interface-demo")  
#input the name of the folder adding "\\" between folders:
folder_name <- "data_prep"
#select file
select.files <- list.files(path = paste(wd,"\\",folder_name, sep=""), pattern = "krn")
select.files

#2.LOAD FUNCTIONS:
# function for marking phrases
wr_phrase <- function(vector) {
  vr <- c() ### an empty vector 
  counter <- 1
  no_seq <- 0
  for (i in 1:length(vector)) {
    if (i==1) {
      if (grepl("{", vector[i], fixed = TRUE)) {
        vr[i] <- counter
      } else {
        vr[i] <- no_seq
      }
    } else if (vr[i-1]!=0) {
      if (grepl("}", vector[i-1], fixed = TRUE) & grepl("{", vector[i], fixed = TRUE)) {
        counter <- counter + 1
        vr[i] <- counter
      } else if (grepl("}", vector[i-1], fixed = TRUE) & !(grepl("{", vector[i], fixed = TRUE))) {
        counter <- counter + 1
        vr[i] <- no_seq
      } else {
        vr[i] <- counter
      }
    } else if (vr[i-1]==0) {
      if (grepl("{", vector[i], fixed = TRUE)) {
        vr[i] <- counter 
      } else {
        vr[i] <- no_seq  
      }
    }
  }
  vr
}

# function to determine where the minus is
wr_minus <- function(vector) {
  vr <- c() ### an empty vector 
  for (i in 1:length(vector)) {
    if (grepl("-", vector[i], fixed = TRUE)) {
      vr[i] <- TRUE
    } else {
      vr[i] <- FALSE
    }
  }
  vr
}


# ------------------------- helper: safely try to read a Humdrum file ----------------------

can_open <- function(fname) {
  full_path <- file.path(folder_name, fname)
  res <- try({
    readHumdrum(full_path)|> 
      filter(Spine == 1)
    }, silent = TRUE)
  !inherits(res, "try-error")
}

# apply to all files
open_results <- vapply(select.files, can_open, logical(1))


file_to_test <- "am_deut107.krn"

# run the helper
ok <- can_open(file_to_test)

# report
if (all(open_results)) {
  message("✅ All files opened successfully.")
} else {
  bad <- names(open_results)[!open_results]
  warning("⚠️ Failed to open: ", paste(bad, collapse = ", "))
}


# ----------------------------- end of helper ------------------------------


#3. CREATE FOLDER "csv" AND GENERATE CSV FILES:

for(i in select.files){
  #added measure and metre data:
  ad <- readHumdrum(paste(folder_name, i, sep = "\\")) |> 
    filter(Spine == 1)
  bar_info <- getHumtab(ad, 'D')[ , c('Bar')]
  time_signature <- try(getHumtab(ad, 'D')[ , c('TimeSignature')])
  ifelse (class(time_signature) == "try-error", time_signature <- gsub("(.*)", "NA", bar_info), time_signature)
  
  if ("Key" %in% colnames(getHumtab(ad, 'D'))) {
    key_info <- getHumtab(ad, 'D')[ , c('Key')]
  } else {
    key_info <- NA
  }
  ifelse (class(key_info) == "try-error", key_info <- gsub("(.*)", "NA", bar_info), key_info)
  
  
  #create data frames:
  rh <- readHumdrum(paste(folder_name, i, sep = "\\"))|> 
    filter(Spine == 1)
  summary_kern <- rh$Token
  duration_info <- recip(rh$Token)
  note_name_info <- kern(rh$Token)
  mint_info <-  mint(rh$Token)
  semits_info <- semits(rh$Token)
  midi_info <- midi(rh$Token)
  deg_info <- deg(rh$Token)

  #final data frame
  split_df <- cbind(as.data.frame(summary_kern), as.data.frame(summary_kern),as.data.frame(deg_info), as.data.frame(duration_info), as.data.frame(note_name_info), as.data.frame(mint_info), as.data.frame(midi_info))
  colnames(split_df) <- c("summary","split","degree","duration","note_name","mint", "pitch")
  id <- c(1:nrow(split_df))
  
  mint_vector <- split_df[,"mint"]
  interval <- as.numeric(gsub("\\D", "", mint_vector))
  split_signs <- split_df[,"split"]
  #use functions
  phrases <- wr_phrase(split_signs)
  mint_direction <- wr_minus(mint_vector)
  
  #final data frame
  final_df <- cbind(as.data.frame(id),as.data.frame(summary_kern), as.data.frame(bar_info), as.data.frame(time_signature), as.data.frame(key_info), as.data.frame(phrases),as.data.frame(deg_info), as.data.frame(duration_info), as.data.frame(note_name_info),as.data.frame(mint_direction), as.data.frame(mint_info), as.data.frame(interval), as.data.frame(midi_info))
  colnames(final_df) <- c("id","summary", "measure", "metre", "key", "phrases","degree","duration","note_name","mint_direction","mint", "interval","pitch")
  #save to CSV
  i_name <- tolower(sub(".krn","",i,fixed = TRUE))
  write.csv(final_df, paste(wd,"\\",folder_name,"\\csv\\",i_name,".csv",sep=""), row.names=FALSE)
} 

 
#4. METADATA

esac_files <- list.files(path = paste(wd, "\\esac", sep=""), pattern = "^esac_.*\\.txt$", full.names = TRUE)

scl_deg_data <- map_df(esac_files, function(file) {
  file_content <- readLines(file, warn = FALSE)
  scl_line <- file_content[grepl("SCL_SEM", file_content)]
  scl_value <- ifelse(length(scl_line) > 0, str_extract(scl_line, "(?<=\\[).+?(?=\\])"), NA)
  
  Filename <- paste0("am_", sub("esac_(.*)\\.txt", "\\1", basename(file)))
  
  data.frame(Filename, scl_value, stringsAsFactors = FALSE)
})


for (i in select.files) {
  
  file_path <- file.path(folder_name, i)  # Poprawne tworzenie ścieżki
  
  rh <- readHumdrum(file_path)
  
  if (!is.null(rh)) {  # Sprawdzenie, czy plik został poprawnie wczytany
    metadata_short <- reference(rh)

    if ("TimeSignature" %in% names(rh)) {
      unique_values <- unique(rh$TimeSignature[!is.na(rh$TimeSignature)])
      unique_values_string <- paste(unique_values, collapse = ", ")
    } else {
      unique_values_string <- NA
      warning(paste("Brak TimeSignature w obiekcie dla pliku:", i))
    }
    
    metadata_short$Filename <- sub("\\.krn$", "", metadata_short$Filename)
    
    metadata_short$meters <- unique_values_string  # Dodanie wartości do tabeli
    
    metadata_short <- metadata_short 
    
    if (!is.null(metadata_short)) {  # Sprawdzenie, czy metadane istnieją
      metadata_short <- as.data.frame(metadata_short)%>% 
        select(any_of(c(
          "Piece",       "Filename",    "AGN",   "AIN",   "AMT",
          "ARE",         "COM",         "GTL",   "ONB",   "OTL",
          "OTL-incipit", "SCT",         "URL-scan", "page",
          "meters",      "corpus",      "subcorpus", "AMD", "Text"
        )))
      
      metadata_short <- left_join(metadata_short, scl_deg_data, by = "Filename")
      
      if (i == select.files[1]) {
        tab_start <- metadata_short
      } else {
        tab1 <- metadata_short
        tab_start <- bind_rows(tab_start, tab1)
      }
    } else {
      warning(paste("Brak metadanych w pliku:", i))
    }
  } else {
    warning(paste("Nie udało się wczytać pliku:", i))
  }
}

tab_start <- tab_start %>%
  # convert Filename to all lowercase
  mutate(Filename = tolower(Filename)) 

corpora_meta <- read_excel(file.path(wd, "corpora.xlsx")) %>%
  rename(Filename = filename) %>%                # align column name
  mutate(Filename = sub("\\.krn$", "", Filename)) # drop the .krn extension


# ─── Join corpus & subcorpus onto your metadata ────────────

tab_start <- tab_start %>%
  left_join(
    corpora_meta %>% select(Filename, corpus, subcorpus),
    by = "Filename"
  )

#SAVE metadata:
write.table(tab_start, file = paste(wd, "/metadane.csv", sep=""), 
            sep = "\t", row.names = FALSE, fileEncoding = "UTF-8", quote = FALSE)


#5. UNION AND JOIN

# Load list of CSV files, load only files that start with "am_deut"

folder_name <- "data_prep\\csv"
files <- list.files(path = paste(wd,"\\",folder_name, sep=""), pattern = "^am_deut.*\\.csv$", full.names = TRUE)


# Load first file and determine data types
first_file <- files[1]
col_spec <- spec(read_csv(first_file))
col_spec$cols$duration <- col_character()

# Function for loading CSV files and adding filename
read_csv_file <- function(file) {
  df <- read_csv(file, col_types = col_spec)  # Wczytaj plik CSV jako ramkę danych
  df <- mutate(df, filename = sub("\\.csv$", "", basename(file)))  # Dodaj kolumnę z nazwą pliku
  return(df)
}

# Load and combine all tables
combined_data <- map_dfr(files, read_csv_file)

# Load file metadane.csv
metadata <- readr::read_delim("metadane.csv", delim = "\t", col_types = cols())

colnames(metadata)[colnames(metadata) == "Filename"] <- "filename"

# Left join na polu filename
combined_data <- inner_join(combined_data, metadata, by = "filename")

# Remove "+" i "-" in the mint column and save in the new column interval_symbol
combined_data <- combined_data %>%
  mutate(interval_symbol = str_remove(mint, "^[+-]"))

# Load file dict_pitch.xlsx
dict_pitch <- readxl::read_excel("dict/dict_pitch.xlsx")
combined_data <- left_join(combined_data, dict_pitch, by = "pitch")

# Load file dict_intervals.xlsx
int_sym <- readxl::read_excel("dict/dict_intervals.xlsx")
combined_data <- left_join(combined_data, int_sym, by = "interval_symbol")

# Load file dict_note_length.xlsx
note_length <- readxl::read_excel("dict/dict_note_length.xlsx")

combined_data <- left_join(combined_data, note_length, by = "duration")

# Save to a CSV file
write.csv(combined_data, "combined_data.csv", row.names = FALSE)



# 6. FILE WITH SONG TEXTS, PLUS SOURCE CORPUS


# 1) Read & prep original lyrics
lyrics_data <- list.files("lyrics", "\\.txt$", full.names=TRUE) %>% 
  map_df(~ tibble(
    filename = sub("LTAN(\\d+)\\.txt$", "am_deut\\1", basename(.x)),
    lyrics   = paste(readLines(.x, encoding="UTF-8", warn = FALSE), collapse=" ")
  )) %>%
  mutate(id = str_extract(filename, "\\d+"))

# 2) Read & prep source texts (recursing into subdirs)
source_data <- list.files("lyrics_source", "\\.txt$", full.names=TRUE, recursive=TRUE) %>%
  map_df(~ {
    parts <- str_split(basename(dirname(.x)), "_", 2)[[1]]
    tibble(
      filename_source = tools::file_path_sans_ext(basename(.x)),
      source_text     = paste(readLines(.x, encoding="UTF-8", warn = FALSE), collapse=" ")
    )
  }) %>%
  mutate(id = str_extract(filename_source, "\\d+"))

# 3) Full outer join & reorder
combined <- full_join(lyrics_data, source_data, by = "id") %>%
  select(id, filename, filename_source, lyrics, source_text)

# 4) Read metadata & left‐join on filename
metadane <- readr::read_delim("metadane.csv", delim = "\t", col_types = cols())

final_data <- metadane %>%
  rename(filename = Filename) %>%
  left_join(combined, by = "filename")

# 5) Write out the joined CSV
readr::write_csv(final_data, "lyrics_with_metadata.csv")
