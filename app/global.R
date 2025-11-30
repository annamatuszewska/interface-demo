
# global.R

# ── Libraries ──────────────────────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidytext)
library(stopwords)
library(plotly)
library(readr)
library(DT)
library(shinyjs)
library(RColorBrewer)
library(writexl)
library(shinyWidgets)

#library(RColorBrewer)
#library(dplyr)
#library(ggplot2)


# ── Data loading & preprocessing ────────────────────────────────────────────

#------------------------------------------------------------------------------#
#–– Helper: pick first n files per group, ordered by a column
#------------------------------------------------------------------------------#

pick_n_by_group <- function(df, group_col, order_col, n = 100) {
  df %>%
    group_by(across(all_of(group_col))) %>%
    arrange(across(all_of(order_col))) %>%
    slice_head(n = n) %>%
    ungroup()
}

# Lyrics & NRC sentiment lexicon (for Text Analysis)
nrc_emotionen <- read.delim("www/emot/German-NRC-EmoLex.txt", header=T, sep="\t")
nrc_emotionen$word <- tolower(nrc_emotionen$German.Word)
nrc_emotionen <- tibble(nrc_emotionen) %>% select( !ends_with(".Word")) %>% 
  pivot_longer(!word, names_to = "sentiment") %>% 
  filter(value != 0) %>% select(word, sentiment) %>% distinct()   

stop_words_de <- tibble(word = stopwords("de", "marimo"))

meta_csv <- read_csv("www/lyrics_with_metadata.csv", col_types = cols())
meta_small <- pick_n_by_group(meta_csv, "corpus", "filename", 100)%>%
  # rename OTL-Incipit → incipit
  rename(incipit = `OTL-incipit`)
lyrics <- meta_small %>%
  transmute(
    doc_id    = filename,
    corpus,
    subcorpus,
    text      = lyrics
  ) %>%
  mutate(
    text = map_chr(text, ~ paste(unique(str_split(.x, "\n")[[1]]), collapse = "\n"))
  )

lyrics_clean <- lyrics %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_de, by = "word") %>%
  filter(!is.na(corpus), !is.na(subcorpus))


# lyrics for text search
lyrics_df <- meta_small  %>%
  select(
    filename,
    filename_source,
    corpus,
    subcorpus,
    lyrics,
    source_text,
    incipit 
  )


# Combined score & melody data (for Rhythm/Music Analysis)
data_raw <- read_csv("www/combined_data.csv", col_types = cols())

keep_files <- pull(meta_small, filename)

# region
data_raw <- data_raw %>%
  filter(filename %in% keep_files) %>%
  mutate(
    region = map_chr(strsplit(as.character(ARE), ","), function(x) {
      if (length(x) >= 4) paste(trimws(x[3]), trimws(x[4]), sep = ": ")
      else if (length(x) >= 3) trimws(x[3])
      else "no region"
    }) %>%
      str_remove("\\.$") %>% str_trim(),
    GTL_trimmed = str_remove(GTL, "\\.+$") %>% str_trim(),
    meters = if_else(is.na(meters) | str_detect(meters, ","), "mixed", as.character(meters))
  )

# ambitus
ambitus_data <- data_raw %>%
  group_by(filename) %>%
  summarise(
    max_pitch = max(pitch, na.rm = TRUE),
    min_pitch = min(pitch, na.rm = TRUE),
    ambitus   = max_pitch - min_pitch,
    .groups   = "drop"
  )
data_raw <- left_join(data_raw, ambitus_data, by = "filename")


# Utility to normalize the 'meters' column:
clean_meters <- function(x) {
  # x should be a character vector
  sapply(x, function(val) {
    if (is.na(val) || grepl(",", val)) {
      "mixed"
    } else {
      as.character(val)
    }
  })
}


# Precompute unique filter choices
regions        <- sort(unique(data_raw$region))
types          <- sort(unique(na.omit(data_raw$GTL_trimmed)))
meters         <- sort(unique(data_raw$meters))
tonalities     <- sort(unique(na.omit(data_raw$key)))
titles         <- sort(unique(na.omit(data_raw$OTL)))
scale_degrees  <- sort(unique(na.omit(as.character(data_raw$scl_value))))
scale_degrees_all <- scale_degrees

data <- data_raw
rm(data_raw)
data <- data %>% 
  rename(sixteenth_value = `16th_value`)%>%
  rename(incipit = `OTL-incipit`)



corpus_info <- list(
  K1 = list(
    description = "The term <i>Altdeutsches Liederbuch</i>, as used by Böhme, does not reflect a strictly historical or literary classification, 
          but rather a cultural one. The melodies are meant to reflect <i>altdeutsche Sitten und Trachten</i>—Old German customs and traditions. 
          Böhme’s own definition of the folksong emphasizes its origins, function, and oral transmission:<br><br>
    
          <i>What is a folksong? We understand it as a song that originated among the people, was sung often and with joy by them, 
          and was passed on and preserved through oral tradition, because of its simple form, universal human content, 
          and its simplicity—whether secular or sacred. […] Folksongs are thus the old songs once sung indiscriminately by princes and peasants, 
          citizens and nobles, clergy and laymen, vagabonds and outlaw knights, craftsmen and country maidens et. alike. 
          Songs that were passed down orally over generations, often surviving to the present day, sometimes in slightly altered forms. 
          They are the songs rooted in the people’s soul and voice.</i> (p. XXII)",
    url = "https://purl.uni-rostock.de/rosdok/ppn738218359"
  ),
  K2 = list(
    description = "
      <i>We are concerned here with ‘volksthümlichen Lidern’. 
      This is what we call the songs written by known or unknown poets and composers, 
      which have passed into the vernacular with little or no change and have become ‘favourite tunes’ 
      without really being folk songs. […] The popular song originates from the circles of the educated, 
      but in terms of content and language it is composed using generally understandable expressions and phrases, 
      and is etherefore sung by the masses. These appealing artistic compositions become true folk songs once 
      the people have ‘processed’ them in their own way.</i>
    ",
    url = "https://archive.org/details/BoehmeVtLieder1895"
  ),
  K3 = list(
    description = "
      <i>Everything we present, however, is truly folk-like in the German sense. But by <folk>
      we mean what should always be understood by it in Germany — not merely the uneducated or even primitive masses, 
      but the entire German people — from prince to peasant, from the highest to the lowest social class...</i>
    ",
    url = "https://mdz-nbn-resolving.de/details:bsb11144622"
  ),
  K4 = list(
    description = "
      <i>In accordance with the title of the book, only true folk songs were included
      – that is, songs that emerged within the folk tradition, were composed by unknown authors,
      were spread, transformed, and preserved through oral tradition, are in part still sung today, 
      they can be distinguished from art songs by their simple form and universal, objective content.</i>
    ",
    url = "https://rosdok.uni-rostock.de/resolve/id/rosdok_document_0000006417"
  ),
  K5 = list(
    description = "
      <i>I have made every effort to exclude any song with a known scholarly author;
      all the songs originate from natural poets — those who truly composed poetry 
      without intending to do so…</i>
    ",
    url = "https://mdz-nbn-resolving.de/details:bsb11160571"
  )
)

