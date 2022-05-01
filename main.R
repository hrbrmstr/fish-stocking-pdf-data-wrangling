library(rvest) # 1.0.2
library(stringi) # 1.7.6
library(pdftools) # 3.1.1
library(tidyverse) # 2.1.5

dir.create("./reports", showWarnings = FALSE)

# read the page with the stocking reports URLs ----
pg <- read_html("https://www.maine.gov/ifw/fishing-boating/fishing/fish-stocking-report.html")

# extract the PDF report urls ----
pg |>
  html_nodes(
    xpath = ".//a[contains(@href, 'Annual Fish') or contains(@href, 'current')]"
  ) |> 
  html_attr("href") |>
  stri_replace_first_fixed(
    pattern = "../..",
    replacement = "https://www.maine.gov/ifw"
  ) -> report_urls

# download them locally ----
download.file(
  url = URLencode(report_urls),
  destfile = file.path("./reports", basename(report_urls)),
  method = "libcurl"
)

# clean & extract the tables ----
list.files("./reports", full.names = TRUE) |> 
  map_df(\(.x) {
    
    # |- read in the pdf text ----
    pgs <- pdftools::pdf_text(.x)
    
    # |- split "\n" delimited lines into character vector ----
    pgs |> 
      stri_split_lines() |> 
      unlist() -> lines
    
    # |- get the year of the report
    year <- substr(stri_trim_both(lines[[1]]), 1, 4)
    
    # |- get all the counties in the report ----
    stri_match_first_regex(lines, "^([[:alpha:][:space:]]+ County)")[,2] |> 
      discard(is.na) |> 
      stri_trim_both() -> counties
    
    # |- we need every chr vctr index where the county name is at the beginning of the line ----
    starts <- grep(sprintf("^[[:space:]]*(%s)", paste0(counties, collapse = "|")), lines)
    
    # |- we need every chr vctr index where "End of" is ----
    ends <- grep("End of", lines)
    
    # |- older reports have the county name on every page and we just need the first one ----
    tibble(counties, starts) |> 
      distinct(counties, .keep_all = TRUE) |>
      pull(starts) -> starts
    
    # |- a bit more cleaning and then extract the records into a data frame ----
    map2_df(starts, ends, \(start, end) {
      
      # |-- we don't want leading/trailing whitespace ----
      records <- stri_trim_both(lines[start:end])
      
      # |-- we need this to add to the data frame ----
      county <- records[1]
      
      # |-- the records/observations start with a date, so we just care about those lines ----
      records <- records[stri_detect_regex(records, "^[[:digit:]]{1,2}/")]
      
      # |-- this is a hack so we can use read.csv to do the hard work for us. ----
      # |-- we replace sequences of 3 or more spaces to tabs, with a special ----
      # |-- case of sticking a tab after the `date` part of the line ----
      records <- stri_replace_all_regex(records, "[[:space:]]{3,}", "\t")
      
      stri_replace_all_regex(
        records, 
        sprintf("%s[[:space:]]+", year), 
        sprintf("%s\t", year)
      ) -> records
      
      # |-- turn the chr vctr into something read.csv can handle ----
      records <- paste0(records, collapse = "\n")
      
      # |-- make the data frame ----
      read.csv(text = records, header=FALSE, sep="\t") |> 
        as_tibble() |> 
        set_names( # |-- make real column names ----
                   c("date", "water", "muni", "species", "qty", "size")
        ) |>
        mutate_all(as.character) |> # |-- there are messed up fields, so we need to do this for now to avoid errors ----
      mutate( # |-- add the county and year ----
              county = county,
              year = year
      ) 
      
    })
    
  }) -> xdf # |_ yay! we have a data frame ----

# this is what Maine stocks each year  ----
c(
  "BROOK TROUT", "BROWN TROUT", 
  "RAINBOW TROUT", "LAKE TROUT", 
  "L.L. SALMON", "SPLAKE"
) -> fish_species

# we turn ^^ into a regex so we can do easy finding/replacing in malforned records ----
species_regex <- sprintf("(%s)$", paste0(fish_species, collapse = "|"))

# fix malformed records ----
xdf |> 
  mutate(
    tmp = size, # |- make a temporary column to hold valid `size` b/c we will be shifting data from `muni`, `species`, and `qty` in malformed records ----
    size = case_when(
      is.na(tmp) ~ qty, # |- copy over the proper `size` in malformed records ----
      TRUE ~ size 
    ),
    qty = case_when(
      is.na(tmp) ~ species, # |- copy over the proper `qty` in malformed records ----
      TRUE ~ qty
    ),
    species = case_when( # |- use the regex to pull the proper `species` from malformed records ----
                         is.na(tmp) ~ stri_match_last_regex(muni, species_regex)[,2], 
                         TRUE ~ species
    ),
    muni = case_when( # |- use the regex to clean up the `muni` column in malformed records ----
                      is.na(tmp) ~ stri_replace_last_regex(muni, species_regex, "") |> 
                        stri_trim_both(),
                      TRUE ~ muni
    ),
    date = lubridate::mdy(date) # |- make the date a proper date ----
  ) |> 
  mutate_at(
    vars(qty, size), as.numeric # |- now we can have numeric columns ----
  ) |>  
  select(-tmp) |> # |_ we no longer need the `tmp` column ----
  write_csv("./data/stocking-report.csv")
