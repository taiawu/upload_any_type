# library(outliers)
# library(readr) # to read files into dataframes
# library(signal) # contains the savitsky-golay filter, whichs masks poly() in stats
# library(stats)
# library(tibble)
# library(dplyr)
# library(purrr)
# library(nnet)
# library(readxl) # for read_excel for quantStudio



make_channel_vec <- function( df ) { # make the vector which specifies channel for each reading
  channels <- df %>% 
    group_by(`0`) %>%
    filter(n() == 1) %>%
    select(`0`) %>%
    as_vector()
  
  n_meas <- df %>%  # the number of wells measured per channel (always the same for all channels )
    group_by(`0`) %>%
    filter(n() > 1) %>%
    tally() %>% 
    nrow() 
  
  rep(channels , each = n_meas + 1) # add one, for the row which will still contain the channel itself
}

read_qTower_to_channel <- function( file_path ) {
  
  df_raw <- read_csv(file_path, 
                     col_names = c(0:500) %>% as.character()) %>% # read in 500 columns; this should exceed any actual run, and fill in columsn to right as NAs
    select_if(~sum(!is.na(.)) > 0) #%>% # remove all columns which are all NAs
  
  
  df <- df_raw %>%
    drop_na(ncol(.)) %>% # drop the header, which is NA in the tailing columns
    mutate( channel = make_channel_vec(.))
  
  
  out <- list(df = df, 
              channels = df %>% pull(channel) %>% unique())
}

read_qTower_by_channel <- function(df, channel_choice) {
  
  df %>% 
    filter(channel == channel_choice) %>%
    rename(well = "0") %>%
    filter(!well %in% .$channel) %>%
    mutate_at(vars(-well, -channel), as.numeric) %>%
    pivot_longer(-c("well", "channel"), names_to = "Temperature", values_to = "value") %>%
    pivot_wider( -channel, names_from = "well", values_from = "value")
  
}

qTower_load <- function(path, channel) {
  read_qTower_to_channel(path)$df %>%
    read_qTower_by_channel( . , channel)
}

read_uploads <- function(filepath, ext){
  switch(ext,
         csv = vroom::vroom(filepath, delim = ","),
         tsv = vroom::vroom(filepath, delim = "\t"),
         xlsx = readxl::read_excel(filepath),
         xls = readxl::read_excel(filepath),
         validate("Invalid file type"))
}


cycle_to_T_func <- function(start_T, inc_T, df) { # returns a numeric vector
  Temps <- rep(start_T, nrow(df))
  steps <- seq.int(from = 0, to = (nrow(df)-1), by = 1)
  steps2 <- steps*inc_T
  Temps2 <- steps2 + Temps
  df <- data.frame("Temperature" = Temps2)
  df
} 

make_channel_vec <- function( df ) { # make the vector which specifies channel for each reading
  channels <- df %>% 
    group_by(`0`) %>%
    filter(n() == 1) %>%
    select(`0`) %>%
    as_vector()
  
  n_meas <- df %>%  # the number of wells measured per channel (always the same for all channels )
    group_by(`0`) %>%
    filter(n() > 1) %>%
    tally() %>% 
    nrow() 
  
  rep(channels , each = n_meas + 1) # add one, for the row which will still contain the channel itself
}

read_qTower <- function( file_path ) {
  
  df_raw <- read_csv(file_path, 
                     col_names = c(0:500) %>% as.character()) %>% # read in 500 columns; this should exceed any actual run, and fill in columsn to right as NAs
    select_if(~sum(!is.na(.)) > 0) #%>% # remove all columns which are all NAs
  
  
  df <- df_raw %>%
    drop_na(ncol(.)) %>% # drop the header, which is NA in the tailing columns
    mutate( channel = make_channel_vec(.)) %>% # add channel as a column
    rename(well = "0") %>%
    
    filter(!well %in% .$channel) %>%
    
    mutate_at(vars(-well, -channel), as.numeric) %>%
    pivot_longer(-c("well", "channel"), names_to = "Temperature", values_to = "value") %>%
    mutate_at(vars(well, channel), as.character) %>%
    mutate_at(vars(Temperature, value), as.numeric) %>%
    mutate(channel_f = factor(.$channel, levels = c("FAM", "JOE", "TAMRA", "ROX", "Cy5", "Cy5.5", "SyproOrange")))
}


# quantStudio
read_quantStudio <- function(filename) { # completed
  df_raw <- readxl::read_excel(filename, skip=5, sheet = 4) %>%
    . [is.na(.[3]) == FALSE, c(2,4,5)] %>%
    set_names( . , c("Well", "Temperature", "RFU")) %>% 
    .[-1,] %>%
    pivot_wider(names_from = "Well", values_from = RFU) %>%
    mutate_if(is.factor, as.character) %>%
    mutate_all(as.numeric) %>%
    round(3)
}


format_stratagene <- function(df) {
  # determine if the data frame is a single chunk, or multiple
  chunk_num_raw <- table(df[,1]) # will be null if a single chunk
  d_rows <- which(df[,1] == "Dissociation Curve") # determine start rows of multiple chunks, if present
  df <- df[,-1] # remove the first column, needed only to determine chunk presence and locations
  
  if (length(chunk_num_raw) > 0 ) { # if multiple chunks present
    chunk_num <- (chunk_num_raw[[1]] + 1) # determine how many
    df_final <- df[1:d_rows[1],]
    
    for (i in c(1:(length(d_rows)-1))) { # stitch everything together into a single dataframe 
      df_chunk <- df[(d_rows[i]+1):(d_rows[i+1]),]
      df_final <- dplyr::bind_cols(df_final, df_chunk) # append temperature column  
    }
    df <- df_final # over-write input df 
  }
  
  df <- df[rowSums(is.na(df)) < ncol(df),] # remove rows which are all NAs, once present between chunks
  well_names <- as_vector(df[1,]) # extract well names
  well_names <- c("Temperature" , well_names[c(TRUE, FALSE)]) # remove empty columns from well names
  df <- df[-c(1,2),] #  remove leading empty rows
  temperatures <- data.frame( "Temperatures" = df[1]) # extract temperatures
  
  to_delete <- seq(1, ncol(df), 2) # remove duplicated temperature rows
  df <- df[,-to_delete] # remove duplicated temperature rows
  
  df <- dplyr::bind_cols(temperatures, df) # append temperature column
  df <- set_names(df, nm = well_names) # re-set the names
}

format_none <- function(filepath) { 
}

format_biorad <- function(df) {
  df[,-1]
}


#### making the various well names 
make_well_names <- function(row_style, num_style) {
  if (row_style == "rows") { rows <-  letters[1:16] } else {rows <- LETTERS[1:16] }
  if (num_style == "01") { cols <- c(c("01", "02", "03", "04", "05", "06", "07", "08", "09"), c(10:24)) } else {cols <- c(1:24) }
  
  int <-  list(row = rows,
               col =cols) %>%
    cross() %>% # general all possible combos of rows and columns
    map(lift(paste0)) %>% # pate them together
    
    as_vector() %>%
    as_tibble()  %>%
    mutate(nchar = lapply(.$value, nchar) %>% as_vector(),
           char =  str_extract_all(.$value, "[A-Z; a-z]", simplify = TRUE) 
           %>% str_to_upper(locale = "en") 
           %>% as_vector()) 
  
  
  if (num_style == "01") {
    ordered <- int %>%
      dplyr::arrange(value) %>%
      .[[1]]
  } else {
    ordered <- int %>%
      dplyr::arrange(char) %>%
      .[[1]]
  }
  ordered
}

# generate vectors of possible well names, for matching to layouts
WELLS1 <- make_well_names("ROWS", "1") # create well names, used in the uploading page 
wells_any <- c(WELLS1, # e.g. A1 .... P24
               make_well_names("ROWS", "01"), # e.g. A01 .... P24
               make_well_names("rows", "1"), # e.g. a1 .... p24
               make_well_names("rows", "01") # e.g. a01 .... p24
)

# e.g. use this way
# well_mappings <- tibble(
#   ROWS1 = make_well_names("ROWS", "1"),
#   ROWS01 = make_well_names("ROWS", "01"),
#   rows1 = make_well_names("rows", "1"),
#   rows01 = make_well_names("rows", "01")
# )