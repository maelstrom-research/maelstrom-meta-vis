# knitr::stitch_rmd(script="./___/___.R", output="./___/___/___.md")
# These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console 

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./manipulation/function-support.R")  # assisting functions for data wrangling and testing
source("./manipulation/object-glossary.R")   # object definitions
source("./scripts/common-functions.R")       # reporting functions and quick views
source("./scripts/graphing/graph-presets.R") # font and color conventions
# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(dplyr)
library(magrittr) #Pipes
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables

# ---- declare-globals ---------------------------------------------------------
# link to the csv file containing the coverage table
path_input <- "./data-unshared/raw/coverage-cognitive.csv"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script (prepared data set)
path_save <- "./data-unshared/derived/dto-1"
# See definitions of commonly  used objects in:
source("./manipulation/object-glossary.R")   # object definitions
# indicate a place to store meta data extracted from the coverage table
path_save_meta <- "./data-unshared/derived/coverage-cognitive-live.csv"
# path to csv that maps cognitive measures to domains (built from path_save_meta)
path_input_meta <- "./data-public/meta/coverage-cognitive-dead.csv"


# ---- utility-functions ----------------------------------------------------- 
# convert a character variance into ASCII encoding
convert_to_ascii <- function( x ) {
  iconv(x, "latin1", "ASCII//TRANSLIT")
}
# ---- load-data ---------------------------------------------------------------
# load the convergence table and give it a generic name
ds <- readr::read_csv(path_input,skip = 2) %>% tibble::as_tibble()

# ---- inspect-data -----------------------------------------------------------


# ---- tweak-data-1 -------------------------------------------------------------
# identify the function of variables with respect to THIS wide-long tranformation
variables_static <- c("Study","Start",	"End")
# all other variables will be considered "dynamic", i.e. moving during elongation
variables_dynamic <- setdiff(colnames(ds), variables_static) # 209 items

# tranform from wide to long with respect to measure 
ds_long <- ds %>% 
  # items_m - how many items does this measure contain? 
  tidyr::gather_("measure","items_m", variables_dynamic) %>% 
  # correct for troublesome characters in strings
  dplyr::mutate(
    measure = gsub("\\'s",'s',measure), # some apostrophes are apostrophes
    measure = convert_to_ascii(measure), # others are single quote
    measure = gsub("a\\?\\?",'',measure) # replace
  ) %>% 
  # dplyr::select(-Start, -End) %>% # we may not want this
  dplyr::arrange(Study) # order by study
# make the names of the variables lowercase for ease of typing
names(ds_long) <- tolower(names(ds_long))
# basic inspection
ds_long %>% head()
ds_long %>% glimpse()

# ---- tweak-data-2 -------------------------------------------------------------
# save unique measure names / extract meta data
ds_long %>%
  dplyr::distinct(measure) %>%
  dplyr::arrange(measure) %>% 
  readr::write_csv(path_save_meta)

# edit the meta data spreadsheed manually and save it in `./data-public/meta/`
# ...
# import the augmented meta data CONTAINING DOMAIN MAPPING
ds_meta  <- readr::read_csv(path_input_meta)

# augemnt the long file with meta data / attach meta data
ds_long <- ds_long %>%
  dplyr::full_join(ds_meta, by = "measure" ) %>%
  tibble::as_tibble() %>%  
  dplyr::filter(!items_m==0) %>% 
  dplyr::arrange(study)

# basic inspection
ds_long %>% head()
ds_long %>% glimpse()

# ---- tweak-data-3 ---------------------------------------------------------
# Each of the computed columns will provide an answer to a specific question:  
# `measures_d`   - how many measures does this domain have?   
# `studies_d`    - how many studies have at least one measure in this domain?  
# `studies_m`    - how many studies have this measure?  
# `measures_s`   - how many measures does this study have?    
# `measures_s_d` - how many measures does this study have in this domain?   
# `items_m`      - how many items does this measure contain?

# count measues, studies, domains and their relationships
d <- ds_long %>% 
  # how many studies have this measure?  
  dplyr::group_by(measure) %>% 
  dplyr::mutate(studies_m = length(!is.na(study))) %>% 
  dplyr::ungroup() %>% 
  # how many measures does this study have? 
  dplyr::group_by(study) %>%  
  dplyr::mutate(measures_s = length(!is.na(measure))) %>% 
  dplyr::ungroup() %>% 
  # how many measures does this study have in this domain?  
  dplyr::group_by(study, domain) %>%  
  dplyr::mutate(measures_s_d = length(!is.na(measure))) %>% 
  dplyr::ungroup() %>% 
  # how many measures does this domain have? 
  # how many studies have at least one measure in this domain?  
  dplyr::group_by(domain) %>% 
  dplyr::mutate(
    measures_d = length(!is.na(unique(measure)))
   ,studies_d  = length(!is.na(unique(study)))
  ) %>% 
  dplyr::ungroup() %>% 
  # make factors for easier handling
  dplyr::mutate(
     study   = factor(study)
    ,measure = factor(measure)
    ,domain  = factor(domain)
  ) %>%
  # order the variables for easier reference
  dplyr::select(
    domain,
    measures_d,studies_d,
    measure, studies_m,
    study, measures_s,
    measures_s_d,
    items_m
  )


# ---- basic-table ------------------------------------------
d %>%
  DT::datatable(
      class      = 'cell-border stripe',
      rownames   = FALSE,
      style      = "bootstrap",
      extensions = "ColReorder",
       options   = list(
         pageLength = 6
        ,autoWidth  = TRUE
        ,colReorder = TRUE
          ),
      filter     = "top"
    )

# ---- basic-table-2 ------------------------------------------
d %>%
    DT::datatable(
      style = "bootstrap",
      extensions = c('ColReorder',"KeyTable","Buttons")
      ,options = list(
        colReorder = list(realtime = FALSE),
        KeyTable   = list(keys = TRUE),
        dom = "Bfrtip",
        buttons = list(list(extend = 'colvis'),"csv")
        )
    )

# ---- basic-graph --------------------------------------------------------------

# Sonata form report structure
# ---- dev-a-0 ---------------------------------
d %>% 
  dplyr::distinct(domain, measures_d, studies_d) %>% 
  dplyr::arrange(domain)
# ---- dev-a-1 ---------------------------------
d %>% 
  dplyr::group_by(domain, measure) %>% 
  dplyr::select(measure,study) %>% 
  print()


# ---- dev-a-2 ---------------------------------
# ---- dev-a-3 ---------------------------------
# ---- dev-a-4 ---------------------------------
# ---- dev-a-5 ---------------------------------

# ---- dev-b-0 ---------------------------------
# ---- dev-b-1 ---------------------------------
# ---- dev-b-2 ---------------------------------
# ---- dev-b-3 ---------------------------------
# ---- dev-b-4 ---------------------------------
# ---- dev-b-5 ---------------------------------

# ---- recap-0 ---------------------------------
# ---- recap-1 ---------------------------------
# ---- recap-2 ---------------------------------
# ---- recap-3 ---------------------------------


# ---- publish ---------------------------------------
path_report_1 <- "./reports/coverage-cognitive/coverage-cognitive.Rmd"
path_report_2 <- "./reports/coverage-cognitive/coverage-cognitive-barebone.Rmd"

allReports <- c(path_report_1, path_report_2)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  
  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

