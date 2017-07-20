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
requireNamespace("readxl")

requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables
# requireNamespace("plyr", quietly=TRUE)
# requireNamespace("reshape2", quietly=TRUE) #For converting wide to long
# requireNamespace("mgcv, quietly=TRUE) #For the Generalized Additive Model that smooths the longitudinal graphs.

# ---- declare-globals ---------------------------------------------------------
# link to the source of the location mapping
# path_input <- "./data-unshared/raw/SearchVariables.csv"
path_input <- "./data-unshared/raw/coverage-cognitive.csv"
# path_input <- "./data-unshared/raw/coverage-memory.csv"
# path_input <- "./data-unshared/raw/coverage-memory-dce.csv"
# test whether the file exists / the link is good
testit::assert("File does not exist", base::file.exists(path_input))
# declare where you will store the product of this script
# path_save <- "./data-unshared/derived/memory"
path_save <- "./data-unshared/derived/dto-1"
# See definitions of commonly  used objects in:
source("./manipulation/object-glossary.R")   # object definitions
path_save_meta <- "./data-unshared/derived/coverage-cognitive-live.csv"
path_input_meta <- "./data-public/meta/coverage-cognitive-dead.csv"

# path_save_meta <- "./data-unshared/meta/memory-live.csv"
# path_input_meta <- "./data-public/meta/memory-dead.csv"

# ---- utility-functions ----------------------------------------------------- 
# functions, the use of which is localized to this script
convert_to_ascii <- function( x ) {
  iconv(x, "latin1", "ASCII//TRANSLIT")
}
# ---- load-data ---------------------------------------------------------------
ds <- readr::read_csv(path_input,skip = 2) %>% as.data.frame() 
ds <- ds %>% tibble::as_tibble()
# ds <- readr::read_csv(path_input) %>% as.data.frame() 

# ---- inspect-data -----------------------------------------------------------
ds %>% dplyr::glimpse()

# ---- tweak-data -------------------------------------------------------------
# identify the function of variables with respect to THIS wide-long tranformation
variables_static <- common_stem_dce
# variables_static <- common_stem
variables_dynamic <- setdiff(colnames(ds), variables_static)
# tranform
ds_long <- ds %>% 
  tidyr::gather_("measure_label","value", variables_dynamic) %>% 
  dplyr::mutate(
    measure_label = gsub("\\'s",'s',measure_label), # some apostrophes are apostrophes
    measure_label = convert_to_ascii(measure_label), # others are single quote
    measure_label = gsub("a\\?\\?",'',measure_label) # replace
  ) %>% 
  # dplyr::select(-Start, -End) %>% 
  dplyr::arrange(Study)

ds_long %>% head()
ds_long %>% glimpse()

# save unique measure names / extract meta data
ds_long %>%
  dplyr::distinct(measure_label) %>%
  dplyr::arrange(measure_label) %>% 
  readr::write_csv(path_save_meta)

## edit the meta data spreadsheed manually and save it in data-public/meta
ds_meta  <- readr::read_csv(path_input_meta)

# augemnt the long file with meta data / attach meta data
ds_long <- ds_long %>%
  dplyr::full_join(ds_meta, by = "measure_label" ) %>%
  tibble::as_tibble() %>%  
  dplyr::filter(!value==0) %>% 
  dplyr::arrange(Study)
# 
names(ds_long) <- tolower(names(ds_long))

# ---- basic-table ------------------------------------------
d <- ds_long %>% 
  dplyr::rename(
    measure = measure_label,
    items_m = value
    
  ) %>%
  dplyr::group_by(measure) %>% # n_studies THAT HAVE THIS MEASURE
  dplyr::mutate(studies_m = length(!is.na(study))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(study) %>%  # n_measures THAT THIS STUDY HAS
  dplyr::mutate(measures_s = length(!is.na(measure))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(study, domain) %>%  # n_measures THAT THIS STUDY HAS IN THIS DOMAIN
  dplyr::mutate(measures_s_d = length(!is.na(measure))) %>% 
  dplyr::ungroup() %>% 
  
  dplyr::group_by(domain) %>% 
  dplyr::mutate(
    measures_d = length(!is.na(unique(measure)))
     # unique_measure =  unique(measure) %>% !is.na() %>% length()
    ,studies_d  = length(!is.na(unique(study)))
    ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    # m = unique_measures,
    # s = unique_studies,
    # measures_d = 
    # studies_d =
      
  ) %>% 
  # dplyr::select(study,measure,domain,n_items, n_studies, n_measures, domain_size)
  # dplyr::select(study,domain,measure,n_items, n_studies, n_measures,n_measures_domain, domain_size)
  dplyr::select(
    # study,domain,measure,n_items, n_studies, n_measures,n_measures_domain, domain_size
    domain,
    # unique_measures,unique_studies,
    # m,s,
    measures_d,studies_d,
    # measure, n_studies,
    measure, studies_m,
    study, measures_s,
    measures_s_d,
    items_m
  )

d %>%
  dplyr::mutate(
    study = factor(study)
    ,measure = factor(measure)
    ,domain = factor(domain)
  ) %>% 
  # dplyr::select(-measure) %>% 
    DT::datatable(
      rownames = FALSE,
      class     = 'cell-border stripe',
      filter    = "top",
      extensions = 'Buttons', 
      options = list(
        # Buttons
         dom = 'Bfrtip' 
        ,buttons =list(list(extend = 'colvis'))
        # General
        # ,pageLength = 6
        # ,autoWidth = TRUE
        )
    )


# ---- basic-graph --------------------------------------------------------------
# Questions that the numbers in columns answer:
# items_m = how many items are were there recorded for this MEASURE have been 
# n_studies = how many studies have this measure?
# n_measures = how many cognitive measures does this study have? 
# n_measures_domain = how many cognitive measures does this study have in this domain?
# domain_size = how many distinct measures does this domain has

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

allReports <- c(path_report_1)

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

