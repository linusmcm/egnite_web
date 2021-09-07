# ---------------------------------------------------------- #
library(shiny)
library(gfonts)
library(janitor)
library(visNetwork)
library(fullPage)
library(shinyjqui)
library(shinyjs)
library(RSQLite)
library(readxl)
library(DBI)
#library(igraph)
library(tidyverse)
# ---------------------------------------------------------- #
# ---------------------------------------------------------- #
#db_conn <- dbConnect(RSQLite::SQLite(), "www/egnite_test.sqlite")

db_conn <- dbConnect(RSQLite::SQLite(), "www/egnite.sqlite")
edges <<- dbReadTable(db_conn, "edges")
NODES <<- data.frame(dbReadTable(db_conn, "nodes")) |> mutate(label = str_replace_all(label, " ", " \n "))
# ---------------------------------------------------------- #
# ---------------------------------------------------------- #
#options(shiny.autoreload.pattern = glob2rx("www/egnite_test.sqlite"))
options(shiny.autoreload = T)
options(shiny.autoreload.interval = 500)
options(shiny.trace = T)
#options(shiny.error=recover)
options(shiny.stacktraceoffset = T)
options(shiny.fullstacktrace = T)
#options(shiny.testmode =T)
# ---------------------------------------------------------- #
#setup_font(id = "jura", output_dir = "www/")
#use_font("jura", "www/css/jura.css")
# ---------------------------------------------------------- #
OPTIONS <- list(
    sectionsColor = c("#fff")
    , navigation = T
    , keyboardScrolling = T
    , fitToSection = T
    , paddingTop = '0em'
    , paddingBottom = '0em'
    #, responsiveHeight=490
    )
# ---------------------------------------------------------- #
STROKE_WIDTH <<- 1
# ---------------------------------------------------------- #
# ---------------------------------------------------------- #
# group_name <- unlist(NODES |>
#             filter(id == "data_engineering") |>
#             select(group)
#             , use.names = F)
#
# all_nodes <- unlist(NODES |>
#     filter(group == group_name) |>
#     distinct(id))


