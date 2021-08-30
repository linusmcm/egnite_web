# ---------------------------------------------------------- #
library(shiny)
library(gfonts)
library(visNetwork)
library(fullPage)
library(shinyjqui)
library(shinyjs)
library(RSQLite)
library(readxl)
library(DBI)
library(tidyverse)
# ---------------------------------------------------------- #
# ---------------------------------------------------------- #
db_conn <- dbConnect(RSQLite::SQLite(), "www/egnite.sqlite")
edges <<- dbReadTable(db_conn, "edges")
NODES <<- data.frame(dbReadTable(db_conn, "nodes")) |> mutate(label = str_replace_all(label, " ", " \n "))
# ---------------------------------------------------------- #
# ---------------------------------------------------------- #
#options(shiny.autoreload.pattern = glob2rx("app.R"))
options(shiny.autoreload = T)
options(shiny.autoreload.interval = 500)
options(shiny.trace = T)
#options(shiny.error=recover)
options(shiny.stacktraceoffset = T)
options(shiny.fullstacktrace = T)
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
# output$consulting <- renderVisNetwork(
#     {
#         nodes <- data.frame(id = c("egnite", "data_architecture","data_engineering","business_intellegence", "data_science", "business_consulting")
#                             , shadow = T
#                             #, size = 200
#                             , borderWidth = STROKE_WIDTH
#                             , borderWidthSelected = STROKE_WIDTH
#                             , labelHighlightBold = F
#                             , label = c("Egnite"
#                                         , paste0("Data ", "\n" , "Architecture")
#                                         , paste0("Data ", '\n' ,"Engineering")
#                                         , paste0("Business ", "\n", "Intellegence")
#                                         , paste0("Data ", "\n", "Science")
#                                         , paste0("Business ", "\n","Consulting"))
#                             , font = list(color = 'white'
#                                           , size = "12"
#                                           , face = "Jura"
#                                           , align = "center"
#                                           , strokeWidth=0.5)
#                             , color = list(background="grey"
#                                            , border="black"
#                                            , highlight = list(background="grey", border="white")
#                                            , hover = list(background="black", border="grey"))
#                             , mass = 5
#                             , value = 40
#                             , shape = "circle")
#         edges <- data.frame(from = c("egnite","egnite","egnite","egnite","egnite")
#                             , to = c("data_architecture","data_engineering","business_intellegence", "data_science", "business_consulting")
#                             , width = STROKE_WIDTH
#                             , shadow = T)
#
#         visNetwork(nodes, edges) %>%
#             visInteraction(dragNodes = T
#                            , hover = T
#                            , dragView = T
#                            , selectable = T
#                            , zoomView = T) %>%
#             visEvents(hoverNode = "function(nodes)
#                           {
#                             Shiny.onInputChange('current_node_id', nodes);
#                             ;}") |>
#             visLayout(randomSeed = 123) |>
#             visNodes(scaling = list(min = 1,
#                                     max = 100,
#                                     label = list(enabled = TRUE,
#                                                  min = 1,
#                                                  max = 80,
#                                                  maxVisible = 24,
#                                                  drawThreshold = 1
#                                     )))
#     })


