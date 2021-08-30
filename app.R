source("global.R")

ui <- fullPage(
     useShinyjs()
     , inlineCSS("#well_panel.well { background-color: #f5f5f500!important; box-shadow: 2px 2px 2px rgba(0,0,0,.5)!important;}")
     , inlineCSS("h1 { text-shadow: 2px 2px 2px rgba(0,0,0,.5)!important; color: #fff!important; }")
     , inlineCSS("h3 { text-shadow: 2px 2px 2px rgba(0,0,0,.5)!important; }")
    , tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "egnite_web.css"))
    , tags$link(rel = "stylesheet", type = "text/css", href = "css/jura.css")
    , tags$style("body {font-family: 'Jura', sans-serif;}")
    , center = TRUE
    , opts = OPTIONS
    , menu = c("egnite" = "egnite","partners" = "partners","contact" = "contact")
    , fullSection(menu = "egnite"
                  , absolutePanel(tags$a(href='.http://egnite.com.au',target="_blank", tags$img(src='logo_blue_flame_web.png')), bottom = '5px', right = '5px')
                 # , absolutePanel(tags$a(href='.http://egnite.com.au',target="_blank", tags$img(src='shiny_logo.png', width='30%')), bottom = '5px', right = '5px')
                  , fullColumn(
                      tagList(
                          visNetworkOutput("intro", width = "100%", height = "900px")
                            , shinyjs::hidden(div(id = "element"
                                                   , absolutePanel(
                                                       wellPanel(id = "well_panel"
                                                                 , h1(textOutput("header_text"))
                                                                 , h3(textOutput("body_text")))
                                                       , width = '30%'
                                                       , top = 60
                                                       , right = 60)))) , width = 12))

    , fullSection(menu = "partners"
                  , absolutePanel(tags$a(href='.http://egnite.com.au',target="_blank", tags$img(src='logo_blue_flame_web.png')), bottom = '5px', right = '5px')
                  , h1("egnite partners")
                 , fullColumn(tags$a(href='https://www.cloudera.com/', target="_blank", tags$img(src='cloudera_logo.png', width='90%')), width=6)
                 , fullColumn(tagList(tags$a(href='https://www.rstudio.com/', target="_blank", tags$img(src='r_studio_logo.png', width='90%')), h1("RStudio partnership - pending")), width=6)
                  )
    , fullSection(menu = "contact"
                  , absolutePanel(tags$a(href='.http://egnite.com.au',target="_blank", tags$img(src='logo_blue_flame_web.png')), bottom = '5px', right = '5px')
                  , a(h1("contact egnite"), href="mailto:info@egnite.com.au"))
)

server <- function(input, output)
{
    current_node_id <- reactiveVal("egnite")
    #group_id <- reactiveVal()
    all_node <- reactiveVal()

    #observe(current_node_id(req(input$click$nodes)))


    observe(all_node(req(input$click)))
    #observeEvent(all_node(), { print(req(all_node())) })


    observeEvent(input$click$nodes,
        {
            print(input$click$nodes)
            print(length(input$click$nodes))
            if(length(req(input$click$nodes))> 0 )
            {
                shinyjs::show("element")
                current_node_id(req(input$click$nodes))
            }
            else
            {
                shinyjs::hide("element")
                current_node_id("egnite")
            }
            visNetworkProxy("intro") %>%
                visFocus(
                    id = req(current_node_id()),
                    scale = 1,
                    offset = list(x = -450, y = -100),
                    locked = TRUE,
                    animation = list(duration = 500, easingFunction = "easeInOutQuad"))
        }, ignoreNULL = T, ignoreInit = F)

    output$header_text <- renderText(unlist(NODES |> filter(id == req(current_node_id())) |> select(label), use.names = F))
    output$body_text <- renderText(unlist(NODES |> filter(id == req(current_node_id())) |> select(body_text), use.names = F))
    #---------------------------------------------------------------------------------- #
    #---------------------------------------------------------------------------------- #
    output$intro <- renderVisNetwork(
    {
        visNetwork(NODES, edges, width = "100%", height = "100%") |>
            visLayout(randomSeed = 123, improvedLayout = T) |>
            visOptions(highlightNearest = F, nodesIdSelection = list(enabled = F, selected = "egnite") ) |>
            visEvents(select = "function(nodes){ Shiny.onInputChange('click', nodes); }") |>
            visEvents(type = "once", startStabilizing = "function() { this.moveTo({scale:2.1})}") |>
            visInteraction(hover = T, hoverConnectedEdges = T, selectable = T, selectConnectedEdges = F ) |>
            visEdges(smooth = list(enabled = F), hoverWidth = 1, width = 1, selectionWidth = 1.2, shadow = list(enabled = TRUE, size = 5), scaling = list(min = 1, max = 1)) |>
            visNodes(shape = "circle"
                           # , y = 10
                           # , x = -200
                            , mass = 5
                            , labelHighlightBold = F
                            , shadow = list(enabled = T, size = 5)
                            , font = list(color = 'white'
                                  , size = "12"
                                  , face = "Jura"
                                  , align = "center"
                                  , strokeWidth=0)
                        , scaling = list(min = 10
                                         , max = 300
                                         , label = list(enabled = TRUE, min = 14, max = 80, maxVisible = 24, drawThreshold = 14))) |>
            visPhysics(solver = "barnesHut", barnesHut= list(avoidOverlap = 0.5, damping = 1)) |>
            visGroups(groupname = "egnite"
                          , shape = "circle"
                          , labelHighlightBold = F
                          , font = list(face = "Jura"
                                        , size = 12
                                        , strokeWidth = 0
                                        , strokeColor = "#fff")
                          , color = list(background = 'rgba(68,101,139,1)'
                                         , border = 'rgba(77,141,201,1)'
                                         , highlight = list(background = 'rgba(68,101,139,0.75)', border = 'rgba(77,141,201,1)')
                                         , hover  = list(background = 'rgba(68,101,139,0.5)', border = 'rgb(255,255,255)') )) |>
            visGroups(groupname = "data_engineering"
                          , shape = "circle"
                          , font = list(face = "Jura"
                                        , size = 12
                                        , strokeWidth = 0
                                        , strokeColor = "#fff")
                          , color = list(background = 'rgba(68,101,139,1)'
                                         , border = 'rgba(77,141,201,1)'
                                         , highlight = list(background = 'rgba(68,101,139,0.75)', border = 'rgba(77,141,201,1)')
                                         , hover  = list(background = 'rgba(68,101,139,0.5)', border = 'rgb(255,255,255)'))) |>
            visGroups(groupname = "data_science"
                      , shape = "circle"
                      , font = list(face = "Jura"
                                    , size = 12
                                    , strokeWidth = 0
                                    , strokeColor = "#fff")
                      , color = list(background = 'rgba(68,101,139,1)'
                                     , border = 'rgba(77,141,201,1)'
                                     , highlight = list(background = 'rgba(68,101,139,0.75)', border = 'rgba(77,141,201,1)')
                                     , hover  = list(background = 'rgba(68,101,139,0.5)', border = 'rgb(255,255,255)')))
        })

}
shinyApp(ui, server)
