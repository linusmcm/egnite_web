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
                 , fullColumn(tagList(visNetworkOutput("intro", width = "100%", height = "900px"), shinyjs::hidden(div(id = "element", uiOutput("dynamic_display")))) , width = 12))
    , fullSection(menu = "partners"
                 , absolutePanel(tags$a(href='.http://egnite.com.au',target="_blank", tags$img(src='logo_blue_sticker.png', width='50%')), bottom = '5px', right = '5px')
                 , h1("our partners")
                 , fullColumn(tags$a(href='https://www.cloudera.com/', target="_blank", tags$img(src='cloudera_logo.png', width='90%')), width=6)
                 , fullColumn(tagList(tags$a(href='https://www.rstudio.com/', target="_blank", tags$img(src='r_studio_logo.png', width='90%')), h1("RStudio partnership - pending")), width=6))
    , fullSection(menu = "contact"
                , absolutePanel(tags$a(href='.http://egnite.com.au',target="_blank", tags$img(src='logo_blue_sticker.png', width='50%')), bottom = '5px', right = '5px')
               # , a(h1("contact egnite"), href="mailto:info@egnite.com.au"))
                , fullColumn(tags$a(href='mailto:info@egnite.com.au', target="_blank", tags$img(src='contact_egnite.png', width='50%')), width=12))

    )

server <- function(input, output)
{
    current_node_id <- reactiveVal("egnite")
    node_group <- reactiveVal("egnite")
    nodes_in_group <- reactiveVal()
    dynamic_x <- reactiveVal(60)
    dynamic_y <- reactiveVal(60)

    # test_node <- reactiveVal()
    # all_node <- reactiveVal()
    # observe(test_node(req(input$click$nodes)))
    # #{"custom":{"visShinyFocus":{"id":"intro","focusId":["egnite"],"options":{"scale":1,"offset":{"x":-450,"y":-100},"locked":true,"animation":{"duration":500,"easingFunction":"easeInOutQuad"}}}}}
    # observe(all_node(list(req(input$click$pointer$DOM$x), req(input$click$pointer$DOM$y) )))
    # observeEvent(all_node(), { print(req(all_node())) })


    observeEvent(input$click$nodes,
        {
            print(input$click$nodes)
            print(length(input$click$nodes))
            if(length(req(input$click$nodes)) > 0 && req(input$click$nodes) != "egnite")
            {
                shinyjs::show("element")
                current_node_id(req(input$click$nodes))
                node_group(unlist(NODES |>
                                      filter(id == req(current_node_id())) |>
                                      select(group)
                                  , use.names = F))
                nodes_in_group(unlist(NODES |>
                                          filter(group == req(node_group())) |>
                                          distinct(id)
                               , use.names = F))
                dynamic_x(unlist(NODES |>
                                     filter(group == req(node_group())) |>
                                     summarise(canvas_x = max(as.integer(canvas_x), na.rm = T)) |>
                                     select(canvas_x)
                                 , use.names = F))

                dynamic_y(unlist(NODES |>
                                     filter(group == req(node_group())) |>
                                     summarise(canvas_y = max(as.integer(canvas_y), na.rm = T)) |>
                                     select(canvas_y)
                                 , use.names = F))

            }
            else
            {
                shinyjs::hide("element")
                current_node_id("egnite")
                nodes_in_group(unlist(NODES |> distinct(id), use.names = F))
            }
            visNetworkProxy("intro") %>%
                visFit(nodes = nodes_in_group()
                       , animation = list(duration = 500, easingFunction = "easeInOutQuad")
                       )
                # visFocus(
                #     scale = 1.5
                #     # , offset = list(x = 0, y = 0)
                #     # , locked = TRUE
                #     # , animation = list(duration = 1500, easingFunction = "easeInOutQuad")
                #     )

        }, ignoreNULL = T, ignoreInit = T)
    output$header_text <- renderText(unlist(NODES |> filter(id == req(current_node_id())) |> select(label), use.names = F))
    output$body_text <- renderText(unlist(NODES |> filter(id == req(current_node_id())) |> select(body_text), use.names = F))
    output$dynamic_display <- renderUI({
        absolutePanel(
            wellPanel(id = "well_panel"
                      , h1(textOutput("header_text"))
                      , h3(textOutput("body_text")))
            , width = '30%'
            , top = dynamic_x()
            , right = dynamic_y())
    })
    #---------------------------------------------------------------------------------- #
    #---------------------------------------------------------------------------------- #
    output$intro <- renderVisNetwork(
    {
        visNetwork(NODES, edges, width = "100%", height = "100%") |>
            visLayout(randomSeed = 123, improvedLayout = T) |>
            visOptions(highlightNearest = F, nodesIdSelection = list(enabled = F, selected = "egnite") ) |>
             visEvents(select = "function(nodes){ Shiny.onInputChange('click', nodes); }") |>
             visInteraction(hover = T, hoverConnectedEdges = T, selectable = T, selectConnectedEdges = F ) |>
             visEdges(physics = T
                      , smooth = list(enabled = F)
                      , hoverWidth = 1
                      , width = 1
                      , selectionWidth = 1.2
                      , shadow = list(enabled = TRUE, size = 5)
                      , scaling = list(min = 1, max = 1)) |>
            visNodes(mass = 2
                     , physics = T
                            , labelHighlightBold = F
                            , shadow = list(enabled = T, size = 5)
                            , font = list(color = 'white'
                                  , size = 12
                                  , face = "Jura"
                                  , align = "center"
                                  , strokeWidth=0)
                        , scaling = list(min = 4
                                         , max = 80
                                         , label = list(enabled = TRUE, min = 14, max = 80, maxVisible = 24, drawThreshold = 14))) |>
            # visPhysics(solver = "barnesHut"
            #            , maxVelocity = 1
            #            , stabilization =list(onlyDynamicEdges = T)
            #            , adaptiveTimestep = T
            #            , barnesHut= list(gravitationalConstant = -2000
            #                                     , centralGravity = 0.3
            #                                     , springLength = 95
            #                                     , springConstant = 0.04
            #                                     , damping = 1
            #                                     , avoidOverlap = 1)) |>
            # visPhysics(solver = "forceAtlas2Based"
            #            , maxVelocity = 1
            #            , stabilization =list(onlyDynamicEdges = T)
            #            , adaptiveTimestep = T
            #            , forceAtlas2Based= list(gravitationalConstant = -50
            #                                     , centralGravity = 0.05
            #                                     , springLength = 100
            #                                     , springConstant = .05
            #                                     , damping = 1
            #                                     , avoidOverlap = 1)) |>
           # visHierarchicalLayout() |>
             visPhysics(solver = "repulsion"
                        , maxVelocity = 50
                       #, stabilization =list(onlyDynamicEdges = T)
                      #  , timestep = 0.1
                        , adaptiveTimestep = T
                        , repulsion= list(nodeDistance = 100
                                          , damping = 1
                                          , springLength = 200
                                          , springConstant= 0.1
                                          , centralGravity = -1)) |>
            visGroups(groupname = "egnite"
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
                          , font = list(face = "Jura"
                                        , size = 14
                                        , strokeWidth = 0
                                        , strokeColor = "#fff")
                          , color = list(background = 'rgba(68,101,139,1)'
                                         , border = 'rgba(77,141,201,1)'
                                         , highlight = list(background = 'rgba(68,101,139,0.75)', border = 'rgba(77,141,201,1)')
                                         , hover  = list(background = 'rgba(68,101,139,0.5)', border = 'rgb(255,255,255)'))) |>
            visGroups(groupname = "data_architecture"
                      , font = list(face = "Jura"
                                    , size = 14
                                    , strokeWidth = 0
                                    , strokeColor = "#fff")
                      , color = list(background = 'rgba(68,101,139,1)'
                                     , border = 'rgba(77,141,201,1)'
                                     , highlight = list(background = 'rgba(68,101,139,0.75)', border = 'rgba(77,141,201,1)')
                                     , hover  = list(background = 'rgba(68,101,139,0.5)', border = 'rgb(255,255,255)'))) |>
            visGroups(groupname = "business_intelligence"
                         , font = list(face = "Jura"
                                    , size = 14
                                    , strokeWidth = 0
                                    , strokeColor = "#fff")
                      , color = list(background = 'rgba(68,101,139,1)'
                                     , border = 'rgba(77,141,201,1)'
                                     , highlight = list(background = 'rgba(68,101,139,0.75)', border = 'rgba(77,141,201,1)')
                                     , hover  = list(background = 'rgba(68,101,139,0.5)', border = 'rgb(255,255,255)'))) |>
            visGroups(groupname = "business_consulting"
                      , font = list(face = "Jura"
                                    , size = 14
                                    , strokeWidth = 0
                                    , strokeColor = "#fff")
                      , color = list(background = 'rgba(68,101,139,1)'
                                     , border = 'rgba(77,141,201,1)'
                                     , highlight = list(background = 'rgba(68,101,139,0.75)', border = 'rgba(77,141,201,1)')
                                     , hover  = list(background = 'rgba(68,101,139,0.5)', border = 'rgb(255,255,255)'))) |>
            visGroups(groupname = "data_science"
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
