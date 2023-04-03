# library(QRAP)

#   ____________________________________________________________________________
#   Server                                                                  ####

shinyServer(function(session, input, output) {

  species <- reactive({
    ids <- readRDS("supported_species.rds")
    ids$display_name <- paste0(ids$scientific_name, " (", ids$display_name, ")")
    ids <- ids[, -2]
  })
  
  output$supported_species <- renderUI({
    selectInput("supported_species", "The species:",
                choices = species()$display_name, selected = "Homo sapiens (Human)", width = "100%")
  })
  
  
  upload_data <- eventReactive(input$upload,{
    inFile <- input$file
    data <- upload.data(path = inFile$datapath, header = input$header, row_names = input$row_names)
  })
  
  # collapse upload_local_data_card
  observeEvent(input$upload, {
    js$collapse("upload_box")
    
    data <- upload_data()
    
    output$loadingBox1 <- renderInfoBox({
      infoBox("Samples:", ncol(data), width = 12, color = "light-blue", icon = icon("vials"), fill = TRUE)
    })
    output$loadingBox2 <- renderInfoBox({
      infoBox("Genes:", nrow(data), width = 12, color = "olive", icon = icon("dna"), fill = TRUE)
    })
  })
  
  
  # show expression data matrix
  output$rawTable <- DT::renderDataTable({
    data <- upload_data()
  },
  rownames = T,
  options = list(pageLength = 10, autoWidth = F, scrollX=TRUE, scrollY="285px")
  )
  
  
  
#   waiter_hide() # hide the waiter
# #
# #   kegg_species <- reactive({
# #     readRDS("www/Species/kegg_species.rds")
# #   })
#
#   observe({ kegg_species() })

  # source("modules/1-server-get-start.R", local = T)
  # source("modules/2-server-condition.R", local = T)
  # source("modules/3-server-pca.R", local = T)
  # source("modules/4-server-hierarchical-cluster.R", local = T)
  # source("modules/5-server-sample-distance.R", local = T)
  # source("modules/6-server-sample-correlation.R", local = T)
  # source("modules/7-server-differential-analysis.R", local = T)
  # source("modules/8-server-degs-patterns.R", local = T)
  # source("modules/9-server-expression-visualization.R", local = T)
  # source("modules/10-server-wgcna-prepare-data.R", local = T)
  # source("modules/11-server-wgcna-detect-module.R", local = T)
  # source("modules/CopyOf11-server-wgcna-detect-module.R", local = T)
  # source("modules/12-server-wgcna-module-trait.R", local = T)
  # source("modules/12.1-server-wgcna-scatter.R", local = T)
  # source("modules/12.2-server-wgcna-expression.R", local = T)
  # source("modules/13-server-gProfiler.R", local = T)
  # source("modules/14-server-cluster_ORA.R", local = T)
  # source("modules/15-server-cluster_GSEA.R", local = T)
  # source("modules/16-server-kegg-pathview.R", local = T)
  # source("modules/17-server-ppi-network.R", local = T)
  # source("modules/18-server-genie3-network.R", local = T)
  # source("modules/19-server-summarize-gene.R", local = T)
  # source("modules/20-server-summarize-function.R", local = T)
##  ............................................................................
##  Neighborhood browser                                                    ####

##  ............................................................................
##  Map chart                                                               ####


})
