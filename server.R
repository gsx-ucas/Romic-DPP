library(DT)
library(plyr)
library(dplyr)
library(gprofiler2)

#   ____________________________________________________________________________
#   Server                                                                  ####

source("global.R", local = T)

# set upload file size limit as 100MB
options(shiny.maxRequestSize = 1000 * 1024^2, warn = -1, shiny.sanitize.errors = TRUE)

shinyServer(function(session, input, output) {

  #-----------------------prepare data--------------------#
  
  # load expression matrix
  upload_data <- eventReactive(input$upload,{
    inFile <- input$file
    withProgress(min = 0, max = 1, {
      incProgress(0.5, message = "uploading ...")
      data <- try(upload.data(path = inFile$datapath, header = input$header, row_names = input$row_names))
      ## sum the expression value of duplicated genes if row.names was duplicated
      if ("try-error" %in% class(data)) {
        data <- upload.data(path = inFile$datapath, header = input$header, row_names = F)
        colnames(data)[1] <- "genes"
        data <- data %>% dplyr::group_by(genes) %>% dplyr::summarise_if(is.numeric, sum)
      }
    })
    return(data)
  })
  
  # show expression data matrix
  output$rawTable <- DT::renderDataTable({
    data <- upload_data()
  },
  rownames = T,
  options = list(pageLength = 15, autoWidth = F, scrollX=TRUE, scrollY="415px")
  )
  
  # collapse upload_box and show sample/gene info
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
  
  # loadd species info
  species <- reactive({
    ids <- readRDS("gprofiler_supported_species.rds")
    ids$display_name <- paste0(ids$scientific_name, " (", ids$display_name, ")")
    ids
  })
  
  #-----------------------Gene ID conversion UI--------------------#
  
  output$source_species <- renderUI({
    if (input$convert_type=='ID conversion' & input$convert_db=='NCBI (local)') {
      selectizeInput("source_species", "The species:",
                  choices = dir("ncbi_db/genedb/") %>% stringr::str_remove(".rds"), selected = "Homo sapiens (taxid=9606)", width = "100%")
    }else if (input$convert_type=='ID conversion' & input$convert_db=='ENSEMBL (online)') {
      selectizeInput("source_species", "The species:",
                  choices = species()$display_name, selected = "Homo sapiens (Human)", width = "100%")
    }else if (input$convert_type=='Orthology search' & input$convert_db=='NCBI (local)') {
      selectizeInput("source_species", "The species:",
                  choices = readRDS("ncbi_db/orthologsdb_info.rds")$display_name, selected = "Homo sapiens (taxid=9606)", width = "100%")
    }else {
      selectizeInput("ens_source_species", "The species:",
                  choices = species()[!is.na(species()$orth_targets), "display_name"], selected = "Homo sapiens (Human)", width = "100%")
    }
  })
  
  # treat numeric id as
  output$convert_inputs <- renderUI({
    if (input$convert_type=='ID conversion' & input$convert_db=='ENSEMBL (online)') {
      selectizeInput("numeric_as", "Treat numeric ID as:",
                  choices = species()[species()$display_name == input$ens_source_species, "numeric"] %>% stringr::str_split(",") %>% unlist, width = "100%")
    }else if (input$convert_type=='ID conversion' & input$convert_db=='NCBI (local)') {
      selectizeInput("ncbi_inputs", "The input gene id type:",
                  choices = c("ENS_GENE", "ENTREZ_GENE", "ENTREZ_ACC"), width = "100%")
    }
  })
  
  # gene transfer targets
  output$convert_targets <- renderUI({
    if (input$convert_type=='ID conversion' & input$convert_db=='ENSEMBL (online)') {
      selectizeInput("convert_targets", "Targets to trasfer:",
                  choices = species()[species()$display_name == input$ens_source_species, "all"] %>% stringr::str_split(",") %>% unlist, width = "100%")
    }else if (input$convert_type=='ID conversion' & input$convert_db=='NCBI (local)') {
      selectizeInput("ncbi_targets", "Targets to trasfer:",
                  choices = c("ENS_GENE", "ENTREZ_GENE", "ENTREZ_ACC")[c("ENS_GENE", "ENTREZ_GENE", "ENTREZ_ACC") != input$ncbi_inputs], width = "100%")
    }
  })
  
  #-----------------------Gene Orthology search UI--------------------#
  # gene transfer targets
  output$target_species <- renderUI({
    if (input$convert_type=='Orthology search' & input$convert_db=='NCBI (local)') {
      req(input$source_species)
      orthologsdb_info <- readRDS("ncbi_db/orthologsdb_info.rds")
      source_tax_id <- orthologsdb_info[orthologsdb_info$display_name == input$source_species, "tax_id"]
      target_tax_id <- readRDS(paste0("ncbi_db/orthologs/", source_tax_id, ".rds"))$Other_tax_id %>% unique()
      target_orgnisam <- orthologsdb_info[orthologsdb_info$tax_id == target_tax_id, "display_name"]
      selectizeInput("target_species", "Target species:", choices = target_orgnisam, width = "100%")
    }else if (input$convert_type=='Orthology search' & input$convert_db=='ENSEMBL (online)') {
      req(input$ens_source_species, species())
      orth_targets <- species()[species()$display_name == input$ens_source_species, "orth_targets"]  %>% stringr::str_split(",") %>% unlist
      orth_targets <- dplyr::inner_join(data.frame(row.names = orth_targets, organism = orth_targets), species(), by = "organism")$display_name
      selectizeInput("target_species", "Target species:", choices = orth_targets, width = "100%")
    }
  })
  
  # treat numeric id as
  output$orth_inputs <- renderUI({
    if (input$convert_type=='Orthology search' & input$convert_db=='ENSEMBL (online)') {
      selectizeInput("orth_numeric_as", "Treat numeric ID as:",
                  choices = species()[species()$display_name == input$ens_source_species, "numeric"] %>% stringr::str_split(",") %>% unlist, width = "100%")
    }else if (input$convert_type=='Orthology search' & input$convert_db=='NCBI (local)') {
      orthologsdb_info <- readRDS("ncbi_db/orthologsdb_info.rds")
      selectizeInput("orth_inputs", "Orthology search inputs:",
                  choices = orthologsdb_info[orthologsdb_info$display_name == input$source_species, "geneID"] %>% stringr::str_split(",") %>% unlist, width = "100%")
    }
  })
  
  # Orthology targets
  output$orth_targets <- renderUI({
    if (input$convert_type=='Orthology search' & input$convert_db=='ENSEMBL (online)') {
      selectizeInput("orth_targets", "Orthology search targets:", choices = "", width = "100%", 
                  options = list( placeholder = 'The default is the format you inputed.', onInitialize = I('function() { this.setValue(""); }')))
    }else if (input$convert_type=='Orthology search' & input$convert_db=='NCBI (local)') {
      orthologsdb_info <- readRDS("ncbi_db/orthologsdb_info.rds")
      ID <- orthologsdb_info[orthologsdb_info$display_name == input$source_species, "geneID"] %>% stringr::str_split(",") %>% unlist
      selectizeInput("orth_targets", "Orthology search targets:",
                  choices = ID[ID != input$orth_inputs], width = "100%")
    }
  })
  
  #-----------------------Conversion function--------------------#
  gconvertdata <- eventReactive(input$gconvert, {
    withProgress(min = 0, max = 1, {
      js$collapse("preview_box")
      
      data <- upload_data()
      data$input <- rownames(data)
      
      if (input$convert_type=='ID conversion' & input$convert_db=='ENSEMBL (online)') {
        incProgress(0.5, message = "converting ...")
        convertData <- gprofiler2::gconvert(query = rownames(data), organism = species()[species()$display_name == input$ens_source_species, "organism"], 
                                            target = input$convert_targets, numeric_ns = input$numeric_as, filter_na = F)
        convertData[is.na(convertData$target), "target"] <- convertData[is.na(convertData$target), "input"]
        convertData <- convertData[!duplicated(convertData$input), ] %>% subset(select = c(input, target))
      }else if (input$convert_type=='ID conversion' & input$convert_db=='NCBI (local)') {
        incProgress(0.5, message = "converting ...")
        convertData <- readRDS(paste0("ncbi_db/genedb/", input$source_species, ".rds"))[, c(input$ncbi_inputs, input$ncbi_targets)] %>% na.omit()
        colnames(convertData) <- c("input", "target")
      }else if (input$convert_type=='Orthology search' & input$convert_db=='ENSEMBL (online)') {
        incProgress(0.5, message = "converting ...")
        convertData <- gprofiler2::gorth(query = rownames(data), numeric_ns = input$orth_numeric_as, filter_na = F, 
                                         source_organism = species()[species()$display_name == input$ens_source_species, "organism"], 
                                         target_organism = species()[species()$display_name == input$target_species, "organism"])
        convertData <- convertData[!is.na(convertData$ortholog_name), ]
        convertData <- convertData[!duplicated(convertData$input), ] %>% subset(select = c(input, ortholog_name))
        colnames(convertData) <- c("input", "target")
      }else if (input$convert_type=='Orthology search' & input$convert_db=='NCBI (local)') {
        if (input$orth_inputs != "ENTREZ_ACC") {
          source_gene_df <- readRDS(paste0("ncbi_db/genedb/", input$source_species, ".rds"))[, c("ENTREZ_ACC", input$orth_inputs)] %>% na.omit()
          source_gene_df <- source_gene_df[!duplicated.data.frame(source_gene_df), ]
          
          orthologsdb_info <- readRDS("ncbi_db/orthologsdb_info.rds")
          source_tax_id <- orthologsdb_info[orthologsdb_info$display_name == input$source_species, "tax_id"]
          target_tax_id <- orthologsdb_info[orthologsdb_info$display_name == input$target_species, "tax_id"]
          orthologs_df <- readRDS(paste0("ncbi_db/orthologs/", source_tax_id, ".rds"))
          orthologs_df <- orthologs_df[orthologs_df$Other_tax_id == target_tax_id, ]
          colnames(orthologs_df)[c(2,4)] <- c("ENTREZ_ACC", "Other_ENTREZ_ACC")
          
          source_orthologs_df <- dplyr::inner_join(source_gene_df, orthologs_df, by = "ENTREZ_ACC")
          
          target_gene_df <- readRDS(paste0("ncbi_db/genedb/", input$target_species, ".rds"))[, c("ENTREZ_ACC", input$orth_targets)] %>% na.omit()
          target_gene_df <- target_gene_df[!duplicated.data.frame(target_gene_df), ]
          colnames(target_gene_df) <- paste0("Other_", colnames(target_gene_df))
          
          source_target_orthologs_df <- dplyr::inner_join(source_orthologs_df, target_gene_df, by = "Other_ENTREZ_ACC")
          convertData <- source_target_orthologs_df[, c(input$orth_inputs, paste0("Other_", input$orth_targets))]
          colnames(convertData) <- c("input", "target")
        }else {
          orthologsdb_info <- readRDS("ncbi_db/orthologsdb_info.rds")
          source_tax_id <- orthologsdb_info[orthologsdb_info$display_name == input$source_species, "tax_id"]
          target_tax_id <- orthologsdb_info[orthologsdb_info$display_name == input$target_species, "tax_id"]
          orthologs_df <- readRDS(paste0("ncbi_db/orthologs/", source_tax_id, ".rds"))
          orthologs_df <- orthologs_df[orthologs_df$Other_tax_id == target_tax_id, ]
          colnames(orthologs_df)[c(2,4)] <- c("ENTREZ_ACC", "Other_ENTREZ_ACC")
          
          target_gene_df <- readRDS(paste0("ncbi_db/genedb/", input$target_species, ".rds"))[, c("ENTREZ_ACC", input$orth_targets)] %>% na.omit()
          target_gene_df <- target_gene_df[!duplicated.data.frame(target_gene_df), ]
          colnames(target_gene_df) <- paste0("Other_", colnames(target_gene_df))
          
          source_target_orthologs_df <- dplyr::inner_join(orthologs_df, target_gene_df, by = "Other_ENTREZ_ACC")
          convertData <- source_target_orthologs_df[, c(input$orth_inputs, paste0("Other_", input$orth_targets))]
          colnames(convertData) <- c("input", "target")
        }
      }
      
      incProgress(0.5, message = "merging converted data ...")
      joinData <- dplyr::inner_join(convertData, data, by = "input", keep = F) %>% subset(select = -input)
      if (joinData$target %>% duplicated %>% any) {
        if (input$methods_dup == "Mean") {
          joinData <- joinData %>% dplyr::group_by(target) %>% dplyr::summarise_if(is.numeric, mean) %>% as.data.frame()
        }else if (input$methods_dup == "Sum") {
          joinData <- joinData %>% dplyr::group_by(target) %>% dplyr::summarise_if(is.numeric, sum) %>% as.data.frame()
        }else {
          joinData <- joinData[!duplicated(joinData$target), ]
        }
      }
      rownames(joinData) <- joinData$target
      joinData <- joinData %>% subset(select = -target)
      
      output$convertedBox <- renderInfoBox({
        infoBox("Converted:", dim(joinData)[1], width = 12, color = "yellow", icon = icon("dna"), fill = TRUE)
      })
      
    })
    return(joinData)
  })
  
  output$convertDF <- DT::renderDataTable({
    gconvertdata()
  },
  rownames = T,
  options = list(pageLength = 15, autoWidth = F, scrollX=TRUE, scrollY="350px")
  )
  
  output$download_convert <- downloadHandler(
    filename = function()  {paste0("ID-conversion-results-", as.character(Sys.time()), ".csv")},
    content = function(file) {
      write.csv(gconvertdata(), file, row.names = T)
    }
  )
  
  ##---------------------------------Matrix mergence---------------------------------page
  
  # show how many samples
  output$merge_sampleBox <- renderInfoBox({
    req(input$merge_file)
    infoBox("Samples:", dim(input$merge_file)[1], width = 12, color = "light-blue", icon = icon("vials"), fill = TRUE)
  })
  
  # get the info of colnames whe merge individual files
  merge_fcol <- reactive({
    req(input$merge_file)
    if (input$merge_type == 'Individual files') {
      column_name_length <- lapply(input$merge_file$datapath, function(x){
        # read the second line to identify delimiters
        Lines_2 <- readLines(x, n = 2)[2]
        # # identify delimiters
        if (grepl(x = Lines_2, pattern = "\t") == T) {
          columns <- readLines(x, n = 1) %>% strsplit("\t") %>% unlist %>% length()
        } else if (grepl(x = Lines_2, pattern = ",") == T) {
          columns <- readLines(x, n = 1) %>% strsplit(",") %>% unlist %>% length()
        } else if (grepl(x = Lines_2, pattern = " ") == T) {
          columns <- readLines(x, n = 1) %>% strsplit(" ") %>% unlist %>% length()
        } else if (grepl(x = Lines_2, pattern = ";") == T) {
          columns <- readLines(x, n = 1) %>% strsplit("\\;") %>% unlist %>% length()
        } else if (grepl(x = Lines_2, pattern = "|") == T) {
          columns <- readLines(x, n = 1) %>% strsplit("\\|") %>% unlist %>% length()
        } else if (grepl(x = Lines_2, pattern = ":") == T) {
          columns <- readLines(x, n = 1) %>% strsplit("\\:") %>% unlist %>% length()
        }
      }) %>% unlist
      
      
      if (length(unique(column_name_length)) == 1) {
        if (isTruthy(input$merge_header)) {
          column_name <- lapply(input$merge_file$datapath, function(x){
            # read the second line to identify delimiters
            Lines_2 <- readLines(x, n = 2)[2]
            # # identify delimiters
            if (grepl(x = Lines_2, pattern = "\t") == T) {
              columns <- readLines(x, n = 1) %>% strsplit("\t") %>% unlist
            } else if (grepl(x = Lines_2, pattern = ",") == T) {
              columns <- readLines(x, n = 1) %>% strsplit(",") %>% unlist
            } else if (grepl(x = Lines_2, pattern = " ") == T) {
              columns <- readLines(x, n = 1) %>% strsplit(" ") %>% unlist
            } else if (grepl(x = Lines_2, pattern = ";") == T) {
              columns <- readLines(x, n = 1) %>% strsplit("\\;") %>% unlist
            } else if (grepl(x = Lines_2, pattern = "|") == T) {
              columns <- readLines(x, n = 1) %>% strsplit("\\|") %>% unlist
            } else if (grepl(x = Lines_2, pattern = ":") == T) {
              columns <- readLines(x, n = 1) %>% strsplit("\\:") %>% unlist
            }
          }) %>% unlist %>% unique()
        }else {
          column_name <- upload.data(path = input$merge_file$datapath[1], header = input$merge_header, row_names = F) %>% colnames()
        }
        shinyjs::enable("merge_start")
        return(column_name)
      }else {
        sendSweetAlert(title = "Error", text = "The files you upload have different number of columns, please provide files with the same format.", type = "error")
        shinyjs::disable("merge_start")
      }
    }
  })
  
  output$merge_preview <- DT::renderDataTable({
    req(input$merge_file)
    # if (input$merge_type == 'Individual files') {
      data <- upload.data(path = input$merge_file$datapath[1], header = input$merge_header, row_names = F)[1:2, ]
    # }
  },
  rownames = T, options = list(dom = 't', autoWidth = F, scrollX=TRUE, scrollY = "80px"))
  
  output$merge_preview_table <- renderUI({
    req(input$merge_file)
    div(
      style = "padding-bottom: 10px", 
      strong("This is what your files look like:"),
      wellPanel(
        style = "margin-bottom:0; border: 1px solid #d4d4d4; background-color: #deefff;",
        DT::dataTableOutput("merge_preview")
      )
    )
  })
  
  observe({
    if (input$merge_type == 'Expression matrix' & (!isTruthy(input$merge_header) | !isTruthy(input$merge_row_names))) {
      sendSweetAlert(title = "Warning", text = "The first column of the file must be the gene name and the sample name must be included in the column name, 
                     otherwise the information of the expression matrix will not be interpretable.", type = "warning")
      shinyjs::disable("merge_start")
    }else {
      shinyjs::enable("merge_start")
    }
  })
  
  
  output$gene_column <- renderUI({
    req(input$merge_file$datapath)
    if (!is.null(merge_fcol())) {
      selectizeInput("gene_col", "Select a column as gene ID:", choices = merge_fcol(), width = "100%")
    }
  })
  
  output$expr_column <- renderUI({
    req(input$merge_file$datapath)
    if (!is.null(merge_fcol())) {
      selectizeInput("expr_col", "Select a column as expression value:", choices = merge_fcol(), width = "100%")
    }
  })
  
  data_merge <- eventReactive(input$merge_start, {
    withProgress(min = 0, max = 1, {
      if (input$merge_type == 'Individual files') {
        incProgress(0.5, message = "reading data ...")
        data <- lapply(input$merge_file$datapath, function(x){
          if (isTruthy(input$merge_skip1)) {
            upload.data(path = x, header = input$merge_header, row_names = F, skip = 1) %>% subset(select = c(input$gene_col, input$expr_col))
          }else {
            upload.data(path = x, header = input$merge_header, row_names = F, skip = 0) %>% subset(select = c(input$gene_col, input$expr_col))
          }
        }) 
        incProgress(0.3, message = "merging data ...")
        data <- data %>% plyr::join_all(by = input$gene_col, type = "inner")
        colnames(data) <- c("geneID", input$merge_file$name %>% stringr::str_remove(pattern = "\\..*"))
      }else {
        incProgress(0.5, message = "reading data ...")
        data <- lapply(input$merge_file$datapath, function(x){
          df <- upload.data(path = x, header = T, row_names = F)
          colnames(df)[1] <- "geneID"
          df
        }) 
        print(data[[1]] %>% head(2))
        print(data[[2]] %>% head(2))
        incProgress(0.3, message = "merging data ...")
        data <- data %>% plyr::join_all(by = "geneID", type = "inner") %>% as.data.frame()
      }
      
      print(head(data))
      
      # process duplicated genes
      if (any(duplicated(data$geneID))) {
        if (input$methods_dup_m == "Mean") {
          data <- data %>% dplyr::group_by(geneID) %>% dplyr::summarise_if(is.numeric, mean) %>% as.data.frame()
        }else if (input$methods_dup_m == "Sum") {
          data <- data %>% dplyr::group_by(geneID) %>% dplyr::summarise_if(is.numeric, sum) %>% as.data.frame()
        }else {
          data <- data[!duplicated(data$geneID), ]
        }
      }
      rownames(data) <- data$geneID
      data <- data[, colnames(data) != "geneID"]
      
      # print(head(data))
      # show the Total gene and non-expressed genes
      output$merge_geneBox1 <- renderInfoBox({
        infoBox("Total Gene:", dim(data)[1], width = 12, color = "olive", icon = icon("dna"), fill = TRUE)
      })
      output$merge_geneBox2 <- renderInfoBox({
        infoBox("Zero Gene:", dim(data[rowSums(data) == 0, ])[1], width = 12, color = "yellow", icon = icon("dna"), fill = TRUE)
      })
      
      # remove non expression genes
      incProgress(0.1, message = "removing non exp genes ...")
      nz_data <- data[rowSums(data) > 0, ]
    })
    return(nz_data)
  })
  
  output$merged_tab <- DT::renderDataTable({
    data_merge()
  },
  rownames = T,
  options = list(pageLength = 15, autoWidth = F, scrollX=TRUE, 
                 scrollY=ifelse(input$merge_type == 'Individual files', "544px", "417px"))
  )
  
  output$download_mergetab <- downloadHandler(
    filename = function()  {paste0("matrix-mergence-results-", as.character(Sys.time()), ".csv")},
    content = function(file) {
      write.csv(data_merge(), file, row.names = T)
    }
  )
  
  ###----------------------------------------------------------------------------------###
  # output$anhub_species <- renderUI({
  #   selectizeInput("anhub_species", label = "Species:",  
  #                  choices = paste0(readRDS("AnnotationHub_species.rds")[,1], 
  #                                   " (taxid=", readRDS("AnnotationHub_species.rds")[,2], ")"), width = "100%")
  # })

})
