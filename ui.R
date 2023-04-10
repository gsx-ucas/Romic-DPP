#   ____________________________________________________________________________
#   UI                                                                      ####
library(DT)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)

jsCode <- "shinyjs.collapse = function(boxid) {
      $('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
};"


shinyUI(
  fluidPage(
    style = "width:100%; padding: 0px",
    useShinydashboard(),
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = "collapse"),

    navbarPage(
      id = "mainMenu",
      title = "QRAP",
      theme = "style/style.css",
      # theme = "style/layui.css",
      # footer = includeHTML("footer.html"),
      fluid = TRUE,
      collapsible = TRUE,

      # ID conversion page start
      tabPanel(
        "ID conversion", value = "gprofiler2", 
        fluidPage(
          style = "margin-left: 10px; margin-right:10px;",
          column(
            width = 4, style = "padding: 0px; margin:0px; border-radius: 10px; background-color: transparent",
            box(
              id = "upload_box", title = "Upload & Preview", width = 12, status = NULL, solidHeader = TRUE, collapsible = T,
              fileInput("file", "Choose input File:", accept = c(".csv", ".tsv", ".txt", ".tab", ".gz"),
                        placeholder = "*.csv/.txt format reads counts file", width = "100%"),
              checkboxInput(inputId = "header", label = "First row as header ?", value = TRUE, width = "100%"),
              checkboxInput(inputId = "row_names", label = "First column as rownames ?", value = TRUE, width = "100%"),
              actionButton("upload", "Upload >>", class = "run-button",  width='100%')
            ),
            conditionalPanel(
              "input.upload",
              box(
                id = "convert_box", title = "Convert Gene ID", width = 12, status = NULL, solidHeader = TRUE, collapsible = F,
                # awesomeRadio(
                #   inputId = "convert_db", label = "Database:",  choices = c("NCBI(local)", "ENSEMBL(online)"), 
                #   status = "info", inline = TRUE,  checkbox = TRUE
                # ),
                awesomeRadio(
                  inputId = "convert_type", label = "Conversion type:",  choices = c("ID conversion", "Orthology search"),
                  selected = "ID conversion", inline = TRUE,  checkbox = TRUE
                ),
                selectizeInput("convert_db", label = "Database:",  choices = c("NCBI (local)", "ENSEMBL (online)"), width = "100%"),
                uiOutput("source_species"),
                conditionalPanel(
                  "input.convert_type=='ID conversion'",
                  # uiOutput("source_species"),
                  uiOutput("convert_inputs"),
                  uiOutput("convert_targets"),
                ),
                conditionalPanel(
                  "input.convert_type=='Orthology search'",
                  # uiOutput("source_species"),
                  uiOutput("target_species"),
                  uiOutput("orth_inputs"),
                  uiOutput("orth_targets")
                ),
                selectInput("methods_dup", "How to process duplicated genes:", choices = c("Mean", "Sum", "Drop"), width = "100%"),
                actionButton("gconvert", "conversion >>", class = "run-button",  width='100%')
              )
            )
          ),
          column(
            width = 6, style = "padding: 0px; margin:0px",
            box(
              id = "preview_box", title = "Matrix before conversion", width = 12, status = NULL, solidHeader = TRUE, collapsible = T,
              DT::dataTableOutput("rawTable")
            ),
            conditionalPanel(
              "input.gconvert",
              box(
                title = "Matrix after conversion", width = 12, status = NULL, solidHeader = TRUE, collapsible = F,
                shinycssloaders::withSpinner(DT::dataTableOutput("convertDF"))
              )
            )
          ),
          column(
            width = 2, style = "padding: 0px; margin:0px",
            infoBoxOutput("loadingBox1", width = 12),
            infoBoxOutput("loadingBox2", width = 12),
            infoBoxOutput("convertedBox", width = 12),
            conditionalPanel(
              "input.gconvert",
              div(
                style = "padding: 15px 15px",
                downloadBttn(outputId = "download_convert", label = "Download CSV", style = "bordered", color = "primary")
              )
            )
          )
        )
      ),
      # Matrix mergence page start
      tabPanel(
        "Matrix mergence", value = "merge",
        fluidPage(
          style = "margin-left: 10px; margin-right:10px;",
          column(
            width = 4, style = "padding: 0px; margin:0px; border-radius: 10px; background-color: transparent",
            box(
              id = "merge_upload_box", title = "Upload & Preview", width = 12, status = NULL, solidHeader = TRUE, collapsible = T,
              awesomeRadio(
                inputId = "merge_type", label = "File types to merge:",  choices = c("Expression matrix", "Individual files"),
                status = "info", inline = TRUE,  checkbox = TRUE
              ),
              fileInput("merge_file", "Choose input File:", accept = c(".csv", ".tsv", ".txt", ".tab", ".gz"),
                        placeholder = "*.csv/.txt format reads counts file", width = "100%", multiple = T),
              checkboxInput(inputId = "merge_header", label = "Dose the first row of the file contain the sample names?", value = TRUE, width = "100%"),
              conditionalPanel(
                "input.merge_type == 'Expression matrix'",
                checkboxInput(inputId = "merge_row_names", label = "Dose the first column of the file contain the gene names?", value = TRUE, width = "100%"),
              ),
              conditionalPanel(
                "input.merge_type == 'Individual files'",
                checkboxInput(inputId = "merge_skip1", label = "Whether to skip the first row when reading in data?", value = FALSE, width = "100%"),
                uiOutput("gene_column"),
                uiOutput("expr_column")
              ),
              uiOutput("merge_preview_table"),
              selectInput("methods_dup_m", "How to process duplicated genes:", choices = c("Mean", "Sum", "Drop"), width = "100%"),
              shinyjs::disabled(actionButton("merge_start", "Upload >>", class = "run-button",  width='100%'))
            )
          ),
          column(
            width = 6, style = "padding: 0px; margin:0px",
            box(
              id = "merge_preview_box", title = "Matrix before conversion", width = 12, status = NULL, solidHeader = TRUE, collapsible = T,
              DT::dataTableOutput("merged_tab")
            )
          ),
          column(
            width = 2, style = "padding: 0px; margin:0px",
            infoBoxOutput("merge_sampleBox", width = 12),
            infoBoxOutput("merge_geneBox1", width = 12),
            infoBoxOutput("merge_geneBox2", width = 12),
            conditionalPanel(
              "input.merge_start",
              div(
                style = "padding: 15px 15px",
                downloadBttn(outputId = "download_mergetab", label =  "Download CSV", style = "bordered", color = "primary", block = T)
              )
            )
          )
        )
      ),
      # ----------------------------------
      # tab panel last - About
      tabPanel("Document"
      )
    )
  )
)
