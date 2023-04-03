#   ____________________________________________________________________________
#   UI                                                                      ####
library(DT)
library(shiny)
library(plotly)
library(shinyjs)
library(shinyBS)
# library(shinythemes)
library(shinyalert)
library(shinyWidgets)
library(shinydashboard)
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
      # title = "QRAP",
      title = img(src = "images/logo_img.png", height = "30px", style = "margin-top: -2px"),
      theme = "style/style.css",
      # theme = "style/layui.css",
      # footer = includeHTML("footer.html"),
      fluid = TRUE,
      collapsible = TRUE,

      tabPanel(
        "Matrix Preview", value = "preview", 
        fluidPage(
          style = "margin-left: 10px; margin-right:10px;",
          column(
            width = 4, style = "padding: 0px; margin:0px; border-radius: 10px; background-color: transparent",
            box(
              id = "upload_box", title = "Upload & Preview", width = 12, status = NULL, solidHeader = TRUE, collapsible = T,
              fileInput("file", "Choose input File:", accept = c(".csv", ".tsv", ".txt", ".tab"),
                        placeholder = "*(.csv/.txt format reads counts file)", width = "100%"),
              checkboxInput(inputId = "header", label = "First row as header ?", value = TRUE, width = "100%"),
              checkboxInput(inputId = "row_names", label = "First column as rownames ?", value = TRUE, width = "100%"),
              actionButton("upload", "Upload >>", class = "run-button",  width='100%')
            ),
            conditionalPanel(
              "input.upload",
              box(
                id = "convert_box", title = "Convert Gene ID", width = 12, status = NULL, solidHeader = TRUE, collapsible = T,
                uiOutput("supported_species"),
                selectInput("keyType", "Gene Types:", choices = c("SYMBOL", "ENSEMBL", "ENTREZID"), width = "100%"),
                selectInput("methods_dup", "How to process duplicated genes:", choices = c("Mean", "Sum", "Drop"), width = "100%"),
                actionButton("conversion", "conversion >>", class = "run-button",  width='100%')
              )
            )
          ),
          column(
            width = 6, style = "padding: 0px; margin:0px",
            DT::dataTableOutput("rawTable"),
            uiOutput("preview_mat_card")
          ),
          column(
            width = 2, style = "padding: 0px; margin:0px",
            infoBoxOutput("loadingBox1", width = 12),
            infoBoxOutput("loadingBox2", width = 12)
          )
        )
      ),
      tabPanel("Matrix process", value = "deseq", source("modules/2-ui-condition.R", local = T)$value),
      # ----------------------------------
      # tab panel last - About
      tabPanel("About"
      )
    )
  )
)
