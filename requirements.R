library(DT)
library(plyr)
library(dplyr)
library(gprofiler2)

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)

pkgs <- c("DT", "plyr", "dplyr", "shiny", "shinyjs", "shinyWidgets", "shinycssloaders", "gprofiler2")

for (i in pkgs) {
  if (!requireNamespace(i)) {
    install.packages(i)
  }
}
