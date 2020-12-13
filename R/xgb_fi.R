#' A XGboost Feature Interaction Graphing Function
#'
#' This function allows you to generate a shiny app that outputs the interaction of an xgbmodel.
#' @param model type: XGBModel
#' @param write.loc type: filepath File path for the destination folder to write results in .xlsx format. Default is D:/xgbfi
#' @param xgbfi.loc type: filepath File path for the XGBfi folder. Default is D:/xgbfi
#' @param max.interaction.depth type: integer Upper bound for extracted feature interactions depth
#' @param features type: Character Vector A character vector of the features in the model.
#' @param max.deepening  type: integer Upper bound for interaction start deepening (zero deepening => interactions starting @root only)
#' @param max.trees type: integer Upper bound for trees to be parsed
#' @param top.k type: integer Upper bound for exportet feature interactions per depth level
#' @param max.histograms type: integer Amounts of split value histograms
#'
#' @keywords xgboost, xgbfi
#'
# @import data.table
# @import DT
#' @importFrom xgboost xgb.dump
#' @importFrom readxl read_xlsx
# @importFrom magrittr '%>%'
# @import shiny
#'
#' @examples
#' library(xgboost)
#' library(Ecdat)
#' data(Icecream)
#' train.data <- data.matrix(Icecream[,-1])
#' bst <- xgboost(data = train.data, label = Icecream$cons, max.depth = 3, eta = 1, nthread = 2, nround = 2, objective = "reg:linear")
#' features <- names(Icecream[,-1])
#' xgb_fi(model = bst, features = features)
#'
#' @export

xgb_fi <- function(model,
                   write.loc = "C:/xgbfi",
                   xgbfi.loc = "C:/xgbfi",
                   features = NULL,
                   max.interaction.depth = 3,
                   max.deepening = -1,
                   max.trees = -1,
                   top.k = 100,
                   max.histograms = 10) {

  xgbfi_exe <- paste0(xgbfi.loc, "/", "bin", "/", "XgbFeatureInteractions.exe")

  featureVector <- c()
  for (i in 1:length(features)) {
    featureVector[i] <- paste(i - 1, features[i], "q", sep = "\t")
  }

  if(!file.exists(file.path(xgbfi.loc, "bin", "XgbFeatureInteractions.exe"))) {
    msg <- paste0("'XGBfi' must be installed to install XGBfiR.")

    message(
      msg, "\n",
      "Would you like to install XGBfi? ", "\n",
      "This will source <https://github.com/Far0n/xgbfi/archive/master.zip>.", "\n",
      "It will be installed in the following location. ", "\n", dirname(xgbfi.loc)
    )

    if (menu(c("Yes", "No")) != 1) {
      stop("'XGBfiR' not installed.", call. = FALSE)
    } else {
      temp <- tempfile("xgbfi")
      download.file("https://github.com/Far0n/xgbfi/archive/master.zip", temp, mode="wb")
      unzip(temp, exdir = dirname(xgbfi.loc))
      file.rename(from = file.path(dirname(xgbfi.loc), 'xgbfi-master'),
                  to = file.path(dirname(xgbfi.loc), "xgbfi"))
    }
  }

  write.table(featureVector, paste0(write.loc, "/", "fmap.txt"),
              row.names = FALSE,
              quote = FALSE,
              col.names = FALSE)

  invisible(xgboost::xgb.dump(model = model,
                              fname = paste0(write.loc, "/", "xgb.dump"),
                              fmap = paste0(write.loc, "/", "fmap.txt"),
                              with_stats = TRUE))

  command <- paste0('"',xgbfi_exe,'"',
                    ' -m ', paste0(write.loc, "/", "xgb.dump"),
                    ' -d ', max.interaction.depth,
                    ' -g ', max.deepening,
                    ' -t ', max.trees,
                    ' -k ', top.k,
                    ' -h ', max.histograms,
                    ' -o ', paste0(xgbfi.loc, "/", "XgbFeatureInteractions"))

  out <- system(command, intern =TRUE)

  if (!dir.exists(file.path(xgbfi.loc, "bin", "EPPlus"))) {
    dir.create(file.path(xgbfi.loc, "bin", "EPPlus"))
    file.copy(file.path(xgbfi.loc, "bin", "lib", "EPPlus.dll"), file.path(xgbfi.loc, "bin", "EPPlus"))
  }

  if (!dir.exists(file.path(xgbfi.loc, "bin", "NGenerics"))) {
    dir.create(file.path(xgbfi.loc, "bin", "NGenerics"))
    file.copy(file.path(xgbfi.loc, "bin", "lib", "NGenerics.dll"), file.path(xgbfi.loc, "bin", "NGenerics"))
  }

  return(purrr::map(1:3,
                    ~ readxl::read_xlsx(paste0(write.loc, "/", "XgbFeatureInteractions.xlsx"),
                                        sheet = .x)))
}

#'%>%' <- magrittr::'%>%'

# shiny::shinyApp(ui = shiny::fluidPage( shiny::navbarPage("XGBoost",
#                                                          shiny::tabPanel("XGBoost Feature Interaction",
#                                                                          shiny::fluidPage(
#                                                                            shiny::tabsetPanel(
#                                                                              shiny::tabPanel("Feature Interaction",
#                                                                                              value = 1,
#                                                                                              shiny::h4("Feature Interaction"),
#                                                                                              shiny::p("The feature interactions present in the model."),
#                                                                                              DT::dataTableOutput("tableVars1")),
#                                                                              shiny::tabPanel("2 Variable Feature Interaction",
#                                                                                              value = 2,
#                                                                                              shiny::h4("Feature Interaction"),
#                                                                                              shiny::p("The 2 variables feature interactions present in the model."),
#                                                                                              DT::dataTableOutput("tableVars2")),
#                                                                              shiny::tabPanel("3 Variable Feature Interaction",
#                                                                                              value = 2,
#                                                                                              shiny::h4("Feature Interaction"),
#                                                                                              shiny::p("The 3 variables feature interactions present in the model."),
#                                                                                              DT::dataTableOutput("tableVars3")),
#                                                                              id = "conditionedPanels"))))),
#                 server = function(input, output) {
#
#                   tableVars1 <- function(){
#                     featuresimp <- openxlsx::read.xlsx(paste0(xgbfi.loc, "/", "XgbFeatureInteractions.xlsx"))
#                     featuresimp <- data.table::as.data.table(featuresimp)
#                     featuresimp[, Gain.Percentage := Gain/sum(Gain)]
#                     cols <- c('wFScore', 'Average.wFScore', 'Average.Gain', 'Expected.Gain', 'Gain.Percentage')
#                     featuresimp[,(cols) := round(.SD,4), .SDcols=cols]
#                     setcolorder(featuresimp, c("Interaction", "Gain.Percentage", colnames(featuresimp)[!colnames(featuresimp) %in% c("Interaction", "Gain.Percentage")]))
#
#                   }
#
#
#                   tableVars2 <- function(){
#                     featuresimp <- openxlsx::read.xlsx(paste0(xgbfi.loc, "/", "XgbFeatureInteractions.xlsx"), sheet = 2)
#                     featuresimp <- data.table::as.data.table(featuresimp)
#                     featuresimp[, c("Var1", "Var2") := tstrsplit(Interaction, "|", fixed=TRUE)]
#                     featuresimp[, ':='(Interaction = NULL)]
#                     featuresimp[, Gain.Percentage := Gain/sum(Gain)]
#                     cols <- c('wFScore', 'Average.wFScore', 'Average.Gain', 'Expected.Gain', 'Gain.Percentage')
#                     featuresimp[,(cols) := round(.SD,4), .SDcols=cols]
#                     setcolorder(featuresimp, c("Var1", "Var2", "Gain.Percentage", colnames(featuresimp)[!colnames(featuresimp) %in% c("Var1", "Var2", "Gain.Percentage")]))
#
#                   }
#
#
#                   tableVars3 <- function(){
#                     featuresimp <- openxlsx::read.xlsx(paste0(xgbfi.loc, "/", "XgbFeatureInteractions.xlsx"), sheet = 3)
#                     featuresimp <- data.table::as.data.table(featuresimp)
#                     featuresimp[, c("Var1", "Var2", "Var3") := tstrsplit(Interaction, "|", fixed=TRUE)]
#                     featuresimp[, ':='(Interaction  = NULL)]
#                     featuresimp[, Gain.Percentage := Gain/sum(Gain)]
#                     cols <- c('wFScore', 'Average.wFScore', 'Average.Gain', 'Expected.Gain', 'Gain.Percentage')
#                     featuresimp[,(cols) := round(.SD,4), .SDcols=cols]
#                     setcolorder(featuresimp, c("Var1", "Var2", "Var3", "Gain.Percentage", colnames(featuresimp)[!colnames(featuresimp) %in% c("Var1", "Var2", "Var3", "Gain.Percentage")]))
#                   }
#
#                   output$tableVars1 <- DT::renderDataTable(DT::datatable(tableVars1(),
#                                                                          filter = 'top',
#                                                                          class = 'hover stripe',
#                                                                          options = list(pageLength = 100,
#                                                                                         lengthMenu = c(10, 50, 100, 200))) %>%
#                                                              DT::formatStyle('Gain.Percentage',
#                                                                              background = styleColorBar(c(0, max(tableVars1()$Gain.Percentage)), 'lightgreen'),
#                                                                              backgroundSize = '100% 90%',
#                                                                              backgroundRepeat = 'no-repeat',
#                                                                              backgroundPosition = 'center') %>%
#                                                              DT::formatPercentage(columns = c('Gain.Percentage'),
#                                                                                   digits = 4))
#
#                   output$tableVars2 <- DT::renderDataTable(DT::datatable(tableVars2(),
#                                                                          filter = 'top',
#                                                                          class = 'hover stripe',
#                                                                          options = list(pageLength = 100,
#                                                                                         lengthMenu = c(10, 50, 100, 200))) %>%
#                                                              DT::formatStyle('Gain.Percentage',
#                                                                              background = DT::styleColorBar(c(0, max(tableVars2()$Gain.Percentage)), 'lightgreen'),
#                                                                              backgroundSize = '100% 90%',
#                                                                              backgroundRepeat = 'no-repeat',
#                                                                              backgroundPosition = 'center') %>%
#                                                              DT::formatPercentage(columns = c('Gain.Percentage'),
#                                                                                   digits = 4))
#
#                   output$tableVars3 <- DT::renderDataTable(DT::datatable(tableVars3(),
#                                                                          filter = 'top',
#                                                                          class = 'hover stripe',
#                                                                          options = list(pageLength = 100,
#                                                                                         lengthMenu = c(10, 50, 100, 200))) %>%
#                                                              DT::formatStyle('Gain.Percentage',
#                                                                              background = DT::styleColorBar(c(0, max(tableVars3()$Gain.Percentage)), 'lightgreen'),
#                                                                              backgroundSize = '100% 90%',
#                                                                              backgroundRepeat = 'no-repeat',
#                                                                              backgroundPosition = 'center') %>%
#                                                              DT::formatPercentage(columns = c('Gain.Percentage'),
#                                                                                   digits = 4))
#
#                 })
