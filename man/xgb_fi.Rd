% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xgb_fi.R
\name{xgb_fi}
\alias{xgb_fi}
\title{A XGboost Feature Interaction Graphing Function}
\usage{
xgb_fi(model, write.loc = "D:/xgbfi", xgbfi.loc = "D:/xgbfi",
  features = NULL, max.interaction.depth = 3, max.deepening = -1,
  max.trees = -1, top.k = 100, max.histograms = 10)
}
\arguments{
\item{model}{type: XGBModel}

\item{write.loc}{type: filepath File path for the destination folder to write results in .xlsx format. Default is D:/xgbfi}

\item{xgbfi.loc}{type: filepath File path for the XGBfi folder. Default is D:/xgbfi}

\item{features}{type: Character Vector A character vector of the features in the model.}

\item{max.interaction.depth}{type: integer Upper bound for extracted feature interactions depth}

\item{max.deepening}{type: integer Upper bound for interaction start deepening (zero deepening => interactions starting @root only)}

\item{max.trees}{type: integer Upper bound for trees to be parsed}

\item{top.k}{type: integer Upper bound for exportet feature interactions per depth level}

\item{max.histograms}{type: integer Amounts of split value histograms}
}
\description{
This function allows you to generate a shiny app that outputs the interaction of an xgbmodel.
}
\examples{
library(xgboost)
library(Ecdat)
data(Icecream)
train.data <- data.matrix(Icecream[,-1])
bst <- xgboost(data = train.data, label = Icecream$cons, max.depth = 3, eta = 1, nthread = 2, nround = 2, objective = "reg:linear")
features <- names(Icecream[,-1])
xgb_fi(model = bst, features = features)

}
\keyword{xgbfi}
\keyword{xgboost,}
