\name{Create_heatmap}
\alias{Create_heatmap}
\title{
Function to create a heatmap with/without stratifying by groups
}
\description{
This function creates a heatmap using ggplot with or without stratifying by groups, optiional arguements to alter transparency level for non-sig correlations.
}
\usage{
Create_heatmap(dat, stratify_by_group, Transparency_level, X_axis_label_orientation, y_axis_plot_order = NULL, x_axis_plot_order = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{dat}{
       Dataset in the long format with 4 columns
  }
  \item{x_axis_labels}{
       the labels for the x axis
  }
  \item{y_axis_labels}{
       the labels for the y axis
  }
  \item{correlation_coefficient}{
       the coefficients to plot
  }
  \item{pvalue}{
       the p values from the correlation tests
  }
  
}
\(Optional){
\item{group}{
  labels denoting grouping
  }
}
\author{
Panayiotis Louca
}


}
