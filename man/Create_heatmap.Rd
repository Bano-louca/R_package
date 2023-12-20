\name{Create_heatmap}
\alias{Create_heatmap}
\title{
Function to create a heatmap with/without stratifying by groups
}
\description{
This function creates a heatmap using ggplot with or without stratifying by groups. Optional arguements to alter transparency level for non-sig correlations and change x axis label orientation.
}
\usage{
Create_heatmap(dataFrame = NULL,
               x_axis_labels = NULL,
               y_axis_labels = NULL,
               correlation_coefficient = NULL,
               pvalue = NULL,
               group = NULL,
               Transparency_level = 25, X_axis_label_orientation = 45)
}

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
  \item{group (Optional)}{
      labels denoting grouping
  }
    \item{Transparency_level (Optional)}{
      Percentage of transparency to apply to non-significant box labels (coefficient & p-values). Higher = more transparent (default = 25)
  }
  \item{X_axis_label_orientation (Optional)}{
      Orientation of x axis labels. Must be either 45 or 90
  }
}
\author{
Panayiotis Louca
}


}
