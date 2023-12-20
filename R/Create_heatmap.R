Create_heatmap <- function(dataFrame = NULL, x_axis_labels = NULL, y_axis_labels = NULL, correlation_coefficient = NULL, pvalue = NULL, group = NULL,
                           Transparency_level = 25, X_axis_label_orientation = 45) {

  # Perform input argument checks
  if (is.null(dataFrame) || is.null(y_axis_labels) || is.null(x_axis_labels) || is.null(correlation_coefficient) || is.null(pvalue)) {
    stop("Please provide valid inputs for dataFrame, x_axis_labels, y_axis_labels, correlation coefficients, and correlation pvalues.")
  } # check inputs for main variables

  if (!is.data.frame(dataFrame)) {
    stop("dataFrame must be a data frame.")
  } # check class of input dataset

  if (!(y_axis_labels %in% names(dataFrame))) {
    stop("The specified variable with y axis labels does not exist in the dataFrame.")
  }

  if (!(x_axis_labels %in% names(dataFrame))) {
    stop("The specified variable with x axis labels  does not exist in the dataFrame.")
  }

  if (!(correlation_coefficient %in% names(dataFrame))) {
    stop("The specified variable with correlation coefficients does not exist in the dataFrame.")
  }

  if (!(pvalue %in% names(dataFrame))) {
    stop("The specified variable with correlation pvalues does not exist in the dataFrame.")
  }

  if (!is.null(group) && !(group %in% names(dataFrame))) {
    stop("The specified variable with the group does not exist in the dataFrame.")
  }

  if (!is.null(X_axis_label_orientation) && !(X_axis_label_orientation %in% c(45, 90))) {
    stop("Invalid angle. Please provide an X_axis_label_orientation of 45 or 90.")
  }

  if (missing(Transparency_level)) {
    print("Custom transparency level not set, using default of 25%")
  }

  # ------------------------------------------------------------------------------------------------------------------- #

  if (is.null(group)) {

    # Rename input variables
    desired_names <- c("y_labels", "x_labels", "corr_coef", "pvals")
    positions <- match(c(y_axis_labels, x_axis_labels, correlation_coefficient, pvalue), names(dataFrame))
    names(dataFrame)[positions] <- desired_names

    # ------------------------------------------------------------------------------------------------------------------- #

    # set data types
    dataFrame$y_labels <- as.character(dataFrame$y_labels)
    dataFrame$x_labels <- as.character(dataFrame$x_labels)
    dataFrame$corr_coef <- as.numeric(dataFrame$corr_coef)
    dataFrame$pvals <- as.numeric(dataFrame$pvals)
  } else if (!is.null(group)) {

    # Rename input variables
    desired_names <- c("y_labels", "x_labels", "corr_coef", "pvals", "group_cat")
    positions <- match(c(y_axis_labels, x_axis_labels, correlation_coefficient, pvalue, group), names(dataFrame))
    names(dataFrame)[positions] <- desired_names

    # ------------------------------------------------------------------------------------------------------------------- #

    # set data types
    dataFrame$y_labels <- as.character(dataFrame$y_labels)
    dataFrame$x_labels <- as.character(dataFrame$x_labels)
    dataFrame$corr_coef <- as.numeric(dataFrame$corr_coef)
    dataFrame$pvals <- as.numeric(dataFrame$pvals)
    dataFrame$group_cat <- as.factor(dataFrame$group_cat)

  }


  # ------------------------------------------------------------------------------------------------------------------- #

  # Ordering of x and y axis labels

  # Check if order has been set
  if (exists("y_axis_plot_order", envir = .GlobalEnv)) {

    print("Setting y axis order")
    dataFrame$y_labels = factor(dataFrame$y_labels, levels = y_axis_plot_order)

  } else if (exists("x_axis_plot_order", envir = .GlobalEnv)) {

    print("Setting x axis order")
    dataFrame$x_labels = factor(dataFrame$x_labels, levels = x_axis_plot_order)

  } else {

    print("Axis ordering has not been set.")
  }

  # ------------------------------------------------------------------------------------------------------------------- #

  # Ordering of group labels

  # Check if order has been set

  if (!is.null(group) && (exists("group_plot_order", envir = .GlobalEnv))) {

    print("Setting group order")
    dataFrame$group_cat = factor(dataFrame$group_cat, levels = group_plot_order)

  }

  # ------------------------------------------------------------------------------------------------------------------- #
  # create labels for within the box

  box_label = paste0(ifelse(round(signif(dataFrame$corr_coef,2), digits = 2) != 0.00, # paste coefficient using ifelse statement above
                            yes = round(signif(dataFrame$corr_coef,2), digits = 2),
                            ifelse(round(signif(dataFrame$corr_coef,2), digits = 3) != 0.000,
                                   yes = round(signif(dataFrame$corr_coef,2), digits = 3),
                                   no = round(signif(dataFrame$corr_coef,2), digits = 4)
                            )),
                     "\n(",
                     ifelse(round(dataFrame$pvals, digits = 2) == 0.00, # paste p value using ifelse statement above
                            sprintf("%.1e", dataFrame$pvals),
                            round(dataFrame$pvals, digits = 2)),
                     ")")

  box_label = gsub("(\\.\\d+)?0+e", "e", box_label) # remove trailing 0 before the e

  # ------------------------------------------------------------------------------------------------------------------- #

  # No stratification by group

  if (is.null(group)) {
    # If want to stratify by a group and "group" is a column in dat

    print("No group stratification selected, proceeding...")



    # ------------------------------------------------------------------------------------------------------------------- #

    # create plot

    plot <-

      ggplot(dataFrame, aes(x = x_labels, y = y_labels)) +

      # Represent the correlation coefficients using colors
      geom_tile(aes(fill = corr_coef), color = "white", width = 1) +


      # Add text labels for correlation coefficients and p-values
      geom_text(aes(label = box_label),
                size = 2.5, color = "black", show.legend = FALSE,
                fontface = ifelse(dataFrame$pvals < 0.05, "bold", "plain"), # this sets labels that're sig to bold
                alpha = ifelse(dataFrame$pvals < 0.05, 1, 1-Transparency_level /100), # this sets labels that are not sig to slightly transparent
                hjust = 0.5) +

      # Customize the color scale
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +

      # Customize the theme
      theme_minimal() +

      # Rotate x-axis labels for better readability
      theme(axis.text.x = element_text(angle = X_axis_label_orientation, vjust = 1, hjust = 1), # rotate x axis labels
            title = element_blank(), # remove main title
            axis.title = element_blank(), # remove axis titles
            strip.text = element_text(face = "bold", hjust = 0, size = 14),# make sub heading bold and change size
            axis.text = element_text(size = 11, face = "bold") # make both axis text bold
      )

    print(plot) # print plot


  }
  if (!is.null(group)) {
    print("Group stratification selected, proceeding...")



    # ------------------------------------------------------------------------------------------------------------------- #

    # create plot

    plot <-

      ggplot(dataFrame, aes(x = x_labels, y = y_labels)) +

      # Represent the correlation coefficients using colors
      geom_tile(aes(fill = corr_coef), color = "white", width = 1) +

      ggforce::facet_col( # facet by group
        facets = ~ group_cat,
        scales = "free_y",
        space = "free"
      ) +



      # Add text labels for correlation coefficients and p-values
      geom_text(aes(label = box_label),
                size = 2.5, color = "black", show.legend = FALSE,
                fontface = ifelse(dataFrame$pvals < 0.05, "bold", "plain"), # this sets labels that're sig to bold
                alpha = ifelse(dataFrame$pvals < 0.05, 1, 1-Transparency_level /100), # this sets labels that are not sig to slightly transparent
                hjust = 0.5) +

      # Customize the color scale
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +

      # Customize the theme
      theme_minimal() +

      # Rotate x-axis labels for better readability
      theme(axis.text.x = element_text(angle = X_axis_label_orientation, vjust = 1, hjust = 1), # rotate x axis labels
            title = element_blank(), # remove main title
            axis.title = element_blank(), # remove axis titles
            strip.text = element_text(face = "bold", hjust = 0, size = 14),# make sub heading bold and change size
            axis.text = element_text(size = 11, face = "bold") # make both axis text bold
      )

    print(plot) # print plot

  }


}
