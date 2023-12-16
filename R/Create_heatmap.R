Create_heatmap <- function(dat, stratify_by_group, Transparency_level, X_axis_label_orientation, y_axis_plot_order = NULL, x_axis_plot_order = NULL) {

  if ("y_axis_labels" %in% colnames(dat) &&
      "x_axis_labels" %in% colnames(dat) &&
      "correlation_coefficient" %in% colnames(dat) &&
      "pvalue" %in% colnames(dat)) {

    if (!is.character(dat$y_axis_labels)) {
      dat$y_axis_labels <- as.character(dat$y_axis_labels)
    }

    if (!is.character(dat$x_axis_labels)) {
      dat$x_axis_labels <- as.character(dat$x_axis_labels)
    }

    if (!is.numeric(dat$correlation_coefficient)) {
      dat$correlation_coefficient <- as.numeric(dat$correlation_coefficient)
    }

    if (!is.numeric(dat$pvalue)) {
      dat$pvalue <- as.numeric(dat$pvalue)
    }

    print("Great, your dataset is ready!")
  } else {
    print("Your dataset doesn't have the correct names. Your dataset sould have at least 'x_axis_labels', 'y_axis_label', 'correlation_coefficient', and 'pvalue'.")
  }

  "y_axis_plot_order" %in% ls()


  # ------------------------------------------------------------------------------------------------------------------- #
  # Ordering of x and y axis labels

  # Check if order has been set
  if (exists("y_axis_plot_order", envir = .GlobalEnv)) {

    print("Setting axis order")
    dat$y_axis_labels = factor(dat$y_axis_labels, levels = y_axis_plot_order)

  } else if (exists("x_axis_plot_order", envir = .GlobalEnv)) {

    print("Setting x axis order")
    dat$x_axis_labels = factor(dat$x_axis_labels, levels = x_axis_plot_order)

  } else {

    print("Axis ordering has not been set. Skipping...")
  }

  # ------------------------------------------------------------------------------------------------------------------- #
  # create labels for within the box

  box_label = paste0(ifelse(round(signif(dat$correlation_coefficient,2), digits = 2) != 0.00, # paste coefficient using ifelse statement above
                            yes = round(signif(dat$correlation_coefficient,2), digits = 2),
                            ifelse(round(signif(dat$correlation_coefficient,2), digits = 3) != 0.000,
                                   yes = round(signif(dat$correlation_coefficient,2), digits = 3),
                                   no = round(signif(dat$correlation_coefficient,2), digits = 4)
                            )),
                     "\n(",
                     ifelse(round(dat$pvalue, digits = 2) == 0.00, # paste p value using ifelse statement above
                            sprintf("%.1e", dat$pvalue),
                            round(dat$pvalue, digits = 2)),
                     ")")

  box_label = gsub("(\\.\\d+)?0+e", "e", box_label) # remove trailing 0 before the e

  # ------------------------------------------------------------------------------------------------------------------- #
  # Stratification by group

  # Ensure the response is lowercase for case-insensitive comparison
  stratify_by_group <- tolower(stratify_by_group)


  if (stratify_by_group == "y" && "group" %in% colnames(dat)) {
    # If want to stratify by a group and "group" is a column in dat
    if (!is.factor(dat$group)) {
      dat$group <- as.factor(dat$group)
    }
    print("Group stratification selected, proceeding...")



    # ------------------------------------------------------------------------------------------------------------------- #

    # create plot

    plot <-
      ggplot(dat, aes(x = x_axis_labels, y = y_axis_labels)) +

      # Represent the correlation coefficients using colors
      geom_tile(aes(fill = correlation_coefficient), color = "white", width = 1) +

      ggforce::facet_col( # facet by group
        facets = ~ group,
        scales = "free_y",
        space = "free"
      ) +

      # Add text labels for correlation coefficients and p-values
      geom_text(aes(label = box_label),
                size = 2.5, color = "black", show.legend = FALSE,
                fontface = ifelse(dat$pvalue < 0.05, "bold", "plain"), # this sets labels that're sig to bold
                alpha = ifelse(dat$pvalue < 0.05, 1, Transparency_level /100), # this sets labels that are not sig to slightly transparent (alpha = 0.55) increase 0.55 to make less transparent
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

    return(plot)

    # ------------------------------------------------------------------------------------------------------------------- #

    # if don't want to stratify
  } else if (stratify_by_group == "n") {
    print("No group stratification selected, proceeding...")


    # ------------------------------------------------------------------------------------------------------------------- #

    # create plot

    plot <-

      ggplot(dat, aes(x = x_axis_labels, y = y_axis_labels)) +

      # Represent the correlation coefficients using colors
      geom_tile(aes(fill = correlation_coefficient), color = "white", width = 1) +



      # Add text labels for correlation coefficients and p-values
      geom_text(aes(label = box_label),
                size = 2.5, color = "black", show.legend = FALSE,
                fontface = ifelse(dat$pvalue < 0.05, "bold", "plain"), # this sets labels that're sig to bold
                alpha = ifelse(dat$pvalue < 0.05, 1, Transparency_level /100), # this sets labels that are not sig to slightly transparent (alpha = 0.55) increase 0.55 to make less transparent
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
    return(plot)

    # ------------------------------------------------------------------------------------------------------------------- #

    # if wanted to stratify by group not present
  } else if (stratify_by_group == "y" && !("group" %in% colnames(dat))) {
    print("No group variable present in your dataset but group stratification selected. Proceeding without stratifying.... ")


    # ------------------------------------------------------------------------------------------------------------------- #

    # create plot

    plot <-

      ggplot(dat, aes(x = x_axis_labels, y = y_axis_labels)) +

      # Represent the correlation coefficients using colors
      geom_tile(aes(fill = correlation_coefficient), color = "white", width = 1) +



      # Add text labels for correlation coefficients and p-values
      geom_text(aes(label = box_label),
                size = 2.5, color = "black", show.legend = FALSE,
                fontface = ifelse(dat$pvalue < 0.05, "bold", "plain"), # this sets labels that're sig to bold
                alpha = ifelse(dat$pvalue < 0.05, 1, Transparency_level /100), # this sets labels that are not sig to slightly transparent (alpha = 0.55) increase 0.55 to make less transparent
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
    return(plot)

    # ------------------------------------------------------------------------------------------------------------------- #



  }
}
