# ===================================================================================
# Dace Apšvalka, October, 2020, www.dcdace.net
# ===================================================================================
#
# 2x2 Repeated measures within-subject ANOVA results and plots
#
# INPUT DATA:
#      - data: dataset in long-format
#      - columns: list of columns of interest.
#                 Must contain sID, DV, Fc1, Fc2
#                 For example:
#                     columns <- list(sID = "Participant_ID",
#                                     DV  = "Subjective_Valence",
#                                     Fc1 = "Item_Category",
#                                     Fc2 = "Emotion_Condition")

#      - param: list of parameters
#                    Must contain y.label, Fc1.label, Fc2.label, title, cat.color, errorbar
#                    For example:
#                        parameters <- list(
#                                        y.label    = "Subjective Valence",
#                                        Fc1.label  = "Item",
#                                        Fc2.label  = "Emotion",
#                                        cat.color  = c('#DF4A56', '#5284a8'),
#                                        errorbar   = "ci"  # can be either sd, se, or ci
#                                           )
#                        parameters$title <- sprintf('%s x %s interaction', parameters$Fc2.label, parameters$Fc1.label)
#
# OUTPUT DATA: a list of result items
#          outliers               - results of identify_outliers function
#          normality              - results of shapiro_test
#          plot.assumption.checks - boxplots and QQ plots in one plot
#          dataSummary            - Within-Subject descriptive summary of all 4 levels
#          res.anova              - ANOVA results of aov function
#          res.txt.anova          - Formatted ANOVA results
#          res.txt.Fc1            - Formatted Factor 1 Main effect
#          res.txt.Fc2            - Formatted Factor 2 Main effect  
#          pwc1                   - Pairwise comparisons grouped by Factor 1
#          pwc2                   - Pairwise comparisons grouped by Factor 2
#          plot.anova             - the results plot
#
# ----------------------------------------------------------------------
# REQUIRED LIBRARIES
# ----------------------------------------------------------------------
# Define a function that checks and installs missing packages
# Define a function that checks and installs missing packages
install_packages <- function(packages) {
  lapply(packages,
    FUN = function(x)(
        if (length(find.package(x, quiet = TRUE)) == 0) {
          install.packages(x, dependencies = TRUE)
        }))
}
# A list of required packages
packages_required <- c(
  "Rmisc", # for getting summary data frame
  "ggplot2", # for plotting
  "ggpubr", # for Quantile-Quantile plot
  "cowplot", # for adding plots together and setting different widths
  "gtools", # for converting pvalues to stars
  "rstatix" # for pairwise ttests
  )
# Install the missing required packages
install_packages(packages_required)
# Load the packages:
invisible(lapply(packages_required, library, character.only = TRUE))


# ===================================================================================
rm_2by2_anova <- function(data, columns, param) {
  # ----------------------------------------------------------------------
  # Results list to store results
  # ----------------------------------------------------------------------
  results <- list()

  # ----------------------------------------------------------------------
  # Prepare the dataset
  # ----------------------------------------------------------------------
  # Subset the dataset to only the columns of interest
  # Because dplyr::select function clashes with MASS::select need to specify to use dplyr:: select
  data_subset <- data %>% dplyr::select(all_of(unlist(columns)))

  # Average same Factor/Level values for each subject if there are several.
  # Will use this data frame df for the rest of the results and plots
  df <- ddply(data_subset,
              .(sID, Fc1, Fc2),
              summarise,
              meanDV = mean(DV, na.rm = TRUE))

  # sID, Fc1 and Fc2 must be factors
  df$sID <- as.factor(df$sID)
  df$Fc1 <- as.factor(df$Fc1)
  df$Fc2 <- as.factor(df$Fc2)

  # ----------------------------------------------------------------------
  # CHECK ASSUMPTIONS
  # ----------------------------------------------------------------------

  cat("================================================================\n\n")
  cat("CHECK ASSUMPTIONS")
  cat("\n\n================================================================")

  # Check for outliers
  # ----------------------------------------------------------------------

  cat("\n Outliers\n\n")

  # Identify if there are extreme outliers
  results$outliers <- df %>%
    group_by(Fc1, Fc2) %>%
    identify_outliers(meanDV)

  # Display
  print(results$outliers)

  # Outlier result text
  outlier.result.txt <- ifelse(
    any(results$outliers$is.extreme == TRUE),
    "Data has extreme outliers!",
    "There are no extreme outliers."
  )
  cat(sprintf("\n%s", outlier.result.txt))
  cat("\n\n-----------------------------------------------")

  # Boxplot title color. It will be red if there are ouliers!
  outlier.title.color <-
    ifelse(any(results$outliers$is.extreme == TRUE),
           "red",
           "black")

  # Create a boxplot
  plot.box <- ggboxplot(
    df,
    x = "Fc2",
    y = "meanDV",
    color = "Fc1",
    title = outlier.result.txt,
    ylab = param$y.label,
    xlab = param$Fc2.label
  ) +
    guides(color = guide_legend(param$Fc1.label)) +
    scale_color_manual(values = param$cat.color) +
    theme(plot.title = element_text(hjust = 0.5, color = outlier.title.color))


  # Check normality
  # ----------------------------------------------------------------------
  cat("\n Normality \n\n")

  # Shapiro-Wilk's test (should be > .05)
  results$normality <- df %>%
    group_by(Fc1, Fc2) %>%
    shapiro_test(meanDV)

  # Display
  print(results$normality)

  # Normality result text
  normality.result.txt <- ifelse(
    any(results$normality$p < 0.05),
    "Not all levels are normaly distributed!",
    "All levels are normally distributed."
  )

  cat(sprintf("\n%s", normality.result.txt))

  # QQ plot title color. It will be red if not all levels meet normality!
  normality.title.color <- ifelse(any(results$normality$p < 0.05),
                                  "red",
                                  "black")

  # Create Quantile-Quantile plots
  plot.qq <-
    ggqqplot(df, "meanDV", ggtheme = theme_minimal(), title = normality.result.txt) +
    facet_grid(Fc1 ~ Fc2, labeller = "label_both") +
    theme(plot.title = element_text(hjust = 0.5, color = normality.title.color))

  # ----------------------------------------------------------------------
  # PLOT Boxplot and QQ
  # ----------------------------------------------------------------------
  options(
    repr.plot.width = 10,
    repr.plot.height = 5,
    repr.plot.res = 200
  ) # change plot size

  # Add result texts as titles for each plot
  results$plot.assumption.checks <- plot_grid(plot.box,
                                              plot.qq,
                                              nrow = 1,
                                              rel_widths = c(1 / 4, 3 / 4))

  # ----------------------------------------------------------------------
  # Within-Subject descriptive summary
  # ----------------------------------------------------------------------
  # Get the within-subject summary. Will need this to plot within-subject error bars.
  # Using summarySEwithin function from the Rmisc package

  results$dataSummary <- summarySEwithin(
    df,
    measurevar = "meanDV",
    withinvars = c("Fc1", "Fc2"),
    idvar = "sID",
    na.rm = TRUE,
    conf.interval = 0.95
  )
  # See how it looks
  cat("\n\n================================================================\n\n")
  cat("SUMMARY DESCRIPTIVES")
  cat("\n\n================================================================\n")

  print(results$dataSummary)

  # ----------------------------------------------------------------------
  # Factor 1 X Factor 2 interaction, ANOVA
  # ----------------------------------------------------------------------
  cat("\n================================================================\n\n")
  cat("2x2 INTERACTION RESULTS")
  cat("\n\n================================================================\n")

  # Within-subject repeated measures ANOVA
  results$res.anova <-
    aov(meanDV ~ Fc1 * Fc2 + Error(sID / (Fc1 * Fc2)),
        data = df)
  #Summary results
  summary.anova <- summary(results$res.anova)
  print(summary.anova)

  # save p-val, will need it to put on the plot
  res.anova.pval <-
    summary.anova[["Error: sID:Fc1:Fc2"]][[1]][["Pr(>F)"]][1]

  # Display result string
  sign.anova <-
    ifelse(res.anova.pval < 0.05, "Significant", "No significant")

  results$res.txt.anova <- sprintf(
    "\n%s %s: F(%d,%d) = %.2f, p = %.7f\n",
    sign.anova,
    param$title,
    summary.anova[["Error: sID:Fc1:Fc2"]][[1]][["Df"]][1],
    summary.anova[["Error: sID:Fc1:Fc2"]][[1]][["Df"]][2],
    summary.anova[["Error: sID:Fc1:Fc2"]][[1]][["F value"]][1],
    res.anova.pval
  )

  cat(results$res.txt.anova)

  # ----------------------------------------------------------------------
  # Simple Main Effects of both factors
  # ----------------------------------------------------------------------
  cat("\n================================================================\n\n")
  cat("Main Effects")
  cat("\n\n================================================================\n")

  # Main Effect of Factor 1

  # save p-val, will need it to put on the plot
  res.Fc1.pval <-
    summary.anova[["Error: sID:Fc1"]][[1]][["Pr(>F)"]][1]

  # Display result string
  sign.Fc1 <-
    ifelse(res.Fc1.pval < 0.05, "Significant", "No significant")

  results$res.txt.Fc1 <- sprintf(
    "Main effect of %s: F(%d,%d) = %.2f, p = %.7f\n",
    param$Fc1.label,
    summary.anova[["Error: sID:Fc1"]][[1]][["Df"]][1],
    summary.anova[["Error: sID:Fc1"]][[1]][["Df"]][2],
    summary.anova[["Error: sID:Fc1"]][[1]][["F value"]][1],
    summary.anova[["Error: sID:Fc1"]][[1]][["Pr(>F)"]][1]
  )

  # display the result sentence

  cat(results$res.txt.Fc1)


  cat("\n\n-----------------------------------------------\n")

  # Main Effect of Factor 2
  res.Fc2.pval <-
    summary.anova[["Error: sID:Fc2"]][[1]][["Pr(>F)"]][1]

  # Display result string
  sign.Fc2 <-
    ifelse(res.Fc2.pval < 0.05, "Significant", "No significant")

  results$res.txt.Fc2 <- sprintf(
    "Main effect of %s: F(%d,%d) = %.2f, p = %.7f\n",
    param$Fc2.label,
    summary.anova[["Error: sID:Fc2"]][[1]][["Df"]][1],
    summary.anova[["Error: sID:Fc2"]][[1]][["Df"]][2],
    summary.anova[["Error: sID:Fc2"]][[1]][["F value"]][1],
    summary.anova[["Error: sID:Fc2"]][[1]][["Pr(>F)"]][1]
  )

  # display the result sentence

  cat(results$res.txt.Fc2)


  cat("\n================================================================\n\n")
  cat("Post-hoc")
  cat("\n\n================================================================\n\n")

  # Pairwise comparisons grouped by Factor 1
  cat("Pairwise comparisons grouped by Factor 1\n")

  results$pwc1 <- df %>%
    group_by(Fc1) %>%
    pairwise_t_test(meanDV ~ Fc2, paired = TRUE, p.adjust.method = "bonferroni") 
  print(results$pwc1)
  
  results$dblock <- df %>%
    split(.,.$Fc1) %>%
    map(.,function(x){
      return(x %>%
               ungroup() %>%
               cohens_d(meanDV~Fc2,paired = T))
    })
  print(results$dblock)  
  results$dwordtype <- df %>%
    split(.,.$Fc2) %>%
    map(.,function(x){
      return(x %>%
               ungroup() %>%
               cohens_d(meanDV~Fc1,paired = T))
    })
print(results$dwordtype)
  
  
  cat("\n\n-----------------------------------------------\n\n")

  # Pairwise comparisons grouped by Factor 2
  cat("Pairwise comparisons grouped by Factor 2\n")

  results$pwc2 <- df %>%
    group_by(Fc2) %>%
    pairwise_t_test(meanDV ~ Fc1, paired = TRUE, p.adjust.method = "bonferroni")
  print(results$pwc2)

  # ----------------------------------------------------------------------
  # P-values for displaying significance stars
  # ----------------------------------------------------------------------
  # 2x2 interaction result
  p.anova <-
    ifelse(res.anova.pval < 0.05, stars.pval(res.anova.pval), "n.s.")
  # Main effects
  p.mainFc1 <-
    ifelse(results$one.way1$p < 0.05,
           stars.pval(results$one.way1$p),
           "n.s.")
  p.mainFc2 <-
    ifelse(results$one.way2$p < 0.05,
           stars.pval(results$one.way2$p),
           "n.s.")

  # plot main title and factor labels (with added significance stars)
  plot.title <- sprintf("%s %s", param$title, p.anova)
  Fc1.title <- sprintf("%s %s:", param$Fc1.label, p.mainFc1)
  Fc2.title <- sprintf("%s %s", param$Fc2.label, p.mainFc2)

  # ----------------------------------------------------------------------
  # Plot the interaction results
  # ----------------------------------------------------------------------
  results$plot.anova <-
    ggplot(data = df,
           aes(
             x = Fc2,
             y = meanDV,
             group = Fc1,
             color = Fc1
           )) +
  # connect the level means with a line
  stat_summary(fun = mean, geom = "line", size = 1) +

  # add mean point and within-subject CI (taken from results$dataSummary created above)
  geom_pointrange(
      data = results$dataSummary,
      aes(
        y = meanDV,
        ymin = meanDV - .data[[param$errorbar]],
        ymax = meanDV + .data[[param$errorbar]],
        color = Fc1
      ),
      size = 1,
      stroke = 0.5,
      show.legend = FALSE
    ) +

  # change the default colors to the defined ones
  scale_color_manual(values = param$cat.color) +
    scale_fill_manual(values = param$cat.color) +

  # add labels and the title
  guides(color = guide_legend(Fc1.title)) +
    labs(x = Fc2.title, y = param$y.label) +
    ggtitle(plot.title) +


  # make it nicer
  theme_minimal() +
    theme(
      text = element_text(size = 16),
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold"
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )

  # return the results list
  return(results)

}
