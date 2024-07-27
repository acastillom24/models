#' univariate_categorical, grafic explore to categorical variable
#'
#' @param data a data frame
#' @param feature name from variable
#' @param target name from target
#' @param labels categories that take target
#' @param colors colors that take target
#' @param ylog scale log the feature variable
#' @param label_rotation rotation the y label
#' @param horizontal_layout show horizontal or vertical
#' @param bar_desc order bar plot
#'
#' @importFrom dplyr .data
#' @importFrom ggplot2 ggplot aes geom_bar geom_text scale_fill_manual labs theme_minimal theme element_text scale_y_log10
#' @importFrom rlang :=
#'
#' @return a plot from explore categorical variable
#' @export
#'
#' @examples
#' example_data <- data.frame(
#'   Sex = c("Male", "Female", "Male", "Female", "Male"),
#'   Survived = c(0, 1, 0, 1, 0)
#' )
#' univariate_categorical(data = example_data, feature = "Sex", target = "Survived")
univariate_categorical <- function(data,
                                   feature,
                                   target = "target",
                                   labels = c("No", "Yes"),
                                   colors = c("#07bbc3", "#fb756e"),
                                   ylog = FALSE,
                                   label_rotation = FALSE,
                                   horizontal_layout = TRUE,
                                   bar_desc = FALSE) {
  # Validación de entrada
  if (!all(c(feature, target) %in% names(data))) {
    stop("Feature or target column not found in data")
  }
  if (length(labels) != 2 || length(colors) != 2) {
    stop("labels and colors must each have exactly 2 elements")
  }

  # Calcular el porcentaje de target=1 por categoría
  cat_perc <- data |>
    dplyr::mutate(!!dplyr::sym(target) := as.integer(!!dplyr::sym(target))) |>
    dplyr::group_by(!!dplyr::sym(target), .drop = FALSE) |>
    dplyr::summarise(target_percent = mean(!!dplyr::sym(target)) * 100, count = dplyr::n()) |>
    dplyr::arrange(dplyr::desc(.data$target_percent))

  if (bar_desc) {
    cat_perc <- cat_perc |>
      dplyr::mutate(!!dplyr::sym(target) := stats::reorder(!!dplyr::sym(target), .data$target_percent))
  }

  # Función para crear el primer gráfico
  create_plot1 <- function() {
    p <- ggplot(data, aes(x = !!dplyr::sym(feature), fill = factor(!!dplyr::sym(target)))) +
      geom_bar(position = "dodge") +
      geom_text(
        aes(label = ggplot2::after_stat(.data$count)),
        stat = "count",
        position = ggplot2::position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3,
        color = "black"
      ) +
      scale_fill_manual(name = target,
                        labels = labels,
                        values = colors) +
      labs(title = feature, x = feature, y = "Count") +
      theme_minimal() +
      theme(plot.title = element_text(
        size = 10,
        face = "bold",
        color = "Blue"
      ))

    if (ylog) {
      p <- p + scale_y_log10() + labs(y = "Count (log)")
    }
    if (label_rotation) {
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    return(p)
  }

  # Función para crear el segundo gráfico
  create_plot2 <- function() {
    p <- ggplot(cat_perc, aes(x = !!dplyr::sym(feature), y = .data$target_percent)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_text(
        aes(label = sprintf("%.1f%%", .data$target_percent)),
        vjust = -0.5,
        size = 3,
        color = "black"
      ) +
      labs(
        title = paste(feature, paste0(target, " %")),
        x = feature,
        y = paste("Percent of", target, "[%]")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          size = 15,
          face = "bold",
          color = "Blue"
        ),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)
      )

    if (label_rotation) {
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    return(p)
  }

  # Crear los gráficos
  plot1 <- create_plot1()
  plot2 <- create_plot2()

  # Organizar los gráficos
  if (horizontal_layout) {
    gridExtra::grid.arrange(plot1, plot2, ncol = 2)
  } else {
    gridExtra::grid.arrange(plot1, plot2, nrow = 2)
  }
}
