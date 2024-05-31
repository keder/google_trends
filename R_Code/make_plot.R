make_compare_plots <- function(x_vect_list, y_vect_list, style = NULL) {
    number_of_plots <- length(y_vect_list)
    if (is.null(style)) {
        style <- list(
            line_colors = c("red", "orange", "yellow", "green", "skyblue", "blue", "violet"),
            legend_inset = c(0.05, 0.05),
            legend_align = "topleft",
            x_label = "X",
            y_label = "Y"
        )
    }
    y_copy <- as.vector(unlist(y_vect_list))
    y_copy[is.na(y_copy)] <- 0
    xlim <- c(min(unlist(x_vect_list)), max(unlist(x_vect_list)))
    ylim <- c(min(y_copy), max(y_copy))
    if (TRUE %in% is.na(ylim)) {
        print(ylim)
        print(y_copy)
    }
    plot(
        unlist(x_vect_list[1]),
        unlist(y_vect_list[1]),
        xlim = xlim, ylim = ylim,
        type = "l",
        col = unlist(style$line_colors)[1],
        xlab = style$x_label,
        ylab = style$y_label,
    )
    for (i in 2:number_of_plots) {
        lines(unlist(x_vect_list[i]), unlist(y_vect_list[i]), col = unlist(style$line_colors)[i])
    }
    legend(
        x = style$legend_align,
        inset = style$legend_inset,
        legend = names(y_vect_list),
        col = "black",
        fill = style$line_colors[1:number_of_plots],
        pt.cex = c(4, 2),
        cex = 1.85
    )
}

make_heatmap <- function(df) {
    rownames(df) <- gsub("_", " ", df[, 1])
    rownames(df) <- gsub("sars-cov-2", "SARS-CoV-2", rownames(df))
    pheatmap(
        df[, -1],
        color = colorRampPalette(heatmap_palette)(100), # Define color palette
        breaks = seq(-1.0, 1.0, length.out = 101),
        fontsize_row = 8, # Adjust row font size
        fontsize_col = 8, # Adjust column font size
        cluster_rows = FALSE, # Cluster rows
        cluster_cols = FALSE, # Cluster columns
        display_numbers = TRUE,
        fontsize_number = 6,
        angle_col = 45,
        legend = TRUE,
        # cex = .25,
        # gpar(fontface="bold"),
    )
}

save_heatmap_tables <- function(df, file_name) {
    pdf(paste0(file_name, ".pdf"), height = 6, width = 13)
    par(mar = c(5.5, 5.1, 5.1, 2.1))
    make_heatmap(df)
    dev.off()
}

make_side_by_side_plots <- function(rate_type, trend, rate, style) {
    rate_name <- str_to_title(gsub("_", " ", rate_type))
    plot(
        x = trend$ds,
        y = trend$data,
        col = "#ec6c77",
        lwd = 5,
        pch = 19,
        type = "p",
        main = style$main,
        ylim = c(
            min(trend$data, rate$data),
            max(trend$data, rate$data)
        ),
        xlab = "",
        ylab = "Value (Standardized)",
        xaxt = "n",
        yaxt = "n",
        cex = 1,
        cex.axis = 1.55,
        cex.lab = 1.55,
        cex.main = 1.55,
        cex.sub = 2
    )
    lines(
        x = rate$ds,
        y = rate$data,
        col = "#92a1ba",
        lwd = 5,
        pch = 15,
        type = "p",
        cex = 1.15
    )
    lines(
        x = trend$ds,
        y = trend$data_smoothed,
        col = "#BF0A30",
        lwd = 5,
        type = "l"
    )
    lines(
        x = rate$ds,
        y = rate$data_smoothed,
        col = "#002868",
        lwd = 5,
        type = "l"
    )

    legend(
        x = style$legend_position,
        inset = c(0.05, 0.05),
        legend = c(paste0(rate_name, " Data"), "Search Data", paste0(rate_name, " Smoother"), "Search Smoother"),
        col = "black",
        fill = c("#92a1ba", "#ec6c77", "#002868", "#BF0A30"),
        pt.cex = 4,
        cex = 1.2
    )

    initial_value <- as.integer(min(trend$ds, rate$ds))
    final_value <- as.integer(max(trend$ds, rate$ds))
    number_of_value <- final_value - initial_value

    x_tlab <- seq(from = initial_value, to = final_value, by = trunc(number_of_value / 15))
    x_lablist <- format(as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m")
    # axis(1, at = seq(0, length(trend$ds), by = 1))
    axis(1, at = x_tlab, labels = FALSE)
    text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 5)

    y_min_value <- round(min(rate$data, trend$data))
    y_max_value <- round(max(rate$data, trend$data))
    y_tlab <- round(seq(from = y_min_value, to = y_max_value, by = (y_max_value - y_min_value) / 5))
    y_lablist <- as.character(round(y_tlab, digits = 4))
    axis(2, at = y_tlab, labels = y_lablist, cex.axis = 1.1)
}

add_letter_label <- function(letter) {
    par(xpd = NA)

    di <- dev.size("in")
    x <- grconvertX(c(0, di[1]), from = "in", to = "user")
    y <- grconvertY(c(0, di[2]), from = "in", to = "user")

    fig <- par("fig")
    x <- x[1] + (x[2] - x[1]) * fig[1:2]
    y <- y[1] + (y[2] - y[1]) * fig[3:4]

    txt <- letter
    x <- x[1] + strwidth(txt, cex = 4) * 6 / 5
    y <- y[2] - strheight(txt, cex = 4) * 4 / 5
    text(x, y, txt, cex = 4)
}

save_side_by_side_plots <- function(rate_type, trend, rate, file_name) {
    pdf(paste0(file_name, ".pdf"), height = 30, width = 20)
    par(mar = c(7.1, 5.1, 3, 2.1))
    rate_name <- str_to_title(gsub("_", " ", rate_type))
    style <- list(main = paste0(rate_name, " vs Google Trend"))
    make_side_by_side_plots(rate_type, trend, rate, style)

    dev.off()
}

save_compare_plots <- function(x_vect_list, y_vect_list, file_name, style = NULL) {
    pdf(file_name, height = 10, width = 10)
    # Definign the number of plots
    par(mar = c(5.5, 5.1, 5.1, 2.1))
    make_compare_plots(x_vect_list, y_vect_list, style)
    dev.off()
}

make_prediction_plot <- function(time_points, prediction_length, data, prediction, additional = NULL) {
    png("output.png", width = 800, height = 600)
    # pred_time_points <- time_points[(length(time_points) - prediction_length + 1):length(time_points)]
    # Plot the values against indexes
    plot(time_points,
        data,
        col = "blue",
        lty = 3,
        type = "l",
        ylim = range(c(
            prediction$lower,
            prediction$upper,
            prediction$median,
            data
        )),
        xlab = "Time",
        ylab = "Incidence"
    )

    # lines(pred_time_points, prediction$median, col = "black", lty = 2, type = "l")
    # lines(pred_time_points, prediction$upper, col = "red", lty = 1, type = "l")
    # lines(pred_time_points, prediction$lower, type = "l", col = "red", lty = 1)

    lines(time_points, prediction$median, col = "black", lty = 2, type = "l")
    lines(time_points, prediction$upper, col = "red", lty = 1, type = "l")
    lines(time_points, prediction$lower, type = "l", col = "red", lty = 1)

    if (!is.null(additional)) {
        lines(time_points, additional, col = "yellow", lty = 3, type = "l")
    }

    initial_value <- as.integer(min(time_points))
    final_value <- as.integer(max(time_points))
    number_of_value <- final_value - initial_value
    x_tlab <- seq(from = initial_value, to = final_value, by = trunc(number_of_value / 15))
    x_lablist <- format(as.Date(x_tlab, origin = "1970-01-01"), "%Y-%m")
    axis(1, at = x_tlab, labels = FALSE)
    text(x = x_tlab, y = par()$usr[3] - 0.05 * (par()$usr[4] - par()$usr[3]), labels = x_lablist, srt = 45, adj = 1, xpd = TRUE, cex.axis = 5)

    legend("topleft", legend = c("Prediction Bounds", "Prediction Median", "Real Data"), col = c("red", "black", "blue"), lty = 1:3)
    dev.off()
}

make_bootstrap_plot <- function(time_points, data, fit, original_fit) {
    png("boot.png", width = 800, height = 600)
    # Plot the values against indexes
    plot(time_points,
        data,
        col = "blue",
        lty = 3,
        type = "l",
        ylim = range(c(
            fit$lower,
            fit$upper,
            fit$median,
            data
        )),
        xlab = "Time",
        ylab = "Incidence"
    )

    lines(time_points, fit$median, col = "black", lty = 2, type = "l")
    lines(time_points, fit$upper, col = "red", lty = 1, type = "l")
    lines(time_points, fit$lower, type = "l", col = "red", lty = 1)

    lines(time_points, original_fit, col = "yellow", lty = 3, type = "l")

    axis(1, at = seq(0, length(time_points), by = 1))

    legend("topleft", legend = c("Prediction Bounds", "Prediction Median", "Real Data", "Original Prediction"), col = c("red", "black", "blue", "yellow"), lty = 1:4)
    dev.off()
}
