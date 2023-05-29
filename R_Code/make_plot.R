make_compare_plots <- function(x_vect_list, y_vect_list, style = NULL) {
    number_of_plots <- length(y_vect_list)
    if (is.null(style)) {
        style <- list(
            c("red", "orange", "yellow", "green", "skyblue", "blue", "violet"),
            c(0.05, 0.05),
            "topleft",
            "X",
            "Y"
        )
        names(style) <- c("line_colors", "legend_inset", "legend_align", "x_label", "y_label")
    }
    y_copy <- as.vector(unlist(y_vect_list))
    y_copy[is.na(y_copy)] <- 0
    xlim <- c(min(unlist(x_vect_list)), max(unlist(x_vect_list)))
    ylim <- c(min(y_copy), max(y_copy))
    if (TRUE %in% is.na(ylim))
    {
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

save_compare_plots <- function(x_vect_list, y_vect_list, file_name, style = NULL) {
    pdf(file_name, height = 10, width = 10)
    # Definign the number of plots
    par(mar = c(5.5, 5.1, 5.1, 2.1))
    make_compare_plots(x_vect_list, y_vect_list, style)
    dev.off()
}
