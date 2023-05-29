
chen_envlp <- function(z, b = 100, v ) {
  z$residual |> 
    as.data.frame() |> 
    ggplot2::ggplot(ggplot2::aes(sample = v)) +
    qqplotr::geom_qq_band(
      alpha = 0.5, fill = "white", col = "black", B = b,
      bandType = "boot"
    ) +
    qqplotr::stat_qq_point(size = 1.2) +
    ggplot2::scale_fill_discrete("Bandtype") +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
}
