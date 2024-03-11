#' Graficar la vida laboral de un NSS
#'
#' @param data Cuenta individual con variables `CVE_NSS`, `FEC_MOV_INI`,
#'             `FEC_MOV_FIN`, `CVE_PATRON`, `SAL_BAS`
#' @param NSS caracter. Número de seguridad social
#'
#' @return A [ggplot2::ggplot()] class element.
#' @export
plot_intervals <- function(data, NSS) {


  data |>
    dplyr::filter(CVE_NSS == NSS) |>
    tibble::rowid_to_column() |>
    ggplot2::ggplot(ggplot2::aes(y = SAL_BAS, color = CVE_PATRON)) +

    ggplot2::geom_point(
      aes(x = FEC_MOV_INI)
    ) +
    ggplot2::geom_point(aes(FEC_MOV_FIN)) +
    ggplot2::geom_linerange(
      ggplot2::aes(xmax = FEC_MOV_FIN, xmin = FEC_MOV_INI)
    ) +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Movimientos afiliatorios",
      subtitle = stringr::str_c("NSS: ", NSS)
    ) +
    ggplot2::theme_minimal()
}
