#' Calcula los días de trabajo para un movimiento afiliatorio.
#'
#' @param interval un vector de longitud `n` de objetos clase [lubridate::interval()]
#' @param start Alternativamente, la fecha de inicio del movimiento afiliatorio.
#' @param end Fecha de fin del movimiento afiliatorio.
#'
#'
#' @return Un vector longitud `n` de clase `numeric` que corresponde a los días
#'         trabajados en cada movimiento afiliatorio.
#' @export
working_days <- function(interval, start = NULL, end = NULL) {

  if(is.null(interval)) {

    interval <- lubridate::interval(start, end)
  }

  ((interval |>
      lubridate::int_length()) / (60 * 60 * 24)) + 1

}


#' Calcula densidad de cotización sobre un periodo base
#'
#' @param data Cuenta individual con variables `CVE_NSS`, `FEC_MOV_INI`, `FEC_MOV_FIN`
#' @param start Inicio del periodo base
#' @param end Fin del periodo base
#'
#' @return Una [tibble::tibble()] con una variable adicional que calcula la densidad
#'         de cotización aportada por cada movimiento afiliatorio.
#' @export
compute_density <- function(data, start, end) {

  original_vars <- names(data)

  over <- lubridate::interval(start, end)

  data |>
    dplyr::mutate(
      work_int = lubridate::interval(FEC_MOV_INI, FEC_MOV_FIN),
      work_intercept = lubridate::intersect(over, work_int),

      start = lubridate::int_start(work_intercept),
      end = lubridate::int_end(work_intercept)
    )  |>
    dplyr::mutate(
      ## Density
      start = dplyr::case_when(
        start <= lag(end) ~ lag(end) + lubridate::days(1),
        T ~ start
      ),
      .by = CVE_NSS
    ) |>
    dplyr::mutate(
      dens_work = lubridate::interval(start, end),
      density_days = working_days(dens_work),
      year_days = working_days(interest_period),
      density = density_days / year_days) |>
    dplyr::select(c(any_of(original_vars), density))


}
