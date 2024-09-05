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


#' Recorta eventos de la Cuenta Individual para calcular la densidad de
#' cotización
#'
#' @param start Fecha de movimiento de inicio
#' @param end Fecha de movimiento de fin
#'
#' @return Un intervalo [lubridate::interval()]
#' @export
tidy_intervals <- function(start, end) {
  start <- lubridate::as_date(start)
  end   <- lubridate::as_date(end)

  last_end <- max(end)

  if (is.na(last_end))
    stop("Fecha de movimiento de fin con datos faltantes.")

  disc <- (start <= dplyr::lag(end)) |> sum(na.rm = T)

  while (disc > 0) {
    start <-
      dplyr::case_when(start <= dplyr::lag(end) ~ dplyr::lag(end) + lubridate::days(1),
                       T ~ start)
    end <-
      dplyr::case_when(end <= start ~ start + lubridate::days(1),
                       T ~ end)
    end <- dplyr::case_when(end > last_end ~ last_end,
                            T ~ end)

    disc <- (start <= dplyr::lag(end)) |> sum(na.rm = T)

  }


  lubridate::interval(start, end)


}

#' ¿Modalidad considerada trabajo?
#'
#' @param mode Un vector de modalidades
#'
#' @return Resultado lógico
#' @export
#'
#' @examples
#' a <- c("10", "31")
#' is_work_mode(a)
is_work_mode <- function(mode) {

  work_modes <-
    c(
      '10',
      '11',
      '12',
      '13',
      '14',
      '15',
      '16',
      '17',
      '18',
      '19',
      '21',
      '27',
      '28',
      '29',
      '30',
      '31',
      '34',
      '35',
      '40',
      '42',
      '43',
      '44',
      '45'
    )

  mode %in% work_modes


}
