#' Convert Fahrenheit to Kelvin
#'
#' Convert a temperature from Fahrenheit to Kelvin
#'
#' @param temp numeric
#'
#' @return numeric
#' @export
#' @examples
#' fahr_to_kelvin(32)
fahr_to_kelvin <- function(temp) {
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

#' Convert Kelvin to Celsius
#'
#' Convert a temperature from Kelvin to Celsius
#'
#' @param temp numeric
#'
#' @return numeric
#' @export
#' @examples
#' kelvin_to_celsius(273.15)
kelvin_to_celsius <- function(temp) {
  if(temp<0) {
    warning("kelvin cannot be negative")
    }
  else{
    celsius <- temp - 273.15
  return(celsius)
    }
}

#' Convert Celsius to Fahrenheit
#'
#' Convert a temperature from Celsius to Fahrenheit
#'
#' @param temp numeric
#'
#' @return numeric
#' @export
#' @examples
#' celsius_to_fahr(0)
celsius_to_fahr <- function(temp) {
  fahr <- (temp * (9 / 5)) + 32
  return(fahr)
}

#' Convert Fahrenheit to Celsius
#'
#' Convert a temperature from Fahrenheit to Celsius
#'
#' @param temp numeric
#'
#' @return numeric
#' @export
#' @examples
#' fahr_to_celsius(32)
fahr_to_celsius <- function(temp) {
  temp_k <- fahr_to_kelvin(temp)
  result <- kelvin_to_celsius(temp_k)
  return(result)
}

#' Convert Kelvin to Fahrenheit
#'
#' Convert a temperature from Kelvin to Fahrenheit
#'
#' @param temp numeric
#'
#' @return numeric
#' @export
#' @examples
#' kelvin_to_fahr(273.15)
kelvin_to_fahr <- function(temp) {
  if(temp<0) {
    warning("kelvin cannot be negative")
  }
  else{
  celsius <- kelvin_to_celsius(temp)
  result <- celsius_to_fahr(celsius)
  return(result)}
}

#' Convert Celsius to Kelvin
#'
#' Convert a temperature from Celsius to Kelvin
#'
#' @param temp numeric
#'
#' @return numeric
#' @export
#' @examples
#' celsius_to_kelvin(0)
celsius_to_kelvin <- function(temp) {
  fahr <- celsius_to_fahr(temp)
  result <- fahr_to_kelvin(fahr)
  return(result)
}
