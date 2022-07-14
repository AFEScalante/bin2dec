# Transform function
binary2dec <- function(binary) {
  stopifnot(is_binary(binary))
  if(class(binary) != "numeric") binary <- as.numeric(binary)

  # Get digits from number
  binary_digits <- get_all_digit(binary)
  n_digits <- length(binary_digits)

  # Iterate over digit * 2 ^ n
  decimal <- purrr::map2(binary_digits, seq_len(n_digits), function(digit, n) {
    return(digit * 2 ^ (n_digits - n))
  }) |> purrr::flatten_dbl() |> sum()

  decimal

}

get_digit <- function(x, d) {
  stopifnot(d > 0)
  (x %% 10 ^ d) %/% (10 ^ (d - 1))
}

get_all_digit <- function(x) {
  get_digit_x <- function(d) get_digit(x, d)
  purrr::map_dbl(nchar(x):1, get_digit_x)
}

is_binary <- function(binary) {
  as.character(binary) |>
    strsplit("") |>
    unlist() |>
    unique() %in% c("0", "1") |>
    all()
}
