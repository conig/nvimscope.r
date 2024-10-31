#' nvimclip
#'
#' @param objname Name of the object to be processed. Default is "mtcars".
#' @return JSON representation of the processed object, copied to the clipboard.
#' @export

nvimclip <- function(objname = "mtcars") {
  if (!dir.exists("tmp/rmdclip")) {
    dir.create("tmp/rmdclip", recursive = TRUE)
  }
  contents <- as.list(get(objname))
  contents_names <- names(contents)
  out <- lapply(seq_along(contents), function(i) {
    name <- contents_names[i]
    if (is.null(name)) name <- i
    x <- contents[[i]]
    data.frame(name = name, contents = process_contents(x, name))
  }) |>
    data.table::rbindlist() |>
    jsonlite::toJSON(pretty = TRUE, escape_unicode = FALSE) |>
    writeLines("tmp/rmdclip/menu.json")
}

#' process_contents
#'
#' @param x Object to be processed.
#' @param name Name of the object.
#' @return Processed content as a string.
#' @export

process_contents <- function(x, name) {
  if (is(x, "numeric")) {
    return(process_numeric(x, name))
  }
  if (is(x, "character")) {
    return(process_character(x, name))
  }

  if (is(x, "factor")) {
    return(process_character(x, name))
  }

  if (is(x, "logical")) {
    return(process_character(x, name))
  }

  process_else(x, name)
}

#' process_numeric
#'
#' @param x Numeric object to be processed.
#' @param name Name of the object.
#' @return Processed numeric content as a string.

process_numeric <- function(x, name) {
  if (length(x) < 2) {
    return(capture.output(print(x)) |> paste(collapse = "\n"))
  }
  if (is.null(name)) name <- ""
  mean_x <- mean(x, na.rm = TRUE) |> round(2)
  median_x <- median(x, na.rm = TRUE) |> round(2)
  sd_x <- sd(x, na.rm = TRUE) |> round(2)
  IQR_x <- IQR(x, na.rm = TRUE) |> round(2)

  range_x <- range(x, na.rm = TRUE) |> paste(collapse = ", ")
  density_x <- capture.output(txtplot::txtdensity(x, width = 45)) |>
    paste(collapse = "\n")

  head_x <- head(x, 5) |> capture.output() |> paste(collapse = "\n")
  tail_x <- tail(x, 5) |> capture.output() |> paste(collapse = "\n")
  kurtosis_x <- psych::kurtosi(x) |> round(2)
  skewness_x <- psych::skew(x) |> round(2)

  is_missing_x <- sum(is.na(x))
  missing_pc <- (is_missing_x / length(x)) * 100
  missing_pc <- missing_pc |> round(2)

  len_x <- length(x)

  glue::glue("
  Name: `{name}`

  head: {head_x}
  tail: {tail_x}

  Observations: {len_x}
  Unique values: {length(unique(x))}
  Missing: {is_missing_x} ({missing_pc}%)

  Range: [{range_x}]
  Mean: {mean_x} (sd: {sd_x})
  Median: {median_x} (IQR: {IQR_x})

  Kurtosis: {kurtosis_x}
  Skewness: {skewness_x}

—————————————————————————————————————————————

  {density_x}

             ")
}

#' process_character
#'
#' @param x Character object to be processed.
#' @param name Name of the object.
#' @return Processed character content as a string.

process_character <- function(x, name) {
  x <- as.character(x)
  if (length(x) < 2) {
    return(capture.output(print(x)) |> paste(collapse = "\n"))
  }
  if (is.null(name)) name <- ""

  head_x <- head(x, 5) |> capture.output() |> paste(collapse = "\n")
  tail_x <- tail(x, 5) |> capture.output() |> paste(collapse = "\n")

  is_missing_x <- sum(is.na(x))
  missing_pc <- (is_missing_x / length(x)) * 100
  missing_pc <- missing_pc |> round(2)

  len_x <- length(x)

  values <- table(x)
  most_common <- data.frame(values)
  most_common <- most_common[order(-most_common$Freq), ]
  if (nrow(most_common) > 5) {
    most_common <- most_common[1:5, ]
  }

  most_common_string <- glue::glue("`{most_common[,1]}`: {most_common[,2]}") |>
    paste(collapse = "\n")


  glue::glue("
  Name: `{name}`

  head: {head_x}
  tail: {tail_x}

  Observations: {len_x}
  Unique values: {length(unique(x))}
  Missing: {is_missing_x} ({missing_pc}%)

  Most common values:
  {most_common_string}
             ")
}

#' process_else
#'
#' @param x Object to be processed.
#' @param name Name of the object.
#' @return Processed content as a string for objects that are not numeric, character, factor, or logical.

process_else <- function(x, name) {
  print_contents <- capture.output(print(x))

  if (length(print_contents) > 40) {
    print_contents <- c(print_contents[1:40], "...")
  }

  paste(print_contents, collapse = "\n")
}
