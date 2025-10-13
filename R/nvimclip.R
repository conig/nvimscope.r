#' nvimclip
#'
#' @param objname Name of the object to be processed. Default is "mtcars".
#' @param name_fun Function to extract names from the object. Default is names.
#' @return JSON representation of the processed object, copied to the clipboard.
#'
#' @examples
#' \donttest{
#'   nvimscope.r::nvimclip(mtcars)
#' }
#' @export
nvimclip <- function(obj) {
  if (!dir.exists("/tmp/nvim-rmdclip")) {
    dir.create("/tmp/nvim-rmdclip", recursive = TRUE)
  }
  s4 <- isS4(obj)
  isSampled <- FALSE
  if (is.data.frame(obj)) {
    if (nrow(obj) > 1e6) {
      original_nrow <- nrow(obj)
      obj <- obj[sample(nrow(obj), 1e5), ]
      isSampled <- TRUE
      attr(isSampled, "original_nrow") <- original_nrow
    }
  }
  # Attempt to convert target object to a list
  contents <- tryCatch(
    {
      if (s4) {
        s4_to_list(obj)
      } else {
        as.list(obj)
      }
    },
    # if not possible, record error in json.
    error = function(e) {
      writeLines("", "/tmp/nvim-rmdclip/error.json")
      stop("Could not find object.")
    }
  )
  s4 <- ifelse(s4, "true", "false")
  # is object is null, record error in json.
  if (is.null(obj)) {
    writeLines("", "/tmp/nvim-rmdclip/error.json")
    stop("Could not find object.")
  }

  content_names <- names(contents)
  processed_data <- lapply(seq_along(contents), function(idx) {
    x <- contents[[idx]]
    name <- if (!is.null(content_names)) content_names[[idx]] else NULL
    debug <- FALSE
    if (is.null(name)) {
      name <- idx
    }
    name <- as.character(name)
    data.frame(
      name = name,
      contents = as.character(process_contents(
        x,
        name,
        debug = debug,
        isSampled = isSampled
      )),
      stringsAsFactors = FALSE
    )
  }) |>
    data.table::rbindlist()
  # Write processed data to json file
  list(s4 = s4, contents = processed_data) |>
    jsonlite::toJSON(pretty = TRUE, escape_unicode = FALSE) |>
    writeLines("/tmp/nvim-rmdclip/menu.json")
}

#' process_contents
#'
#' @param x Object to be processed.
#' @param name Name of the object.
#' @return Processed content as a string.
#' @export

process_contents <- function(x, name, debug = FALSE, isSampled = FALSE) {
  sample_string <- ifelse(
    isSampled,
    glue::glue(
      "(sampled for speed, actual obs: {attr(isSampled, 'original_nrow')}"
    ),
    ""
  )

  if (is(x, "numeric")) {
    return(process_numeric(x, name, sample_string))
  }
  if (is(x, "character")) {
    return(process_character(x, name, sample_string))
  }

  if (is(x, "factor")) {
    return(process_character(x, name, sample_string))
  }

  if (is(x, "logical")) {
    return(process_character(x, name, sample_string))
  }

  if (is(x, "ggplot")) {
    return(process_ggplot(x, name))
  }

  suppressMessages(process_else(x, name, debug = debug, sample_string))
}

#' process_numeric
#'
#' @param x Numeric object to be processed.
#' @param name Name of the object.
# @param sample_string Logical indicating if the data was sampled.
#' @return Processed numeric content as a string.

process_numeric <- function(x, name, sample_string) {
  if (length(x) < 2) {
    return(capture.output(print(x)) |> paste(collapse = "\n"))
  }
  if (is.null(name)) {
    name <- ""
  }
  mean_x <- mean(x, na.rm = TRUE) |> round(2)
  median_x <- median(x, na.rm = TRUE) |> round(2)
  sd_x <- sd(x, na.rm = TRUE) |> round(2)
  IQR_x <- IQR(x, na.rm = TRUE) |> round(2)

  range_x <- range(x, na.rm = TRUE) |>
    round(2) |>
    paste(collapse = ", ")
  density_x <- tryCatch(
    {
      if (length(x) > 1000000) {
        x_dens <- sample(x, 1000000)
        msg <- "*Plot based on a random sample\n  of 1,000,000 observations."
      } else {
        x_dens <- x
        msg <- ""
      }

      capture.output(txtplot::txtdensity(
        na.omit(x_dens),
        width = 35,
        height = 15
      )) |>
        paste(collapse = "\n")
    },
    error = function(e) {
      return("Could not generate density plot.")
    }
  )
  head_x <- head(x, 5) |>
    capture.output() |>
    paste(collapse = "\n")
  tail_x <- tail(x, 5) |>
    capture.output() |>
    paste(collapse = "\n")
  kurtosis_x <- psych::kurtosi(x) |> round(2)
  skewness_x <- psych::skew(x) |> round(2)

  is_missing_x <- sum(is.na(x))
  missing_pc <- (is_missing_x / length(x)) * 100
  missing_pc <- missing_pc |> round(2)

  class_x <- class(x) |> paste(collapse = ", ")

  len_x <- length(x)

  glue::glue(
    "



  Name: `{name}` <{class_x}>

—————————————————————————————————————————————

  head: {head_x}
  tail: {tail_x}

  Observations:  {len_x} {sample_string}
  Unique values: {length(unique(x))}
  Missing:       {is_missing_x} ({missing_pc}%)

  Range:  [{range_x}]
  Mean:   {mean_x} (sd: {sd_x})
  Median: {median_x} (IQR: {IQR_x})

  Kurtosis: {kurtosis_x}
  Skewness: {skewness_x}

—————————————————————————————————————————————

  {density_x}
  {msg}

             "
  )
}

#' process_character
#'
#' @param x Character object to be processed.
#' @param name Name of the object.
#' @return Processed character content as a string.

process_character <- function(x, name, sample_string) {
  if (length(x) < 2) {
    return(capture.output(print(x)) |> paste(collapse = "\n"))
  }
  if (is.null(name)) {
    name <- ""
  }

  head_x <- head(x, 5) |>
    capture.output() |>
    paste(collapse = "\n")
  tail_x <- tail(x, 5) |>
    capture.output() |>
    paste(collapse = "\n")

  is_missing_x <- sum(is.na(x))
  missing_pc <- (is_missing_x / length(x)) * 100
  missing_pc <- missing_pc |> round(2)

  len_x <- length(x)

  values <- table(x)
  most_common <- data.frame(values)
  most_common <- most_common[order(-most_common$Freq), ]
  unique_n <- length(na.omit(unique(x)))
  if (unique_n > 1) {
    if (nrow(most_common) > 5) {
      most_common <- most_common[1:5, ]
    }
    most_common_string <- glue::glue(
      "`{most_common[,1]}`: {most_common[,2]}"
    ) |>
      paste(collapse = "\n  ")
  } else {
    most_common_string <- x[1]
  }

  class_x <- class(x) |> paste(collapse = ", ")

  glue::glue(
    "



  Name: `{name}` <{class_x}>

—————————————————————————————————————————————

  head: {head_x}
  tail: {tail_x}

  Observations:  {len_x} {sample_string}
  Unique values: {unique_n}
  Missing:       {is_missing_x} ({missing_pc}%)

  Most common values:
  {most_common_string}
             "
  )
}

process_ggplot <- function(x, name) {
  if (is.null(name)) {
    name <- ""
  }
  class_x <- class(x) |> paste(collapse = ", ")
  mapping <- capture.output(print(x$mapping)) |>
    paste(collapse = "\n  ")
  layers <- capture.output(print(x$layers)) |>
    paste(collapse = "\n  ")

  glue::glue(
    "



  name: `{name}` <{class_x}>

—————————————————————————————————————————————

  {mapping}

—————————————————————————————————————————————

  Layers:

  {layers}
"
  )
}

#' process_else
#'
#' @param x Object to be processed.
#' @param name Name of the object.
#' @return Processed content as a string for objects that are not numeric, character, factor, or logical.

process_else <- function(x, name, debug = FALSE, sample_string) {
  len_x <- length(x)
  if (debug) {
    browser()
  }

  if (length(x) > 40) {
    x <- x[1:40]
    msg <- "..."
  } else {
    msg <- ""
  }
  print_contents <- capture.output(print(x))

  print_contents <- paste(print_contents, collapse = "\n  ")
  class_x <- class(x) |> paste(collapse = ", ")
  glue::glue(
    "



  name: `{name}` <{class_x}>

—————————————————————————————————————————————

  length: {len_x} {sample_string}

  {print_contents}
  {msg}
"
  )
}
