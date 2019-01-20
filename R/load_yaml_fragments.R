#' Load all YAML fragments from a file
#'
#' These function extracts all YAML fragments from a file and then
#' calls [yaml::yaml.load()] to parse them. It then returns a list
#' of the parsed fragments.
#'
#' @inheritParams extract_yaml_fragments
#'
#' @return A list of objects.
#' @examples
#' yum::load_yaml_fragments(text=c("---", "First: YAML fragment", "---",
#'                                 "Outside of YAML",
#'                                 "---", "Second: YAML fragment", "---",
#'                                 "Also outside of YAML"));
#'
#' @export
load_yaml_fragments <- function(file,
                                text,
                                select=".*",
                                delimiterRegEx = "^---$",
                                ignoreOddDelimiters = FALSE,
                                encoding="UTF-8",
                                silent=TRUE) {

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("To parse YAML content, the \"yaml\" package is required. ",
         "Please install it using `install.packages('yaml');`.",
         call. = FALSE);
  }

  if (!missing(file)) {
    yamlLineSets <- extract_yaml_fragments(file=file,
                                           delimiterRegEx=delimiterRegEx,
                                           ignoreOddDelimiters=ignoreOddDelimiters,
                                           silent=TRUE);
  } else if (!missing(text)) {
    yamlLineSets <- extract_yaml_fragments(text=text,
                                           delimiterRegEx=delimiterRegEx,
                                           ignoreOddDelimiters=ignoreOddDelimiters,
                                           silent=TRUE);
  } else {
    stop("Provide either a `file` or a `text` to scan!");
  }

  rawSpecs <- lapply(yamlLineSets,
                     yaml::yaml.load);

  if (!silent) {
    if (!missing(file)) {
      cat("Loaded ",
          length(rawSpecs),
          " YAML fragments from file '",
          file,
          "'.\n", sep="");
    } else if (!missing(text) && !silent) {
      cat("Loaded ",
          length(rawSpecs),
          " YAML fragments from the supplied `text` argument.\n",
          sep="");
    }
  }

  specNames <-
    lapply(rawSpecs,
           names);

  if (length(select) > 0) {
    if (!silent) {
      cat("Applying the selection ",
          ifelse(length(select)==1,
                 "criterion",
                 "criteria"),
          " specified in the `select` argument (specifically,",
          vecTxtQ(select),
          ").\n", sep="");
    }
    combinedSelect <-
      paste0(select, collapse="|");
    rawSpecs <-
      lapply(rawSpecs,
             function(spec) {
               selectedElements <-
                 grep(combinedSelect,
                      names(spec),
                      perl=TRUE);
               return(spec[selectedElements]);
             });

    if (!silent) {
      cat("Selected ",
          sum(unlist(lapply(rawSpecs, length))),
          " YAML fragments.\n",
          sep="");
    }
  } else {
    if (!silent) {
      cat("No selection criteria were specified, so ",
          "returning all objects.");
    }
  }

  rawSpecs <-
    rawSpecs[unlist(lapply(rawSpecs,
                           length)) > 0];

  class(rawSpecs) <-
    c("yumFromFile", "list");

  return(rawSpecs);

}
