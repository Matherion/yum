#' Load all YAML fragments from all files in a directory
#'
#' These function extracts all YAML fragments from all files in a
#' directory returning a list of character vectors containing the
#' extracted fragments.
#'
#' These function extracts all YAML fragments from all files in a
#' directory and then calls [yaml::yaml.load()] to parse them. It
#' then returns a list where each element is a list with the parsed
#' fragments in a file.
#'
#' @param path The path containing the files.
#' @param recursive Whether to also process subdirectories (`TRUE`)
#' or not (`FALSE`).
#' @param fileRegexes A vector of regular expressions to match the files
#' against: only files matching one or more regular expressions in this
#' vector are processed. The default regex (`^[^\.]+.*$`) matches all
#' files except those that start with a period (`.`).
#' @inheritParams extract_yaml_dir
#'
#' @return A list of lists of objects.
#' @examples
#' yum::load_yaml_fragments(text=c("---", "First: YAML fragment", "---",
#'                                 "Outside of YAML",
#'                                 "---", "Second: YAML fragment", "---",
#'                                 "Also outside of YAML"));
#'
#' @export
load_yaml_dir <- function(path,
                          recursive = TRUE,
                          fileRegexes = c("^[^\\.]+.*$"),
                          select=".*",
                          delimiterRegEx = "^---$",
                          ignoreOddDelimiters = FALSE,
                          encoding="UTF-8",
                          silent=TRUE) {

  if (!dir.exists(path)) {
    stop("Directory '",
         path,
         "' does not exist!");
  }

  fileList <-
    list.files(path=path,
               pattern=fileRegexes,
               recursive=recursive,
               full.names=TRUE);

  res <- lapply(fileList,
                load_yaml_fragments,
                select=select,
                delimiterRegEx = delimiterRegEx,
                ignoreOddDelimiters = ignoreOddDelimiters,
                encoding = encoding,
                silent=silent);

  names(res) <-
    fileList;

  class(res) <-
    c("yumFromDir", "list");

  return(res);

}
