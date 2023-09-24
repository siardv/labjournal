require(scholar)

get_scholar_id_fix <- function (last_name = "", first_name = "", affiliation = NA)
{
  if (!any(nzchar(c(first_name, last_name))))
    stop("At least one of first and last name must be specified!")
  site <- getOption("scholar_site")
  url <- paste0(site, "/citations?view_op=search_authors&mauthors=",
                first_name, "+", last_name, "&hl=en&oi=ao")
  page <- get_scholar_resp(url)
  if (is.null(page))
    return(NA)
  aa <- httr::content(page, as = "text")
  # added by Bas Hofstra: bugfix for IDs that have a dash ("-")
  ids <- substring(aa, regexpr(";user=", aa))
  ids <- substr(ids, 1, 19) # error prone, but unsure how to solve otherwise
  # if (nchar(stringr::str_extract_all(string = aa, pattern = ";user=[[:alnum:]]+[[:punct:]]")[[1]][1]) < 18) {
  #   ids <- stringr::str_extract_all(string = aa, pattern = ";user=[[:alnum:]]+[[:punct:]]+[[:alnum:]]+[[:punct:]]")
  # } else {
  #   ids <- stringr::str_extract_all(string = aa, pattern = ";user=[[:alnum:]]+[[:punct:]]")
  # }
  if (length(unlist(ids)) == 0) {
    message("No Scholar ID found.")
    return(NA)
  }
  ids <- ids %>% unlist %>% gsub(";user=|[[:punct:]]$", "",
                                 .) %>% unique
  if (length(ids) > 1) {
    profiles <- lapply(ids, scholar::get_profile)
    if (is.na(affiliation)) {
      x_profile <- profiles[[1]]
      warning("Selecting first out of ", length(profiles),
              " candidate matches.")
    }
    else {
      which_profile <- sapply(profiles, function(x) {
        stringr::str_count(string = x$affiliation, pattern = stringr::coll(affiliation,
                                                                           ignore_case = TRUE))
      })
      if (all(which_profile == 0)) {
        warning("No researcher found at the indicated affiliation.")
        return(NA)
      }
      else {
        x_profile <- profiles[[which(which_profile !=
                                       0)]]
      }
    }
  }
  else {
    x_profile <- scholar::get_profile(id = ids)
  }
  return(x_profile$id)
}
