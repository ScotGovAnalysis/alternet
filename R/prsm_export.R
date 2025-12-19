#' Exports a network as an xlsx-formatted PRSM model
#'
#' @param network The network to be exported
#' @param filepath The filepath to save the xlsx file to.
#' @param scaling Factor to scale the coordinates up by

#' @examples export_to_prsm(example_network, file = "example_network.xlsx", scaling = 2)
#' @export
export_to_prsm = function(network, filepath = NULL, scaling = 2) {
  nodes = network$nodes %>%
    # rescale coordinates to keep layout nice
    dplyr::mutate(x = x*scaling, y = y*scaling)

  edges = network$edges %>%
    recode_by_dict(c("from", "to"), "name", "label", nodes)

  factors = nodes %>%
    dplyr::transmute(x, y, label,
                     groupLabel = type %>% tidyr::replace_na("Factor"),
                     shape = "box",
                     creator = NA, createdTime = NA, modifier = NA, modifiedTime = NA,
                     style = groupLabel %>% factor() %>% as.integer())

  links = edges %>%
    dplyr::transmute(from, to,
                     groupLabel = polarity,
                     edgeHidden = FALSE,
                     creator = NA, createdTime = NA,
                     style = groupLabel %>% factor() %>% as.integer())

  readr::write_csv(factors, stringr::str_c(filepath, "_factors", ".csv"))
  readr::write_csv(links, stringr::str_c(filepath, "_links", ".csv"))
}
