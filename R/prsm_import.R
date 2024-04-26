#' Imports a PRSM excel-formatted model as a network, converting to a standardised R format
#'
#' @param filepath The filepath of the PRSM Excel file
#' @param scaling Factor to scale the coordinates down by

#' @return A named list with a tibble containing information about nodes, and a tibble containing information about edges
#' @examples import_from_prsm_xlsx("inst/extdata/example_network.xlsx", scaling = 2)
#' @export
import_from_prsm_xlsx = function(filepath, scaling = 2) {
  nodes = readxl::read_excel(filepath, sheet = "Factors") %>% # select the "Factors" (or nodes)
    dplyr::transmute(name = paste0("elem-", 1:nrow(.)),
                     refno = 1:nrow(.),
                     label,
                     type = groupLabel,
                     id = paste0("node-", refno),
                     x = x / scaling, # rescale coordinates to keep layout nice
                     y = y / scaling,
                     description = Note,
                     tags = as.character(level),
                     font_colour = "black",
                     font_weight = as.character(NA))

  # handling styles is something to tackle in future
  node_styles = nodes %>%
    dplyr::mutate(type = type %>% dplyr::na_if("")) %>%
    dplyr::distinct(type, .keep_all = TRUE) %>% # identify distinct groups by style
    dplyr::select(type, font_colour, font_weight)

  nodes = nodes %>%
    dplyr::select(-font_colour, -font_weight)

  edges = readxl::read_excel(filepath, sheet = "Links") %>% # select the "Links" (or edges)
    dplyr::transmute(name = paste0("conn-", 1:nrow(.)),
                     refno = 1:nrow(.),
                     polarity = groupLabel %>%
                       stringr::str_to_lower(), # format polarity as "positive" and "negative" instead of "+" and "-"
                     from,
                     to,
                     id = paste0("edge-", refno),
                     curvature = as.double(NA), # handling curvature is something to tackle in future
                     description = as.character(NA),
                     weight = 1) %>%
    recode_by_dict(c("from", "to"), "label", "name", nodes) # specifying from and to nodes by name instead of label

  list(nodes = nodes, edges = edges, node_styles = node_styles) # return the node, edge and style information
}
