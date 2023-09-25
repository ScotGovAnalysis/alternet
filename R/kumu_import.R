#' Imports a Kumu json-formatted model as a network, converting to a standardised R format
#'
#' @param filepath The filepath of the Kumu json file
#' @param scaling Factor to scale the coordinates down by

#' @return A named list with a tibble containing information about nodes, and a tibble containing information about edges
#' @examples import_from_kumu_json("inst/extdata/example_network.json", scaling = 2)
#' @export
import_from_kumu_json = function(filepath, scaling = 2) {
  json_data = jsonlite::fromJSON(filepath) # read the data into a list

  nodes = json_data$elements %>% # select the "elements" (or nodes)
    tidyr::unnest(cols = c(attributes)) %>% # tidy the format of the columns
    dplyr::rename(element = `_id`, refno = id) %>%
    dplyr::left_join(json_data$maps$elements[[1]] %>% #join with map layout information
                       tidyr::unnest(cols = any_of(c("position", "style"))),
                     by = "element") %>%
    dplyr::rename(name = element,
                  type = `element type`,
                  id = `_id`) %>%
    # rescale coordinates to keep layout nice
    dplyr::mutate(x = x / scaling,
                  y = y / scaling,
                  description = as.character(NA),
                  tags = as.character(NA))

  # handling styles is something to tackle in future
  # styles = nodes %>%
  #   dplyr::distinct(type, font_colour, font_weight) # identify distinct groups by style

  nodes = nodes %>%
    dplyr::select(-any_of(c("pinned", "fontColor", "fontWeight")))

  edges = json_data$connections %>% # select the "connections" (or edges)
    tidyr::unnest(cols = c(attributes)) %>% # tidy the format of the columns
    dplyr::rename(connection = `_id`, refno = id) %>%
    dplyr::left_join(tibble::as_tibble(json_data$maps$connections[[1]]), #join with map layout information
                     by = "connection") %>%
    dplyr::select(name = connection,
                  refno,
                  polarity = `connection type`,
                  from,
                  to,
                  id = `_id`) %>%
    dplyr::mutate(polarity = polarity %>%
                    dplyr::recode(`+` = "positive", `-` = "negative"), # format polarity as "positive" and "negative" instead of "+" and "-"
                  curvature = as.double(NA), # handling curvature is something to tackle in future
                  description = as.character(NA),
                  weight = 1)

  list(nodes = nodes, edges = edges) # return the node, edge and style information
}
