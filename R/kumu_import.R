#' Imports a Kumu json-formatted model as a network, converting to a standardised R format
#'
#' @param filepath The filepath of the Kumu json file
#' @param scaling Factor to scale the coordinates down by

#' @return A named list with a tibble containing information about nodes, and a tibble containing information about edges
#' @examples import_from_kumu_json("example_network.json", scaling = 2)
#' @export
import_from_kumu_json = function(filepath, scaling = 2) {
  json_data = jsonlite::fromJSON(filepath) # read the data into a list

  nodes = json_data$elements %>% # select the "elements" (or nodes)
    tidyr::unnest(cols = c(attributes)) %>% # tidy the format of the columns
    dplyr::rename(element = `_id`, refno = id) %>%
    dplyr::left_join(json_data$maps$elements[[1]] %>% #join with map layout information
                       tidyr::unnest(cols = c(position, style)),
                     by = "element") %>%
    dplyr::rename(name = element,
                  type = `element type`,
                  id = `_id`,
                  font_colour = fontColor,
                  font_weight = fontWeight) %>%
    # rescale coordinates to keep layout nice
    dplyr::mutate(x = multiply_chr(x, 1/scaling),
                  y = multiply_chr(y, 1/scaling))

  styles = nodes %>%
    dplyr::distinct(type, font_colour, font_weight) # identify distinct groups by style

  nodes = nodes %>%
    dplyr::select(-pinned, -font_colour, -font_weight)

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
                  id = `_id`,
                  curvature) %>%
    dplyr::mutate(polarity = polarity %>%
                    dplyr::recode(`+` = "positive", `-` = "negative")) # format polarity as "positive" and "negative" instead of "+" and "-"

  list(nodes = nodes, edges = edges, styles = styles) # return the node, edge and style information
}
