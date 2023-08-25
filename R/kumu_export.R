#' Exports a network as a json-formatted Kumu model
#'
#' @param network The network to be exported
#' @param filepath The filepath to save the json file to. If NULL, the json document is returned without being saved
#' @param scaling Factor to scale the coordinates up by

#' @return A json document formatted to be imported into Kumu
#' @examples export_to_kumu(example_network, file = "example_network.json", scaling = 2)
#' @export
export_to_kumu = function(network, filepath = NULL, scaling = 2) {
  nodes = network$nodes %>%
    # rescale coordinates to keep layout nice
    dplyr::mutate(x = x*scaling,
                  y = y*scaling) # also reverse y direction as decision explorer has origin in bottom left rather than top left

  edges = network$edges %>%
    dplyr::mutate(polarity = polarity %>%
                     dplyr::recode(positive = "+", negative = "-"))

  styles = network$styles

  elements = nodes %>%
    dplyr::select(`_id` = name, id = refno, label, `element type` = type) %>%
    alternet:::nest_for_json(attributes = c(id, label, `element type`))

  connections = edges %>%
    dplyr::select(`_id` = name,
                  id = refno,
                  `connection type` = polarity,
                  from,
                  to) %>%
    alternet:::nest_for_json(attributes = c(id, `connection type`)) %>%
    dplyr::select(`_id`, attributes, from, to)

  maps = list(`_id` = "map_import",
              name = "Imported_map",
              elements = nodes %>%
                dplyr::select(`_id` = id, element = name, x, y) %>%
                alternet:::nest_for_json(position = c(x,y)) %>%
                dplyr::select(`_id`, position, element),
              connections = edges %>%
                dplyr::select(`_id` = id, connection = name))

  # create xml document
  json_doc = list(elements = elements,
                  connections = connections,
                  maps = list(maps))

  if(! filepath %>% is.null()) {
    jsonlite::write_json(json_doc, filepath, auto_unbox = TRUE)
  }

  jsonlite::toJSON(json_doc, auto_unbox = TRUE)
}
