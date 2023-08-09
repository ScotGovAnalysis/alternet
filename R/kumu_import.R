import_from_kumu_json = function(filepath) {
  json_data = jsonlite::fromJSON(filepath)

  nodes = json_data$elements %>%
    tidyr::unnest(cols = c(attributes)) %>%
    dplyr::rename(element = `_id`, refno = id) %>%
    dplyr::left_join(json_data$maps$elements[[1]] %>%
                       tidyr::unnest(cols = c(position, style)),
                     by = "element") %>%
    dplyr::rename(name = element,
                  type = `element type`,
                  id = `_id`,
                  font_colour = fontColor,
                  font_weight = fontWeight)

  styles = nodes %>%
    dplyr::distinct(type, font_colour, font_weight)

  nodes = nodes %>%
    dplyr::select(-pinned, -font_colour, -font_weight)

  edges = json_data$connections %>%
    tidyr::unnest(cols = c(attributes)) %>%
    dplyr::rename(connection = `_id`, refno = id) %>%
    dplyr::left_join(tibble::as_tibble(json_data$maps$connections[[1]]),
                     by = "connection") %>%
    dplyr::select(name = connection,
                  refno,
                  polarity = `connection type`,
                  from,
                  to,
                  id = `_id`,
                  curvature) %>%
    dplyr::mutate(polarity = polarity %>%
                    dplyr::recode(`+` = "positive", `-` = "negative"))

  list(nodes = nodes, edges = edges, styles = styles)
}
