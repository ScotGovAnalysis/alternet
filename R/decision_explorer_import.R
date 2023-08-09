import_from_decision_explorer_xml = function(filepath) {
  xml_data = xml2::read_xml(filepath)

  nodes = xml_data %>%
    get_node_info() %>%
    dplyr::mutate(name = stringr::str_c("elem-", refno)) %>%
    dplyr::mutate(x = multiply_chr(x, 1/2.5),
                  y = multiply_chr(y, -1/2.5))

  edges = xml_data %>%
    get_edge_info() %>%
    dplyr::left_join(nodes, by = c("from" = "refno")) %>%
    dplyr::select(polarity, from = name, to) %>%
    dplyr::left_join(nodes, by = c("to" = "refno")) %>%
    dplyr::select(polarity, from, to = name) %>%
    dplyr::mutate(refno = 1:nrow(.),
                  name = stringr::str_c("conn-", refno))

  list(nodes = nodes, edges = edges)
}



#' @NoRd
get_node_info = function(raw_xml) {
  nodes_xml = xml2::xml_find_all(raw_xml, ".//concept") # Navigate to the node elements

  layout_xml = xml2::xml_find_all(raw_xml, ".//position") # Navigate to the position elements

  nodes = tibble::tibble(refno = xml2::xml_attr(nodes_xml, "id"),
                         label = xml2::xml_text(nodes_xml),
                         type = xml2::xml_attr(nodes_xml, "style"))

  layout = tibble::tibble(x = xml2::xml_attr(layout_xml, "x"),
                          y = xml2::xml_attr(layout_xml, "y"),
                          refno = xml2::xml_attr(layout_xml, "concept"))

  nodes %>%
    dplyr::left_join(layout, by = "refno")
}

#' @NoRd
get_edge_info = function(raw_xml) {
  edges_xml = xml2::xml_find_all(raw_xml, ".//link") # Navigate to the edge elements

  tibble::tibble(from = xml2::xml_attr(edges_xml, "linkfrom"),
                 to = xml2::xml_attr(edges_xml, "linkto"),
                 polarity = xml2::xml_attr(edges_xml, "sign"))
}
