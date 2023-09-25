#' Imports a Stella xml-formatted CLD as a network, converting to a standardised R format
#'
#' @param filepath The filepath of the Stella xml file
#' @param translation Number to translate the coordinates down by

#' @return A named list with a tibble containing information about nodes, and a tibble containing information about edges
#' @examples import_from_stella_xml("inst/extdata/example_network.stmx",translation = c(200,0))
#' @export
import_from_stella_xml = function(filepath, translation = c(200,0)) {
  xml_data = xml2::read_xml(filepath) # read the data into an xml document

  nodes = xml_data %>%
    get_node_info_stella() %>%
    dplyr::mutate(name = stringr::str_c("elem-", refno), # create a unique id for the node, based on the refno
                  id = stringr::str_c("node-", refno)) %>%
    # rescale coordinates to keep layout nice
    dplyr::mutate(x = x - translation[1],
                  y = y - translation[2],
                  type = as.character(NA)) %>%
    dplyr::select(name, refno, label, type, id, x, y, font_colour) %>%
    dplyr::mutate(description = as.character(NA),
                  tags = as.character(NA))

  node_styles = nodes %>%
    dplyr::distinct(font_colour) %>%
    dplyr::transmute(type = font_colour,
                     font_colour,
                     font_weight = NA)

  nodes = nodes %>% dplyr::select(-font_colour)

  edges = xml_data %>%
    get_edge_info_stella() %>%
    recode_by_dict(c("from", "to"), "label", "name", nodes) %>% # specifying from and to nodes by name instead of label
    dplyr::mutate(refno = 1:nrow(.),# assign a unique refno for the edge
                  name = stringr::str_c("conn-", refno),
                  id = stringr::str_c("edge-", refno), # create a unique id for the edge, based on the refno
                  curvature = as.double(NA)) %>%
    dplyr::select(name, refno, polarity, from, to, id, curvature) %>%
    dplyr::mutate(polarity = polarity %>%
                    dplyr::recode(`+` = "positive", `-` = "negative"),
                  description = as.character(NA),
                  weight = 1)

  list(nodes = nodes, edges = edges, node_styles = node_styles) # return the node, edge and style information
}

#' Extracts node information from Decision Explorer xml (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file

#' @return A tibble containing information about nodes
#' @examples xml2::read_xml("example_network.mdx") %>% get_node_info_stella()
get_node_info_stella = function(raw_xml) {
  nodes_xml = xml2::xml_find_all(raw_xml, ".//view/aux") # Navigate to the node elements

  nodes = tibble::tibble(label = xml2::xml_attr(nodes_xml, "name"),
                         x = xml2::xml_attr(nodes_xml, "x") %>% as.double(),
                         y = xml2::xml_attr(nodes_xml, "y") %>% as.double(),
                         font_colour = xml2::xml_attr(nodes_xml, "font_color")) %>%
    dplyr::mutate(refno = seq_along(label))

  nodes
}

#' Extracts edge information from Decision Explorer xml (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file

#' @return A tibble containing information about edges
#' @examples xml2::read_xml("example_network.mdx") %>% get_edge_info_stella()
get_edge_info_stella = function(raw_xml) {
  edges_xml = xml2::xml_find_all(raw_xml, ".//connector") # Navigate to the edge elements

  tibble::tibble(from = xml2::xml_child(edges_xml, "from") %>% xml2::xml_text(),
                 to = xml2::xml_child(edges_xml, "to") %>% xml2::xml_text(),
                 polarity = xml2::xml_attr(edges_xml, "polarity"))
}
