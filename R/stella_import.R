#' Imports a Stella xml-formatted CLD as a network, converting to a standardised R format
#'
#' @param filepath The filepath of the Stella xml file
#' @param translation Number to translate the coordinates down by
#' @param model_type Specify "cld" to read from a CLD model with variables as nodes or "sf" to read from a stock and flow model with modules as nodes

#' @return A named list with a tibble containing information about nodes, and a tibble containing information about edges
#' @examples import_from_stella_xml("inst/extdata/example_network.stmx",translation = c(200,0))
#' @export
import_from_stella_xml = function(filepath, translation = c(200,0), model_type = "cld") {
  xml_data = xml2::read_xml(filepath) # read the data into an xml document

  nodes = xml_data %>%
    get_node_info_stella(model_type) %>%
    dplyr::mutate(name = stringr::str_c("elem-", refno), # create a unique id for the node, based on the refno
                  id = stringr::str_c("node-", refno)) %>%
    # rescale coordinates to keep layout nice
    dplyr::mutate(x = x - translation[1],
                  y = y - translation[2]) %>%
    dplyr::mutate(description = as.character(NA),
                  tags = as.character(NA))

  node_styles = nodes %>%
    dplyr::mutate(type = type %>% dplyr::na_if("")) %>%
    dplyr::distinct(type, .keep_all = TRUE) %>% # identify distinct groups by style
    dplyr::select(type, font_colour, font_weight)

  nodes = nodes %>%
    dplyr::select(name, refno, label, type, id, x, y, description, tags)

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

#' Extracts node information from Stella xml (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file
#' @param model_type Specify "cld" to read from a CLD model with variables as nodes or "sf" to read from a stock and flow model with modules as nodes

#' @return A tibble containing information about nodes
#' @examples xml2::read_xml("example_network.stmx") %>% get_node_info_stella()
get_node_info_stella = function(raw_xml, model_type) {
  if(model_type == "cld") {
    node_indicator = ".//d1:view/d1:aux"
    type_variable = "font_colour"
  }
  if(model_type == "sf") {
    node_indicator = ".//d1:view/d1:module"
    type_variable = "background"
  }

  nodes_xml = xml2::xml_find_all(raw_xml, node_indicator) # Navigate to the node elements

  nodes = tibble::tibble(label = xml2::xml_attr(nodes_xml, "name") %>% clean_string(),
                         x = xml2::xml_attr(nodes_xml, "x") %>% as.double(),
                         y = xml2::xml_attr(nodes_xml, "y") %>% as.double(),
                         font_colour = xml2::xml_attr(nodes_xml, "font_color"),
                         font_weight = xml2::xml_attr(nodes_xml, "font_weight"),
                         background = xml2::xml_attr(nodes_xml, "background")) %>%
    dplyr::mutate(refno = seq_along(label),
                  type = !!rlang::sym(type_variable))

  nodes
}

#' Extracts edge information from Stella xml CLD map (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file

#' @return A tibble containing information about edges
#' @examples xml2::read_xml("example_network.stmx") %>% get_edge_info_stella()
get_edge_info_stella = function(raw_xml) {
  edges_xml = xml2::xml_find_all(raw_xml, ".//d1:view/d1:connector") # Navigate to the edge elements

  tibble::tibble(from = xml2::xml_child(edges_xml, "d1:from") %>% xml2::xml_text() %>% clean_string(),
                 to = xml2::xml_child(edges_xml, "d1:to") %>% xml2::xml_text() %>% clean_string(),
                 polarity = xml2::xml_attr(edges_xml, "polarity"))
}

clean_string = function(string) {
  string %>%
    stringr::str_remove_all("\"") %>%
    stringr::str_replace_all("\\\\n", " ") %>%
    stringr::str_replace_all("_", " ") %>%
    stringr::str_squish()
}
