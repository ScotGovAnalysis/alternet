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
    get_edge_info_stella(model_type) %>%
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
  if(model_type == "cld") nodes = get_node_info_stella_cld(raw_xml)
  if(model_type == "sf") nodes = get_node_info_stella_sf(raw_xml)

  nodes
}

#' Extracts edge information from Stella xml CLD map (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file
#' @param model_type Specify "cld" to read from a CLD model with variables as nodes or "sf" to read from a stock and flow model with modules as nodes

#' @return A tibble containing information about edges
#' @examples xml2::read_xml("example_network.stmx") %>% get_edge_info_stella()
get_edge_info_stella = function(raw_xml, model_type) {
  if(model_type == "cld") edges = get_edge_info_stella_cld(raw_xml)
  if(model_type == "sf") edges = get_edge_info_stella_sf(raw_xml)

  edges
}

#' Extracts node information from Stella xml CLD map (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file

#' @return A tibble containing information about nodes
#' @examples xml2::read_xml("example_network.stmx") %>% get_node_info_stella_cld()
get_node_info_stella_cld = function(raw_xml) {
  nodes_xml = xml2::xml_find_all(raw_xml, ".//view/aux") # Navigate to the node elements

  nodes = tibble::tibble(label = xml2::xml_attr(nodes_xml, "name"),
                         x = xml2::xml_attr(nodes_xml, "x") %>% as.double(),
                         y = xml2::xml_attr(nodes_xml, "y") %>% as.double(),
                         font_colour = xml2::xml_attr(nodes_xml, "font_color")) %>%
    dplyr::mutate(refno = seq_along(label),
                  type = font_colour,
                  font_weight = NA)

  nodes
}

#' Extracts edge information from Stella xml CLD map (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file

#' @return A tibble containing information about edges
#' @examples xml2::read_xml("example_network.stmx") %>% get_edge_info_stella_cld()
get_edge_info_stella_cld = function(raw_xml) {
  edges_xml = xml2::xml_find_all(raw_xml, ".//connector") # Navigate to the edge elements

  tibble::tibble(from = xml2::xml_child(edges_xml, "from") %>% xml2::xml_text(),
                 to = xml2::xml_child(edges_xml, "to") %>% xml2::xml_text(),
                 polarity = xml2::xml_attr(edges_xml, "polarity"))
}

#' Extracts node information from Stella xml stock and flow map (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file

#' @return A tibble containing information about nodes
#' @examples xml2::read_xml("example_network.stmx") %>% get_node_info_stella_sf()
get_node_info_stella_sf = function(raw_xml) {
  nodes = xml2::xml_find_all(raw_xml, ".//d1:module[@x]") # Navigate to the node elements

  x = xml2::xml_attr(nodes, "x") # Get the x position of the nodes
  y = xml2::xml_attr(nodes, "y") # Get the x position of the nodes
  name = xml2::xml_attr(nodes, "name") # Get the names of the nodes
  name = stringr::str_replace_all(name, "\\\\n", " ") # Remove new lines from the names

  font_colour = xml2::xml_attr(nodes, "font_color") # Get the x position of the nodes
  font_weight = xml2::xml_attr(nodes, "font_weight") # Get the x position of the nodes
  background = xml2::xml_attr(nodes, "background") # Get the x position of the nodes

  data.frame(label = name,
             x = 1.5*as.integer(x),
             y = 1.5*as.integer(y),
             type = background,
             font_colour = background, # at some point, will change to account for font colour and background separately
             font_weight = font_weight) %>%
    dplyr::mutate(refno = seq_along(label))
}

#' Extracts edge information from Stella xml stock and flow map (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file

#' @return A tibble containing information about edges
#' @examples xml2::read_xml("example_network.stmx") %>% get_edge_info_stella_sf()
get_edge_info_stella_sf = function(raw_xml) {
  data.frame(from = get_edge_node_sf(raw_xml, "from"), # Get the "from" nodes of each edge
             to = get_edge_node_sf(raw_xml, "to"), # Get the "to" nodes of each edge
             polarity = get_edge_polarity_sf(raw_xml)) # Get the polarity of each edge
}

#' Extracts edge information from Stella xml stock and flow map (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file
#' @param end whether the node sought is the "from" or "to" end

#' @return A vector containing the from or to nodes
#' @examples xml2::read_xml("example_network.stmx") %>% get_edge_node_sf("from")
get_edge_node_sf = function(raw_xml, end) {
  nodes = xml2::xml_find_all(raw_xml, paste0(".//d1:", end)) # Navigate to the from or to elements
  nodes = xml2::xml_text(nodes) # Convert the elements to text
  nodes = stringr::str_replace_all(nodes, "_", " ") # Convert underscores to spaces
  stringr::str_remove_all(nodes, "\"") # Strip out extraneous quotation marks
}

#' Extracts edge information from Stella xml stock and flow map (as created by xml2::read_xml())
#'
#' @param raw_xml xml document read from the model file

#' @return A vector containing the polarities
#' @examples xml2::read_xml("example_network.stmx") %>% get_edge_polarity_sf()
get_edge_polarity_sf = function(raw_xml) {
  edges = xml2::xml_find_all(raw_xml, ".//d1:connector[@uid]") # Get all the edges
  xml2::xml_attr(edges, "polarity") # Return the polarity values
}
