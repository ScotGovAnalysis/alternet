#' Exports a network as an xml-formatted Stella CLD
#'
#' @param network The network to be exported
#' @param filepath The filepath to save the xml file to. If NULL, the xml document is returned without being saved
#' @param translation Number to translate the coordinate up by

#' @return An xml document formatted to be imported into Stellar
#' @examples export_to_stella(example_network, file = "example_network.stmx", translation = c(200,0))
#' @export
export_to_stella = function(network, filepath = NULL, translation = c(200,0)) {
  nodes = network$nodes %>%
    # rescale coordinates to keep layout nice
    dplyr::mutate(x = x + translation[1],
                  y = y + translation[2],
                  label = label %>% fix_labels_stella())

  edges = network$edges %>%
    dplyr::rename(uid = refno) %>%
    # switch the from and to columns in edges to be based on label rather than name
    recode_by_dict(c("from", "to"), "name", "label", nodes) %>%
    # get the start and end coordinates for the link
    dplyr::left_join(nodes %>% dplyr::select(label, x, y), by = c("from" = "label")) %>%
    dplyr::rename(start_x = x, start_y = y) %>%
    dplyr::left_join(nodes %>% dplyr::select(label, x, y), by = c("to" = "label")) %>%
    dplyr::mutate(angle = calculate_link_angle(start_x, x, start_y, y), # calculate initial angle for the connectors
                  polarity = polarity %>% dplyr::recode(positive = "+", negative = "-")) %>% # recode polarity
    dplyr::select(uid, polarity, angle, from, to)

  styles = network$styles

  # create xml document
  xml_doc = xml2::xml_new_root("xmile",
                               .version = '1.0',
                               .encoding = 'utf-8',
                               xlmns="http://docs.oasis-open.org/xmile/ns/XMILE/v1.0",
                               `xmlns:isee`="http://iseesystems.com/XMILE")

  xml_doc %>% xml2::xml_add_child("isee:prefs", layer = "cld")

  xml_doc %>% xml2::xml_add_child("model")

  xml_doc %>% xml2::xml_child("model") %>% xml2::xml_add_child("variables")
  # add each node as an aux
  for(i in 1:nrow(nodes)) {
    xml_doc %>%
      xml2::xml_child("model") %>%
      xml2::xml_child("variables") %>%
      xml2::xml_add_child("aux",
                          name = nodes$label[i])
  }

  xml_doc %>%
    xml2::xml_child("model") %>%
    xml2::xml_add_child("views") %>%
    xml2::xml_add_child("view", `isee:converter_size` = "name_only")

  # add the position of each node to the view
  for(i in 1:nrow(nodes)) {
    xml_doc %>%
      xml2::xml_child("model") %>%
      xml2::xml_child("views") %>%
      xml2::xml_child("view") %>%
      xml2::xml_add_child("aux",
                          x = nodes$x[i],
                          y = nodes$y[i],
                          name = nodes$label[i])
  }
  # add each edge as a connector
  for(i in 1:nrow(edges)) {
    xml_doc %>%
      xml2::xml_child("model") %>%
      xml2::xml_child("views") %>%
      xml2::xml_child("view") %>%
      xml2::xml_add_child("connector",
                          uid = edges$uid[i],
                          polarity = edges$polarity[i],
                          angle = edges$angle[i]) %>%
      xml2::xml_add_child("from", edges$from[i]) %>%
      xml2::xml_add_sibling("to", edges$to[i])
  }

  if(! filepath %>% is.null()) {
    xml2::write_xml(xml_doc, filepath)
  }
  xml_doc
}

fix_labels_stella = function(labels) {
  labels %>%
    stringr::str_replace_all("\\.", " ") %>% # full stops break Stella for some reason
    stringr::str_squish()
}
