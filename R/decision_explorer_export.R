#' Exports a network as an xml-formatted Decision Explorer model
#'
#' @param network The network to be exported
#' @param filepath The filepath to save the xml file to. If NULL, the xml document is returned without being saved
#' @param scaling Factor to scale the coordinates up by

#' @return An xml document formatted to be imported into Decision Explorer
#' @examples export_to_decision_explorer(example_network, file = "example_network.mdx", scaling = 5)
#' @export
export_to_decision_explorer = function(network, filepath = NULL, scaling = 5) {
  nodes = network$nodes %>%
    # rescale coordinates to keep layout nice
    dplyr::mutate(x = multiply_chr(x, scaling),
                  y = multiply_chr(y, -scaling)) # also reverse y direction as decision explorer has origin in bottom left rather than top left

  edges = network$edges %>%
    # switch the from and to columns in edges to be based on refno rather than name
    recode_by_dict(c("from", "to"), "name", "refno", nodes)

  node_styles = network$node_styles %>% hex_to_percent()

  # create xml document
  xml_doc = xml2::xml_new_root("model",
                               .version = '1.0',
                               .encoding = 'ISO-8859-1')

  xml_doc %>% xml2::xml_add_child("info")

  xml_doc %>% xml2::xml_add_child("conceptstyles")
  # add each node as a concept
  for(i in 1:nrow(nodes)) {
    xml_doc %>%
      xml2::xml_child("conceptstyles") %>%
      xml2::xml_add_child("conceptstyle",
                          bold = node_styles$font_weight[i] %>% tidyr::replace_na("0") %>% dplyr::recode(bold = "1"),
                          name = node_styles$type[i] %>% tidyr::replace_na("standard"),
                          redpercent = node_styles$red[i],
                          greenpercent = node_styles$green[i],
                          bluepercent = node_styles$blue[i])
  }

  xml_doc %>% xml2::xml_add_child("concepts")
  # add each node as a concept
  for(i in 1:nrow(nodes)) {
    xml_doc %>%
      xml2::xml_child("concepts") %>%
      xml2::xml_add_child("concept",
                          nodes$label[i],
                          id = nodes$refno[i],
                          style = nodes$type[i] %>% tidyr::replace_na("standard"))
  }

  xml_doc %>% xml2::xml_add_child("links")
  # add each edge as a link
  for(i in 1:nrow(edges)) {
    xml_doc %>%
      xml2::xml_child("links") %>%
      xml2::xml_add_child("link",
                          sign = edges$polarity[i],
                          linkto = edges$to[i],
                          linkfrom = edges$from[i])
  }

  xml_doc %>% xml2::xml_add_child("views") %>% xml2::xml_add_child("mapview")
  # add the position of each node to the mapview
  for(i in 1:nrow(nodes)) {
    xml_doc %>%
      xml2::xml_child("views") %>%
      xml2::xml_child("mapview") %>%
      xml2::xml_add_child("position",
                          x = nodes$x[i],
                          y = nodes$y[i],
                          concept = nodes$refno[i])
  }

  if(! filepath %>% is.null()) {
    xml2::write_xml(xml_doc, filepath)
  }
  xml_doc
}

hex_to_percent = function(node_styles) {
  node_styles %>%
    dplyr::mutate(rgb = font_colour %>%
                    col2rgb() %>%
                    t() %>%
                    tibble::as_tibble()) %>%
    tidyr::unnest(rgb) %>%
    dplyr::mutate_at(c("red", "green", "blue"), ~round(./2.55, 1))
}
