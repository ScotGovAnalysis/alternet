export_to_decision_explorer = function(network, file = NULL) {
  nodes = network$nodes %>%
    dplyr::mutate(x = multiply_chr(x, 2.5),
                  y = multiply_chr(y, -2.5))

  edges = network$edges %>%
    dplyr::select(-name, -refno) %>%
    dplyr::left_join(nodes, by = c("from" = "name")) %>%
    dplyr::select(polarity, from = refno, to) %>%
    dplyr::left_join(nodes, by = c("to" = "name")) %>%
    dplyr::select(polarity, from, to = refno)
  styles = network$styles

  xml_doc = xml2::xml_new_root("model",
                               .version = '1.0',
                               .encoding = 'ISO-8859-1')

  xml_doc %>% xml2::xml_add_child("info")

  xml_doc %>% xml2::xml_add_child("concepts")
  for(i in 1:nrow(nodes)) {
    xml_doc %>%
      xml2::xml_child("concepts") %>%
      xml2::xml_add_child("concept",
                          nodes$label[i],
                          id = nodes$refno[i],
                          style = nodes$type[i])
  }

  xml_doc %>% xml2::xml_add_child("links")
  for(i in 1:nrow(edges)) {
    xml_doc %>%
      xml2::xml_child("links") %>%
      xml2::xml_add_child("link",
                          sign = edges$polarity[i],
                          linkto = edges$to[i],
                          linkfrom = edges$from[i])
  }

  xml_doc %>% xml2::xml_add_child("views") %>% xml2::xml_add_child("mapview")
  for(i in 1:nrow(nodes)) {
    xml_doc %>%
      xml2::xml_child("views") %>%
      xml2::xml_child("mapview") %>%
      xml2::xml_add_child("position",
                          x = nodes$x[i],
                          y = nodes$y[i],
                          concept = nodes$refno[i])
  }

  if(! file %>% is.null()) {
    xml2::write_xml(xml_doc, file)
  }
  xml_doc
}
