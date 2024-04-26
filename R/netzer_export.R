#' Exports a network as csvs for use in NetZer
#'
#' @param network The network to be exported
#' @param filepath The filepath to save the csvs file to. Two csvs are created, inserting "_nodes" and "_edges" ahead of the filetype. If NULL, the list of nodes and edges is returned without being saved
#' @param scaling Factor to scale the coordinates up by

#' @return A list containing nodes and edges formatted for NetZer
#' @examples export_to_netzer(alternet::example_network, file = "example_network.csv")
#' @export
export_to_netzer = function(network, filepath = NULL) {
  nodes = network$nodes %>%
    # rescale coordinates to keep layout nice
    dplyr::transmute(data_name = "Imported network",
                     data_version =  1,
                     System = "System 1",
                     `Variable.ID` = name,
                     `Section.on.Systems.Map` = type %>% tidyr::replace_na("None"),
                     `Variable.Name` = label,
                     Description = description %>% tidyr::replace_na(" "),
                     Value = 1,
                     Unit = " ",
                     `Source.organisation` = " ",
                     Weblink = " ",
                     Tags = tags %>% tidyr::replace_na(" "),
                     Quality.of.evidence = " ",
                     Additional.comments = " ",
                     insertdate = " ",
                     LiveData = 0,
                     Hidden = 0,
                     x,
                     y)

  edges = network$edges %>%
    dplyr::transmute(data_name = "Imported network",
                     `Variable.ID.A` = from,
                     `Variable.ID.B` = to,
                     Relationship = polarity %>%
                       dplyr::recode(positive = "Same", negative = "Opposite"), # recode the polarity indicators to the NetZer format
                     `Source.organisation` = " ",
                     `Justification...quantifiable` = description %>% tidyr::replace_na(" "),
                     `Relationship.Strength` = weight %>% dplyr::recode(`1` = "Medium"),
                     Weblink = " ")

  if(! filepath %>% is.null()) {
    readr::write_csv(nodes,
                     filepath %>%
                       stringr::str_replace(".csv", "_nodes.csv")) # write nodes
    readr::write_csv(edges,
                     filepath %>%
                       stringr::str_replace(".csv", "_edges.csv")) # write edges
  }

  list(nodes = nodes, edges = edges) # return nodes and edges as a list
}
