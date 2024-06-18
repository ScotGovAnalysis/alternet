#' Imports a Kumu json-formatted model as a network, converting to a standardised R format
#'
#' @param filepath The filepath of the Kumu json file
#' @param scaling Factor to scale the coordinates down by

#' @return A named list with a tibble containing information about nodes, and a tibble containing information about edges
#' @examples import_from_kumu_json("inst/extdata/example_network.json", scaling = 2)
#' @export
import_from_kumu_json = function(filepath, scaling = 2) {


  json_data = jsonlite::fromJSON(filepath) # read the data into a list

  nodes = json_data$elements %>% # select the "elements" (or nodes)
    tidyr::unnest(cols = c(attributes)) # tidy the format of the columns

  if(!"id" %in% colnames(nodes)){   #if "id" not in attributes, add id column
    nodes <- nodes %>%
      dplyr::mutate(id = dplyr::row_number())
  }
  if(!"element type" %in% colnames(nodes)){   #if "element type" not in attributes, add a constant string as type
    nodes <- nodes %>%
      dplyr::mutate("element type" = "type") #
  }

   nodes <- nodes %>%
     dplyr::rename(element = `_id`, refno = id) %>%
    dplyr::left_join(json_data$maps$elements[[1]] %>% #join with map layout information
                       tidyr::unnest(cols = any_of(c("position", "style"))),
                     by = "element") %>%
    add_if_absent(c("pinned", "fontColor",
                    "fontWeight", "element type")) %>%
    dplyr::rename(name = element,
                  type = `element type`,
                  id = `_id`,
                  font_colour = fontColor,
                  font_weight = fontWeight) %>%
    dplyr::mutate(x = x / scaling, # rescale coordinates to keep layout nice
                  y = y / scaling,
                  description = as.character(NA),
                  tags = as.character(NA))

  # handling styles is something to tackle in future
  node_styles = nodes %>%
    dplyr::mutate(type = type %>% dplyr::na_if("")) %>%
    dplyr::distinct(type, .keep_all = TRUE) %>% # identify distinct groups by style
    dplyr::select(type, font_colour, font_weight)

  nodes = nodes %>%
    dplyr::select(-pinned, -font_colour, -font_weight)

  edges = json_data$connections %>% # select the "connections" (or edges)
    tidyr::unnest(cols = c(attributes))  # tidy the format of the columns

  if(!"id" %in% colnames(edges)){ #if "id" not in attributes, add id column
    edges <- edges %>%
      dplyr::mutate(id = dplyr::row_number())
  }

  if(!"connection type" %in% colnames(nodes)){   #if "connection type" not in attributes, add a "postive" as type
    edges <- edges %>%
      dplyr::mutate("connection type" = "positive") #
  }

  edges <- edges %>%
    dplyr::rename(connection = `_id`, refno = id) %>%
    dplyr::left_join(tibble::as_tibble(json_data$maps$connections[[1]]), #join with map layout information
                     by = "connection") %>%
    dplyr::select(name = connection,
                  refno,
                  polarity = `connection type`,
                  from,
                  to,
                  id = `_id`) %>%
    dplyr::mutate(polarity = polarity %>%
                    dplyr::recode(`+` = "positive", `-` = "negative"), # format polarity as "positive" and "negative" instead of "+" and "-"
                  curvature = as.double(NA), # handling curvature is something to tackle in future
                  description = as.character(NA),
                  weight = 1)

  list(nodes = nodes, edges = edges, node_styles = node_styles) # return the node, edge and style information
}
