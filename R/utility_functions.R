multiply_chr = function(x, factor) {
  x %>%
    as.double() %>%
    `*`(factor) %>%
    as.character()
}

nest_for_json = function(data, ...) {
  data %>%
    tidyr::nest(...) %>%
    purrr::pmap(listify_elements) %>%
    jsonlite:::simplify()
}

listify_elements = function(...) {
  list(...) %>%
    purrr::map_if(tibble::is_tibble, simplify_to_list)
}

simplify_to_list = function(df) {
  x = df %>%
    as.list()

  x[! x %>% purrr::map_lgl(is.na)]
}

calculate_link_angle = function(start_x, end_x, start_y, end_y, curvature = NA) {
  (-atan2(end_y-start_y,end_x-start_x) * 180 / pi) %>%
    round(3) %>%
    `%%`(360)
}
