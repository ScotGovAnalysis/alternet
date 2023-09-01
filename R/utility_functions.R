# multiple two numbers that can be stored as characters (and convert to character)
multiply_chr = function(x, factor) {
  x %>%
    as.double() %>%
    `*`(factor) %>%
    as.character()
}

# nest data in such a way that it gets interpreted correctly by jsonlite
nest_for_json = function(data, ...) {
  data %>%
    tidyr::nest(...) %>%
    purrr::pmap(listify_elements) %>%
    jsonlite:::simplify()
}

# convert any of the input variables that are tibbles into lists
listify_elements = function(...) {
  list(...) %>%
    purrr::map_if(tibble::is_tibble, simplify_to_list)
}

# turn a variable into a list, and filter out NA elements
simplify_to_list = function(df) {
  x = df %>% as.list()

  x[! x %>% purrr::map_lgl(is.na)]
}

#calculate the angle for a link between two points
calculate_link_angle = function(start_x, end_x, start_y, end_y, curvature = NA) {
  (-atan2(end_y-start_y,end_x-start_x) * 180 / pi) %>% # calculate angle in degrees
    `%%`(360) %>% # force angle to the 0 to 360 degree range
    round(3) # round to three decimal places
}

# recode a number of columns by looking up the alternatives in a dictionary
recode_by_dict = function(data, columns, old_key, new_key, dict) {
  # if multiple columns are to be recoded, iteratively run the function for each column
  if(length(columns) > 1) {
    output = columns %>%
      purrr::reduce(recode_by_dict, old_key, new_key, dict, .init = data)
  }

  # otherwise, recode the column
  if(length(columns) == 1) {
    # select just the relevant columns from dict and rename them to avoid conflicts
    dict = dict %>%
      dplyr::select(old_key = !!old_key,
                    new_key = !!new_key)

    output = data %>%
      dplyr::left_join(dict, by = structure(names = columns, .Data = "old_key")) %>% # add the ne_key information
      dplyr::mutate(!!columns := new_key) %>% # recode the required column with new_key
      dplyr::select(-new_key) # remove new_key again
  }

  output
}
