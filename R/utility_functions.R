multiply_chr = function(x, factor) {
  x %>%
    as.double() %>%
    `*`(factor) %>%
    as.character()
}
