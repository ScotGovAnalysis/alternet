test_that("import_from_stella_xml works", {
  expect_equal(alternet::example_network$nodes %>% dplyr::select(-type),
               import_from_stella_xml("inst/extdata/example_network.stmx")$nodes %>% dplyr::select(-type))
  expect_equal(alternet::example_network$edges,
               import_from_stella_xml("inst/extdata/example_network.stmx")$edges)
})
