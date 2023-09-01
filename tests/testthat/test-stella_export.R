test_that("export_to_stella works", {
  expect_equal(alternet::example_network %>% export_to_stella(),
               xml2::read_xml("inst/extdata/example_network.stmx"))
})
