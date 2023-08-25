test_that("export_to_kumu works", {
  expect_equal(alternet::example_network %>% export_to_kumu() %>% jsonlite::fromJSON(),
               jsonlite::fromJSON("inst/extdata/example_network.json"))
})
