test_that("import_from_kumu_json works", {
  expect_equal(alternet::example_network,
               import_from_kumu_json("inst/extdata/example_network.json"))
})
