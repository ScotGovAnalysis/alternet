test_that("export_to_decision_explorer works", {
  expect_equal(example_network %>% export_to_decision_explorer(),
               xml2::read_xml("inst/extdata/example_network.mdx"))
})
