
context('GetZestimate')

set_zillow_web_service_id('X1-ZWz181enkd4cgb_82rpe')
zapi_key = getOption('ZillowR-zws_id')

test_that(" provide the correct zip code", {
  # expect an error due to incorrect zip code
  expect_error(GetZestimate(zpids=abc,rentzestimate=TRUE,api_key=zapi_key))
  # expect an error due to incorrect zip code
  expect_error(GetZestimate(zpids=a1b2c3,rentzestimate=TRUE,api_key=zapi_key))
})

test_that(" provide the correct api key", {
  # expect an error due to incorrect api key
  expect_error(GetZestimate(zpids='1341571',rentzestimate=TRUE,api_key=abc))
  # expect an error due to incorrect api key
  expect_error(GetZestimate(zpids='1341571',rentzestimate=TRUE,api_key=a1b2c3))
})

test_that(" output is a dataframe", {
  # expect data frame
  expect_s3_class(GetZestimate(zpids='1341571',rentzestimate=TRUE,api_key=zapi_key), "data.frame")
})

