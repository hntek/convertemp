context("Testing temperature conversion")

test_that("fahr_to_celsius returns a number", {
  expect_equal(fahr_to_celsius(68), 20)
  expect_equal(fahr_to_celsius(-10), -23.3333333333)
  expect_equal(fahr_to_celsius(0), -17.7777777777)
})

test_that("celsius_to_kelvin returns a number", {
  expect_equal(celsius_to_kelvin(68), 341.15)
  expect_equal(celsius_to_kelvin(-10), 263.15)
  expect_equal(celsius_to_kelvin(0), 273.15)
})



test_that("celsius_to_fahr returns a number", {
  expect_equal(celsius_to_fahr(68), 154.4)
  expect_equal(celsius_to_fahr(-10), 14)
  expect_equal(celsius_to_fahr(0), 32)
})

test_that("kelvin_to_celsius returns a number", {
  expect_equal(kelvin_to_celsius(68), -205.15)
  expect_equal(kelvin_to_celsius(1000), 726.85)
  expect_equal(kelvin_to_celsius(0), -273.15)
})

test_that("kelvin_to_fahr returns a number", {
  expect_equal(kelvin_to_fahr(68), -337.27)
  expect_equal(kelvin_to_fahr(1000), 1340.33)
  expect_equal(kelvin_to_fahr(0), -459.67)
})

test_that("fahr_to_kelvin returns a number", {
  expect_equal(fahr_to_kelvin(68), 293.15)
  expect_equal(fahr_to_kelvin(-441.67), 10)
  expect_equal(fahr_to_kelvin(-459.67), 0)
})

test_that("expect a number not a string", {
  expect_that(fahr_to_celsius("A"), throws_error())
  expect_that(fahr_to_kelvin("A"), throws_error())
  expect_that(celsius_to_fahr("A"), throws_error())
  expect_that(celsius_to_kelvin("A"), throws_error())
  expect_that(kelvin_to_celsius("A"), throws_error())
  expect_that(kelvin_to_fahr("A"), throws_error())

})

test_that("kelvin cannot be negative", {
expect_warning(kelvin_to_fahr(-10),"kelvin cannot be negative")
expect_warning(kelvin_to_celsius(-10),"kelvin cannot be negative")
})
