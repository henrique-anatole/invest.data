# tests functions created with support from ChatGPT
test_that("direct input sets and retrieves key", {
  # save user’s real key if any
  old_key <- getOption("tiingo_key")  
  # clear any existing option or environmental key         
  options(tiingo_key = NULL)
  Sys.unsetenv("TIINGO_KEY")     
  # restore after test
  on.exit({
    options(tiingo_key = old_key)
    Sys.unsetenv("TIINGO_KEY")
  })

  # ensure option is unset
  set_tiingo_api_key("FAKEKEY123")
  expect_equal(getOption("tiingo_key"), "FAKEKEY123")
  
  # returns invisibly
  expect_equal(invisible(set_tiingo_api_key("NEWKEY", overwrite = TRUE)), "NEWKEY")
})

test_that("options() key is respected unless overwrite = TRUE", {
  # save user’s real key if any
  old_key <- getOption("tiingo_key")  
  # clear any existing option or environmental key         
  options(tiingo_key = NULL)
  Sys.unsetenv("TIINGO_KEY")     
  # restore after test
  on.exit({
    options(tiingo_key = old_key)
    Sys.unsetenv("TIINGO_KEY")
  })

  # ensure option is unset
  options(tiingo_key = "OPTIONKEY")
  set_tiingo_api_key()
  expect_equal(getOption("tiingo_key"), "OPTIONKEY")
  # calling again without overwrite does not change it
  set_tiingo_api_key("OVERWRITEKEY")
  expect_equal(getOption("tiingo_key"), "OPTIONKEY")
  # calling with overwrite changes it
  set_tiingo_api_key("OVERWRITEKEY", overwrite = TRUE)
  expect_equal(getOption("tiingo_key"), "OVERWRITEKEY")
})

test_that("environment variable is used when no option or direct key", {
  # save user’s real key if any
  old_key <- getOption("tiingo_key")  
  # clear any existing option or environmental key         
  options(tiingo_key = NULL)
  Sys.unsetenv("TIINGO_KEY")     
  # restore after test
  on.exit({
    options(tiingo_key = old_key)
    Sys.unsetenv("TIINGO_KEY")
  })

  # ensure env var is unset
  Sys.setenv(TIINGO_KEY = "ENVKEY")
  set_tiingo_api_key()
  expect_equal(getOption("tiingo_key"), "ENVKEY")
})

test_that("keyring is used if available and no option/env key", {
  # save user’s real key if any
  old_key <- getOption("tiingo_key")  
  # clear any existing option or environmental key         
  options(tiingo_key = NULL)
  Sys.unsetenv("TIINGO_KEY")     
  # restore after test
  on.exit({
    options(tiingo_key = old_key)
    Sys.unsetenv("TIINGO_KEY")
  })

  # check that keyring is called and sets the option
  with_mocked_bindings(
    {
      set_tiingo_api_key()
      expect_equal(getOption("tiingo_key"), "KEYRINGKEY")
    },
    .package = "keyring",
    key_get = function(...) "KEYRINGKEY"
  )
})

test_that("error is thrown if no key found anywhere", {
  # save user’s real key if any
  old_key <- getOption("tiingo_key")  
  # clear any existing option or environmental key         
  options(tiingo_key = NULL)
  Sys.unsetenv("TIINGO_KEY")     
  # restore after test
  on.exit({
    options(tiingo_key = old_key)
    Sys.unsetenv("TIINGO_KEY")
  })

  if (requireNamespace("keyring", quietly = TRUE)) {
    with_mocked_bindings({
        expect_error(set_tiingo_api_key(), "No valid Tiingo API key")
      },
      .package = "keyring",
       key_get = function(...) stop("no key found")
    )
  } else {
    expect_error(set_tiingo_api_key(), "No valid Tiingo API key")
  }
})

test_that("invalid key input raises error", {
  # check for non-character input and length > 1
  expect_error(set_tiingo_api_key(1234, overwrite = TRUE), "tiingo_key must be a single character string")
  expect_error(set_tiingo_api_key(c("A", "B"), overwrite = TRUE), "tiingo_key must be a single character string")
})

test_that("direct input sets and retrieves keys", {
  old_key <- getOption("bin_key")
  old_secret <- getOption("bin_secret")
  options(bin_key = NULL)
  options(bin_secret = NULL)
  on.exit({
    options(bin_key = old_key)
    options(bin_secret = old_secret)
    Sys.unsetenv("BIN_KEY")
    Sys.unsetenv("BIN_SECRET")
  })

  res <- set_binance_api_key("FAKEKEY", "FAKESECRET")
  expect_equal(getOption("bin_key"), "FAKEKEY")
  expect_equal(getOption("bin_secret"), "FAKESECRET")
  expect_equal(res$key, "FAKEKEY")
  expect_equal(res$secret, "FAKESECRET")
})

test_that("options() keys are respected unless overwrite = TRUE", {
  options(bin_key = "OPTIONKEY")
  options(bin_secret = "OPTIONSECRET")
  
  set_binance_api_key()
  expect_equal(getOption("bin_key"), "OPTIONKEY")
  expect_equal(getOption("bin_secret"), "OPTIONSECRET")

  set_binance_api_key("NEWKEY", "NEWSECRET", overwrite = TRUE)
  expect_equal(getOption("bin_key"), "NEWKEY")
  expect_equal(getOption("bin_secret"), "NEWSECRET")
})

test_that("environment variables are used when no option or direct keys", {
  options(bin_key = NULL)
  options(bin_secret = NULL)
  Sys.setenv(BIN_KEY = "ENVKEY", BIN_SECRET = "ENVSECRET")

  set_binance_api_key()
  expect_equal(getOption("bin_key"), "ENVKEY")
  expect_equal(getOption("bin_secret"), "ENVSECRET")
})

test_that("invalid key input raises error", {
  expect_error(set_binance_api_key(1234, "SECRET", overwrite = TRUE), "bin_key must be a single character string")
  expect_error(set_binance_api_key("KEY", 1234, overwrite = TRUE), "bin_secret must be a single character string")
  expect_error(set_binance_api_key(c("A","B"), "SECRET", overwrite = TRUE), "bin_key must be a single character string")
  expect_error(set_binance_api_key("KEY", c("X","Y"), overwrite = TRUE), "bin_secret must be a single character string")
})

test_that("error is thrown if no key found anywhere", {
  options(bin_key = NULL)
  options(bin_secret = NULL)
  Sys.unsetenv("BIN_KEY")
  Sys.unsetenv("BIN_SECRET")

  expect_error(set_binance_api_key(), "No valid Binance API key and secret found")
})
