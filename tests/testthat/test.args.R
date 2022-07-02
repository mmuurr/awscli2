test_that("processx arg string", {

  set_default_profile("test-profile")

  expect_equal(
    aws_args(
      c("command", "subcommand1", "subcommand2"),
      "key1" = "val1",
      "key1" = "val2",
      "key2" = NULL,
      "key2" = "val3",
      "key3" = NULL,
      "key3" = "val4",
      "positional1",
      "key3" = NULL,
      "positional2",
      "",
      "positional3",
      NULL
    ),
    c(
      "--output", "json",
      "--profile", "test-profile",
      "command", "subcommand1", "subcommand2",
      "--key1", "val2",
      "--key2", "val3",
      "positional1",
      "--key3",
      "positional2",
      "positional3"
    )
  )

  expect_equal(
    aws_args(
      c("command", "subcommand1", "subcommand2"),
      "key1" = "val1",
      "key1" = "val2",
      "key2" = NULL,
      "key2" = "val3",
      "key3" = NULL,
      "key3" = "val4",
      "positional1",
      "key3" = NULL,
      "positional2",
      "",
      "positional3",
      NULL,
      .config = list()
    ),
    c(
      "--output", "json",
      "--profile", "test-profile",
      "command", "subcommand1", "subcommand2",
      "--key1", "val2",
      "--key2", "val3",
      "positional1",
      "--key3",
      "positional2",
      "positional3"
    )
  )

  expect_equal(
    aws_args(
      c("command", "subcommand1", "subcommand2"),
      "key1" = "val1",
      "key1" = "val2",
      "key2" = NULL,
      "key2" = "val3",
      "key3" = NULL,
      "key3" = "val4",
      "positional1",
      "key3" = NULL,
      "positional2",
      "",
      "positional3",
      NULL,
      .config = list("profile" = "foo")
    ),
    c(
      "--output", "json",
      "--profile", "foo",
      "command", "subcommand1", "subcommand2",
      "--key1", "val2",
      "--key2", "val3",
      "positional1",
      "--key3",
      "positional2",
      "positional3"
    )
  )
  
})


test_that("ignore doubledash", {

  expect_equal(
    aws_args(
      c("rds", "create-db-instance"),
      "--already" = "doubledashed",
      "not" = "doubled",
      .config = list()
    ),
    c(
      "--output", "json",
      "--profile", "test-profile",
      "rds", "create-db-instance",
      "--already", "doubledashed",
      "--not", "doubled"
    )
  )
  
})
