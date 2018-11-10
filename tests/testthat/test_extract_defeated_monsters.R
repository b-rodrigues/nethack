context("Testing extract_defeated_monsters")

only_vanquished <- readr::read_lines("only_vanquished.txt")

monsters_only_vanquished <- extract_defeated_monsters(only_vanquished)

monsters_only_vanquished_expected <- readr::read_csv("monsters_only_vanquished.csv")

monsters_only_vanquished_expected$value <- as.numeric(monsters_only_vanquished_expected$value)

test_that("In case of only vanquished monsters", {
    expect_equal(monsters_only_vanquished, monsters_only_vanquished_expected)
})



only_one <- readr::read_lines("only_one_monster_vanquished.txt")

monsters_only_one <- extract_defeated_monsters(only_one)

monsters_only_one_expected <- readr::read_csv("monsters_only_one.csv")

monsters_only_one_expected$value <- as.numeric(monsters_only_one_expected$value)

test_that("In case of only one vanquished monster", {
    expect_equal(monsters_only_vanquished, monsters_only_vanquished_expected)
})



nothing <- readr::read_lines("no_vanquished_no_genocided_nor_extinct.txt")

monsters_nothing <- extract_defeated_monsters(nothing)

test_that("In case of no vanquished, nor genocided nor extinct", {
    expect_equal(monsters_nothing, NA)
})



genocided_extinct <- readr::read_lines("genocided_and_extinct.txt")

monsters_genocided_and_extinct <- extract_defeated_monsters(genocided_extinct)

monsters_genocided_and_extinct_expected <- readr::read_csv("monsters_genocided_and_extinct.csv")

monsters_genocided_and_extinct_expected$value <- as.numeric(monsters_genocided_and_extinct_expected$value)

test_that("In case of no vanquished, nor genocided nor extinct", {
    expect_equal(monsters_genocided_and_extinct, monsters_genocided_and_extinct_expected)
})
