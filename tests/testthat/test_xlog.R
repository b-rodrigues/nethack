context("Testing clean_xlog")

xlog <- readr::read_delim("test_xlog.csv",
                   "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

clean_xlog <- clean_xlog(xlog)

test_that("Two rows", {
    expect_equal(nrow(clean_xlog), 2)
})

test_that("Colnames", {
    expect_equal(colnames(clean_xlog), c("points", "deathdnum", "deathlev", "maxlvl", "hp", "maxhp",
                                         "deaths", "deathdate", "birthdate", "role", "race",
                                         "gender", "align", "name", "death", "turns", "realtime",
                                         "starttime", "endtime", "gender0", "align0", "killed_while"))
})
