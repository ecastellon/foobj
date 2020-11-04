## -*- coding: utf-8 -*-

## test_file cambia dir de trabajo
context("archivos con objetos")

test_that("try-load",
{
    skip("tested")
    expect_message(try_load(), "... falta nombre archivo !!!")
    ## dir.trab. es tests/testthat
    aa <- try_load("../tstread.rda")
    expect_true(setequal(aa, c("a", "b", "c")))
    expect_null(try_load("tstread.rda"))
    expect_message(try_load("tstread.rda"), "^\\.\\.\\. NO.+")
    rm(list = aa)
})

test_that("path may be",
{
    expect_true(is_path("./aa.txt"))
    expect_true(is_path("./aa.txt/bb"))
    expect_true(is_path("aa/b.doc"))
    expect_true(is_path("aa\\b.doc"))
    expect_false(is_path("aa/b.doc/"))
    expect_false(is_path("aa.txt"))
})
