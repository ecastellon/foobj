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
    skip("tested")
    expect_true(is_path("./aa.txt"))
    expect_true(is_path("./aa.txt/bb"))
    expect_true(is_path("aa/b.doc"))
    expect_true(is_path("aa\\b.doc"))
    expect_false(is_path("aa/b.doc/"))
    expect_false(is_path("aa.txt"))
})

test_that("file good",
{
    skip("tested")
    expect_true(ok_fname("pp.rda"))
    expect_true(ok_fname("./pp.rda"))
    expect_true(ok_fname("pp."))
    expect_false(ok_fname("tt/pp.rda"))
    expect_false(ok_fname("p?p.rda"))
})

test_that("utils",
{
    expect_identical(dots_arg(a, b), c("a", "b"))
    expect_identical(dots_arg(c("a", "b")), c("a", "b"))
})

test_that("save objects",
{
    ##skip("tem")
    aa <- data.frame(x = 1:3, y = 3:1)
    expect_error(save_df("", file = "tt.rda"))
    expect_error(save_df(1:3, file = "tt.rda"))
    expect_warning(save_df(aa, file = "pp/tt.rda")) ## directory
    expect_equal(add_tof(aa, file = "tt.rda"), "aa")
    expect_equal(save_df(aa, file = "tt.rda"), "aa")
    expect_equal(save_df(aa, file = "tt.rda", name = "bb"), "bb")
})
