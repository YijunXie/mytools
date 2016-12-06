context("piecewise linear scoring function")

test_that("piecewise linear scoring function",{
	r1 = pwls(0.09,0.05,0.95)
	r2 = pwls(0.01,0.05,0.95)
	re = pwls(0.01,0.05,10)

	expect_true(r1>0)
	expect_true(r2>0)
	expect_false(r1>r2)
})
