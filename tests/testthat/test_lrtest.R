context("likelihood ratio test")

test_that("likelihood ratio test",{
	d1 = c(rep(1,5),rep(0,95))
	d2 = c(rep(1,8),rep(0,190))
	d_e = c(rep(1,5),rep(0,10))
	derr = lrtest(rep(0,100),0.95)

	r1 = list(0,1)
	expect_equal(lrtest(d1,0.05),r1)
	expect_true(lrtest(d2,0.05)[[2]] > 0.05)
	expect_false(lrtest(d_e,0.05)[[2]] > 0.05)
	expect_error(if(derr[[1]]<1){})
})
