context("chibar estimation")

test_that("chibar estimation",{
	d1 = chi.bar(rnorm(1000),q = 0.9,n = 1)
	d2 = chi.bar(rt(1000,df = 5),q = 0.9,n = 1)
	de = chi.bar(rep(0,100))

	expect_equal(length(d1),3)
	expect_true(d1[1] < 0)
	expect_false(d2[3] < 0)
	expect_error(if(de[1]<1){})
})
