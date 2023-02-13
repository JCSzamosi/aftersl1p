test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Test long_distance_df() ------------------------------------------------------

# Read in the data
otumat = read.csv('long_distance_df/otumat.csv', row.names = 1)
taxmat = read.csv('long_distance_df/taxmat_ambig.csv', row.names = 1)
samdat = read.csv('long_distance_df/samdat.csv', row.names = 1)
samdat$SampleID = rownames(samdat)
