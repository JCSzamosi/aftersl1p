# Import packages --------------------------------------------------------------
library(phyloseq)

# Test dbig_genera() -----------------------------------------------------------

## Read in the data
taxmat = read.csv('tests/testthat/taxmat_unique.csv', row.names = 1)
otumat = read.csv('tests/testthat/otumat.csv', row.names = 1)
samdat = read.csv('tests/testthat/samdat.csv', row.names = 1)
taxmat_ambig = read.csv('tests/testthat/taxmat_ambig.csv', row.names = 1)
taxmat_dbig = read.csv('tests/testthat/taxmat_dbig.csv', row.names = 1)

## Make the unique ps obj (should match itself)
ps = phyloseq(otu_table(otumat, taxa_are_rows = TRUE),
              tax_table(as.matrix(taxmat)),
              sample_data(samdat))

## Make the ambig ps obj
ps_ambig = ps
tax_table(ps_ambig) = tax_table(as.matrix(taxmat_ambig))

## Make the disambiguated ps obj (should match ambig)
ps_dbig = ps
tax_table(ps_dbig) = tax_table(as.matrix(taxmat_dbig))

test_that('dbig_genera() works',{
    expect_equal(dbig_genera(ps), ps) #nothing to do, output should match input
    expect_equal(dbig_genera(ps_ambig), ps_dbig) #check output
})
