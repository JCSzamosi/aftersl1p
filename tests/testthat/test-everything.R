# Import packages --------------------------------------------------------------
library(phyloseq)

# Test dbig_genera() -----------------------------------------------------------

## Read in the data
taxmat = read.csv('dbig_genera/taxmat_unique.csv', row.names = 1)
otumat = read.csv('dbig_genera/otumat.csv', row.names = 1)
samdat = read.csv('dbig_genera/samdat.csv', row.names = 1)
taxmat_ambig = read.csv('dbig_genera/taxmat_ambig.csv', row.names = 1)
taxmat_dbig = read.csv('dbig_genera/taxmat_dbig.csv', row.names = 1)

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

# Test prop_tax_down() ---------------------------------------------------------

## Read in the data
taxmat = read.csv('prop_tax_down/taxmat_unique.csv', row.names = 1)
otumat = read.csv('prop_tax_down/otumat.csv', row.names = 1)
samdat = read.csv('prop_tax_down/samdat.csv', row.names = 1)
taxmat_propped = read.csv('prop_tax_down/taxmat_unique_propped.csv',
                          row.names = 1)
taxmat_ambig = read.csv('prop_tax_down/taxmat_ambig.csv', row.names = 1)
taxmat_pr_dbg = read.csv('prop_tax_down/taxmat_ambig_propped_dbig.csv',
                         row.names = 1)
taxmat_pr_ndb = read.csv('prop_tax_down/taxmat_ambig_propped_nodbig.csv',
                         row.names = 1)

## Make the unique ps obj (should match itself)
ps = phyloseq(otu_table(otumat, taxa_are_rows = TRUE),
              tax_table(as.matrix(taxmat)),
              sample_data(samdat))

## Make the propped ps obj
ps_propped = ps
tax_table(ps_propped) = tax_table(as.matrix(taxmat_propped))

## Make the ambig ps obj
ps_ambig = ps
tax_table(ps_ambig) = tax_table(as.matrix(taxmat_ambig))
ps_dbig = ps
tax_table(ps_dbig) = tax_table(as.matrix(taxmat_pr_dbg))

## Make the non-dbigged ps obg
ps_ndb = ps
tax_table(ps_ndb) = tax_table(as.matrix(taxmat_pr_ndb))

test_that('prop_tax_down() works',{
    expect_equal(prop_tax_down(ps, indic = FALSE), ps_propped)
    expect_equal(prop_tax_down(ps_propped, indic = FALSE), ps_propped) #do nothing
    expect_equal(prop_tax_down(ps_ambig, indic = FALSE), ps_dbig) # disambiguate
    expect_equal(prop_tax_down(ps_ambig, indic = FALSE, dbig = FALSE),
                 ps_ndb)
})
