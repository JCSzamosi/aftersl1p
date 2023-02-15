# Test make_phy_df() -----------------------------------------------------------

test_that('make_phy_df() works',{
    ## Read in the data
    taxmat = read.csv('make_phy_df/taxmat.csv', row.names = 1)
    taxmat_ambig = read.csv('make_phy_df/taxmat_ambig.csv', row.names = 1)
    otumat = read.csv('make_phy_df/otumat_rel.csv', row.names = 1)
    samdat = read.csv('prop_tax_down/samdat.csv', row.names = 1)

    ps = phyloseq(otu_table(as.matrix(otumat), taxa_are_rows = TRUE),
                  tax_table(as.matrix(taxmat)),
                  sample_data(samdat))
    ps_amb = phyloseq(otu_table(as.matrix(otumat), taxa_are_rows = TRUE),
                      tax_table(as.matrix(taxmat_ambig)),
                      sample_data(samdat))
    plain_out = read.csv('make_phy_df/phy_df_out.csv')
    ranks = c('Kingdom', 'Phylum', 'Class', 'Order', 'Family','Genus')
    for (r in ranks){
        plain_out = order_taxa(plain_out, r)
    }
    otu_out = read.csv('make_phy_df/phy_df_otu_out.csv')
    ranks = c(ranks,'Species', 'AmbigGenus', 'OTU')
    for (r in ranks){
        otu_out = order_taxa(otu_out, r)
    }
tst = make_phy_df(ps, prop = FALSE)
    expect_equal(make_phy_df(ps,prop = FALSE), plain_out)
    expect_equal(make_phy_df(ps,rank = 'OTU', prop = FALSE), otu_out)
    expect_equal(make_phy_df(ps_amb, rank = 'Genus', prop = TRUE), plain_out)
})