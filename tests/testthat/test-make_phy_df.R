# Test make_phy_df() -----------------------------------------------------------

test_that('make_phy_df() works when all slots are filled',{
    ## Read in the data

    ### Taxa matrix (no ambiguous genera)
    taxmat = read.csv('make_phy_df/taxmat.csv', row.names = 1)

    ### Taxa matrix (some identical genera are in different families)
    taxmat_ambig = read.csv('make_phy_df/taxmat_ambig.csv', row.names = 1)

    ### The OTU matrix used throughout this test
    otumat = read.csv('make_phy_df/otumat_rel.csv', row.names = 1)

    ### The sample data (mapfile) used throughout this test
    samdat = read.csv('prop_tax_down/samdat.csv', row.names = 1)

    ## Make the non-ambig phyloseq object
    ps = phyloseq::phyloseq(phyloseq::otu_table(as.matrix(otumat),
                                                taxa_are_rows = TRUE),
                  phyloseq::tax_table(as.matrix(taxmat)),
                  phyloseq::sample_data(samdat))

    ## Make the ambiguous phyloseq object
    ps_amb = phyloseq::phyloseq(phyloseq::otu_table(as.matrix(otumat),
                                                    taxa_are_rows = TRUE),
                      phyloseq::tax_table(as.matrix(taxmat_ambig)),
                      phyloseq::sample_data(samdat))

    ## Read in the expected, disambiguated `make_phy_df()` output
    plain_out = read.csv('make_phy_df/phy_df_out.csv')

    ### Ideally re-write this test so it doesn't rely on `order_taxa()` working
    ### correctly
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

test_that('make_phy_df() works when there is no sample_data',{

    ## Read in the data

    ### Taxa matrix (no ambiguous genera)
    taxmat = read.csv('make_phy_df/taxmat.csv', row.names = 1)

    ### Taxa matrix (some identical genera are in different families)
    taxmat_ambig = read.csv('make_phy_df/taxmat_ambig.csv', row.names = 1)

    ### The OTU matrix used throughout this test
    otumat = read.csv('make_phy_df/otumat_rel.csv', row.names = 1)

    ## Make the non-ambig phyloseq object
    ps = phyloseq::phyloseq(phyloseq::otu_table(as.matrix(otumat),
                                                taxa_are_rows = TRUE),
                  phyloseq::tax_table(as.matrix(taxmat)))

    ## Read in the expected, disambiguated `make_phy_df()` output
    plain_out = read.csv('make_phy_df/phy_df_out.csv')

    ### Ideally re-write this test so it doesn't rely on `order_taxa()` working
    ### correctly
    ranks = c('Kingdom', 'Phylum', 'Class', 'Order', 'Family','Genus')
    for (r in ranks){
        plain_out = order_taxa(plain_out, r)
    }
    otu_out = read.csv('make_phy_df/phy_df_otu_out.csv')
    ranks = c(ranks,'Species', 'AmbigGenus', 'OTU')
    for (r in ranks){
        otu_out = order_taxa(otu_out, r)
    }

    plain_out = (plain_out
                 %>% dplyr::select(StudyID, Abundance:Genus)
                 %>% dplyr::rename(Sample = StudyID))
    tst = make_phy_df(ps, prop = FALSE)
    expect_equal(make_phy_df(ps,prop = FALSE), plain_out)
})

