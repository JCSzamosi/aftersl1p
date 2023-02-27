# Test long_distance_df() ------------------------------------------------------

test_that('long_distance_df works with no baseline',{
    # Read in the data
    otumat = read.csv('long_distance_df/otumat.csv', row.names = 1)
    taxmat = read.csv('long_distance_df/taxmat_ambig.csv', row.names = 1)
    samdat = read.csv('long_distance_df/samdat.csv', row.names = 1)
    rownames(samdat) = samdat$StudyID

    ps = phyloseq::phyloseq(phyloseq::otu_table(as.matrix(otumat),
                                                taxa_are_rows = TRUE),
                  phyloseq::tax_table(as.matrix(taxmat)),
                  phyloseq::sample_data(samdat))
    dmat = phyloseq::distance(ps, method = 'bray')

    lddf = read.csv('long_distance_df/lddf_nobase.csv', row.names = 1)

    expect_equal(long_distance_df(dmat,samdat,idcol='StudyID'),
                                  lddf)
})

test_that('long_distance_df works with a baseline',{
    otumat = read.csv('long_distance_df/otumat.csv', row.names = 1)
    taxmat = read.csv('long_distance_df/taxmat_ambig.csv', row.names = 1)
    samdat = read.csv('long_distance_df/samdat.csv', row.names = 1)
    rownames(samdat) = samdat$StudyID

    ps = phyloseq::phyloseq(phyloseq::otu_table(as.matrix(otumat),
                                                taxa_are_rows = TRUE),
                  phyloseq::tax_table(as.matrix(taxmat)),
                  phyloseq::sample_data(samdat))
    dmat = phyloseq::distance(ps, method = 'bray')

    # Create single-column baseline df
    bs1 = data.frame(Notes = 'BSL')
    # Read in the expected output where only one column is included in baseline
    lddf_bs1 = read.csv('long_distance_df/lddf_base_1col.csv',row.names = 1)

    # Create two-column baseline df
    bs2 = data.frame(Notes = 'BSL', Group = 'A')
    # Read in the expected output where both metadata columns are included in
    # baseline, and there is only one baseline sample that everything is
    # compared to
    lddf_bs2 = read.csv('long_distance_df/lddf_base_2col_1bsamp.csv',
                        row.names = 1)

    expect_equal(long_distance_df(dmat, samdat, idcol = 'StudyID',
                                  baseline = bs1),
                 lddf_bs1) # One column baseline, many baseline values
    expect_equal(long_distance_df(dmat, samdat, idcol = 'StudyID',
                                  baseline = bs2),
                 lddf_bs2) # Two columns baseline, one baseline value
})
