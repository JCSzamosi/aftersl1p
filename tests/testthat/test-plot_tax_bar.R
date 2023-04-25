test_that('plot_tax_bar() is throwing the right errors',{
    load('plot_tax_bar/dats.RData')
    fam_df = make_phy_df(dat_rel_filt, 'Family', prop = FALSE)

    err = 'rank argument must be one of the columns in your data frame'
    expect_error(plot_tax_bar(fam_df, 'Genus', sample = 'SampleID'),err)

    err = "leglen must be an integer"
    expect_error(plot_tax_bar(fam_df, 'Family', sample = 'SampleID',
                              leglen = 'foo'), err)

    cols = rep(c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33",
                 "#A65628","#F781BF"), 3)
    names(cols) = unique(fam_df$Class)
    err = 'The colour vector names need to match the rank levels'
    expect_error(plot_tax_bar(fam_df, 'Family', sample = 'SampleID',
                              colours = cols), err)

})

test_that('plot_tax_bar() is throwing the right warnings',{
    load('plot_tax_bar/dats.RData')
    fam_df = make_phy_df(dat_rel_filt, 'Family', prop= FALSE)

    wrn = "leglen is greater than the number of unique taxa. showing all taxa."
    expect_warning(plot_tax_bar(fam_df, 'Family', sample = 'SampleID',
                                leglen = 30), wrn)
    wrn = 'leglen can not be negative. treating it like 0.'
    expect_warning(plot_tax_bar(fam_df, 'Family', sample = 'SampleID',
                                leglen = -3), wrn)

    wrn = paste('The supplied colour vector is shorter than the number',
	                   'of distinct taxa.')
    cols = c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33",
                 "#A65628","#F781BF")
    expect_warning(plot_tax_bar(fam_df, 'Family', sample = 'SampleID',
                                colours = cols), wrn)

    names(cols) = unique(fam_df$Family)[1:length(cols)]
    expect_warning(plot_tax_bar(fam_df, 'Family', sample = 'SampleID',
                                colours = cols), wrn)

    wrn = paste('Your per-sample abundances sum to >1. Did you mean to',
	               'specify \'means = TRUE\'?')
    expect_warning(plot_tax_bar(fam_df, 'Family', sample = 'Source'),wrn)
})
