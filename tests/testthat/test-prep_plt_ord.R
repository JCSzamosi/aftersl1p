test_that('make_ord_df() works with jaccard, pcoa',{
    # Read in the expected result
    good_jacc = read.csv('prep_plt_ord/jacc_df.csv', row.names = 1)
    good_jacc = (good_jacc
                 %>% dplyr::mutate(AxisX = factor(AxisX),
                            AxisY = factor(AxisY)))

    # Read in the data
    load('./prep_plt_ord/dat_rel_filt.RData')

    # Run it
    expect_equal(make_ord_df(dat_rel_filt, dist_meth = 'jaccard'), good_jacc)
    expect_equal(make_ord_df(dat_rel_filt, dist_meth = 'jaccard', axes = 1:3),
                 (good_jacc
                  %>% dplyr::filter(AxisX != 'Axis.4 4.2%',
                        AxisY != 'Axis.4 4.2%')
                  %>% droplevels()))



})

test_that('make_ord_df() works with bray, pcoa', {
    # Read in the expected result
    good_bray = read.csv('prep_plt_ord/bray_df.csv', row.names = 1)
    good_bray = (good_bray
                 %>% dplyr::mutate(AxisX = factor(AxisX),
                            AxisY = factor(AxisY)))

    # Read in the data
    load('./prep_plt_ord/dat_rel_filt.RData')

    # Run it
    expect_equal(make_ord_df(dat_rel_filt), good_bray)
    expect_equal(make_ord_df(dat_rel_filt, axes = 1:3),
                 (good_bray
                  %>% dplyr::filter(AxisX != 'Axis.4 9.4%',
                        AxisY != 'Axis.4 9.4%')
                  %>% droplevels()))


})

test_that('make_ord_df() works with euclid, rda', {
    # Read in the expected result
    good_aitch = read.csv('prep_plt_ord/aitch_df.csv', row.names = 1)
    good_aitch= (good_aitch
                 %>% dplyr::mutate(AxisX = factor(AxisX),
                            AxisY = factor(AxisY)))

    # Read in the data
    load('./prep_plt_ord/dat_aitch.RData')

    # Run it
    expect_equal(make_ord_df(dat_aitch, dist_meth = 'euclidean',
                             ord_meth = 'RDA'), good_aitch)
    expect_equal(make_ord_df(dat_aitch, dist_meth = 'euclidean',
                             ord_meth = 'RDA', axes = 1:3),
                 (good_aitch
                  %>% dplyr::filter(AxisX != 'PC4 7%',
                        AxisY != 'PC4 7%')
                  %>% droplevels()))


})

test_that('make_ord_df() does something sensible when dist method is unknown', {
    # Read in the data
    load('./prep_plt_ord/dat_rel_filt.RData')

    expect_warning(make_ord_df(dat_rel_filt, dist_meth = 'jsd'),
                   regexp = paste('This function has only been tested with jaccard,',
                   'bray, and euclid distance methods. Other methods may',
                   'work but you are responsible for making sure what',
                   'you\'re doing is sensible.'))
})

test_that('make_ord_df() does something sensible when ord method is unknown', {
    # Read in the data
    load('./prep_plt_ord/dat_rel_filt.RData')

    expect_error(make_ord_df(dat_rel_filt, ord_meth = 'DCA'),
         regexp = paste('This function currently only works with PCoA and RDA',
                   'ordination methods. Pull requests are welcome.'))
})

test_that(paste('make_ord_df() errors correctly when RDA is used with',
                    'non-euclidean distance'),{

    # Read in the data
    load('./prep_plt_ord/dat_rel_filt.RData')

    expect_error(make_ord_df(dat_rel_filt, dist_meth = 'bray', ord_meth = 'RDA'),
      regexp = paste('RDA ordination is not sensible with non-metric distance',
                'metrics. For now only euclidean distance is accepted with',
                'RDA, but pull requests are welcome for other metric',
                'distance metrics which you may wish to implement and test.'))
})
