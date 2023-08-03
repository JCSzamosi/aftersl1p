test_that('plot_read_depth() is raising the right warnings',{
    load('plot_read_depth/dat.RData')
    wrn = paste('With no cvar specified,',
                'only the first colour in "clrs" will be used.')
    expect_warning(plot_read_depth(dat_filt, clrs = col_vect),
                   wrn)
})

test_that('plot_read_depth() is raising the right errors',{
    load('plot_read_depth/dat.RData')
    err = paste('With a continuous "cvar", the provided colour',
                'vector "clrs" must have names "high" and "low".')
    expect_error(plot_read_depth(dat_filt, cvar = 'ContVar', clrs = col_vect),
                 err)

    err = paste('If "clrs" is named, its names must contain',
                               'all the values in the "cvar" column')
    col_nms = col_vect
    names(col_nms) = c('a','b','c','d','e','f','g','h')
    expect_error(plot_read_depth(dat_filt, cvar = 'Source', clrs = col_nms),
                 err)
})
