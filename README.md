
<!-- README.md is generated from README.Rmd. Please edit that file -->

# AfterSl1p

<!-- badges: start -->

![experimental](man/figures/lifecycle-experimental.svg)
<!-- badges: end -->

This is a draft R package that provides convenience functions for the
organization, curation, and visualization of microbiome data sets. This
package is lightly maintained by a single human and is provided as is,
with no guarantees of any kind.

Please see the [CHANGELOG](./CHANGELOG.md) for important updates and bug
fixes, and make sure you are using the most recent version of this
package. A major bug fix was released January 17, 2022, so please make
sure you are running a newer version than that. Any bugs or feature
requests can be reported via the [issues
tab](https://github.com/JCSzamosi/aftersl1p/issues). I can make no
guarantee about how soon updates or fixes will happen, but I will do my
best to fix critical bugs a quickly as possible.

To use this package I recommend cloning the repository using git. That
will make it easiest for you to stay up to date with new versions. If
using git is not an option for you, you can download a zip file of the
code using the green button at the top right corner of the page; however
I recommend you check back at least once every six months and download a
new version if there have been updates.

## Installation

You can install the most stable version of AfterSl1p from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JCSzamosi/aftersl1p@*release")
```

You can also install the latest, development version with

``` r
# install.packages("devtools")
devtools::install_github("JCSzamosi/aftersl1p@v0.0.1.9001")
```

N.B.: The value after the @ in the development version will change
frequently, so check back here (or check the Releases section of the
repository) to make sure you’re getting the most recent version.

<!--
## Example

This is a basic example which shows you how to solve a common problem:


```r
library(AfterSl1p)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:


```r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->
