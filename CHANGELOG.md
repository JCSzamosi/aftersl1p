* 2023-08-03 v0.0.1.9002 (development update)
	* **BREAKING CHANGES**
		* Completely re-writes `plot_read_depth()`. 
			* allows users to plot read depth with a variable on the X axis and
			a colour parameter
			* users can access the old function with `plt_read_depth()`
			temporarily, but this will be removed before the next full release.
		* `rank_abund()` is broken and is no longer exported. Please file a bug
		report if you were using this function.
	* exports `order_taxa()`, by request 
	* makes `prop_tax_down()` slightly more efficient by checking up front if
	there is nothing to do.
	* deprecates `order_levs()` because it isn't used anywhere. Its intended
	function is performed by `order_taxa()`.
	* introduces visual and automatic testing of the new `plot_read_depth()`
	function.

* 2023-04-25 v0.0.1.9001 (development update)
	* in `plot_tax_bar()` 
		* the `legloc` argument is now passed directly to
		`ggplot2::theme(legend.position)` and can take any value that can take.
		* added a `r_ticks` argument. FALSE by default (default behaviour
		is unchanged). If TRUE, the tick text on the x-axis is rotated 90
		degrees and reads down to up.
		* introduced improved functionality when a custom colour vector is used,
		with and without names
		* introduce a `leglen` option to allow the user to limit how many taxa
		are displayed in the legend without removing any taxa from the plot.
		* soft-deprecate the `yscale` argument. Will stop supporting non-linear
		y-axes soon
		* improve error when the `rank` argument is missing from the input dat
		frame
		* introduce a warning when the per-sample abundaces sum to greater than
		1 but the `mean` argument is not set to `TRUE`.
		* prep the function so I can stop exporting the whole `ggplot2`
		namespace
	* introduce lifecycle management with the `lifecycle()` package
	* start using roxygen2md to use Markdown in documentation.
	* introduce the `benchmark` folder which contains "good" plotting outputs
	against which new versions of the package can be tested. Created a .Rmd
	file in that folder which tests `plot_tax_bar()`.
	* remove the files that held the old colour vectors


* 2023-04-14 v0.0.1 (was v1.0.1)
	* The multiple colour vectors have been replaced with a single object,
	`tax_colours`, which will cycle if there are more than 30 taxa. Having more
	than 30 distinguishable colours is not possible anyway.
	* A bunch of under-the hood reorganization: Functions have been moved to
	their own .R files or grouped into files with their internal helpers
	* A test suite using `testthat` has been introduced to catch future problems
	* rename default git branch to `main`
	* rationalize the versioning system based on guidelines from [the r-pkgs
	book](https://r-pkgs.org)

* 2022-01-11 v0.0.0 (was v1.0)
	* **Major bug fix**
	* this package has seen minimal changes since January of 2018 when it was
	first written; however, in April of 2020 there was a major change in the
	functionality of the `right_join()` function in the `dplyr` package
	contained within the `tidyverse` suite of packages.	This change broke the 
	sorting of taxa relative to OTUs/ASVs when running the 	function 
	`dbig_genera()` which is called by default when you run	`prop_tax_down()`. 
	* Whether you were affected by this bug will depend on if you have
	updated `dplyr` to version 1.0.0. or later. You can check your current
	version of `dplyr` by running `packageVersion('dplyr')` in your R console.
	* If you ran `prop_tax_down()` on your data after April of 2020, and you did
	_not_ have the `dbig = FALSE` flag set, your taxon assignments **were
	effectively randomized**. Any analyses that relied on taxon assignment
	(e.g. differential abundance analyses like DESeq, ANCOM, aldex, etc. or
	visualizations like taxa bar charts) will be incorrect and need to be
	re-run.
	* Estimates of alpha and beta diversity, PCoA plots, PERMANOVA tests, and 
	placement of samples on	UPGMA trees are taxon-insensitive, and are _not_ 
	affected by this bug.
	* Regardless of which version of `dplyr` you are currently running, it is 
	**highly recommended** that you update to this version of the aftersl1p 
	package ASAP. The current fix will work with versions of `dplyr` from before
	and after the change.
