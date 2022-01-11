* 2022-01-11 v1.0
	* major bug fix
	* this package has seen minimal changes since January of 2018 when it was
	first written; however, in April of 2020 there was a major change in the
	functionality of the `right_join()` function in the `dplyr` package
	contained within the `tidyverse` suite of packages.	This change broke the 
	sorting of taxa relative to OTUs/ASVs when running the 	function 
	`dbig_genera()` which is called by default when you run	`prop_tax_down()`. 
	* Whether you were affected by this bug will depend on if you had
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
