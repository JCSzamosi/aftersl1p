% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop_tax_down.R
\name{dbig_genera}
\alias{dbig_genera}
\title{Disambiguate taxa}
\usage{
dbig_genera(physeq)
}
\arguments{
\item{physeq}{A phyloseq object with a tax table. Tax table must have 'Genus'
and 'Family' columns.}
}
\description{
Check for any taxa that have the same name but are in different higher-level
classifications, and append the family name
}
\section{Details}{
 \code{dbig_genera} takes a phyloseq object with a
taxonomy table, disamibugates the genera by appending family names to any
genus names that are found in multiple families in this data set, and
returns the object with an update tax_table() object. The input phyloseq
object must have 'Genus' and 'Family' columns in its tax_table object. The
output object with have the disambiguated names in the 'Genus' column and
will have a new column called 'AmbigGenus' where the old genus names can be
found.
}

\section{Value}{
 A phyloseq object with an updated Genus column and a column
called AmbigGenus containing the old Genus names.
}

