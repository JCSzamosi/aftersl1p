% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_plot_rank_ab.R
\name{df_glom}
\alias{df_glom}
\title{Like tax_glom, but for data frames}
\usage{
df_glom(
  phy_df,
  ranks,
  IDcol = "X.SampleID",
  rank = "Phylum",
  abunds = "Abundance"
)
}
\arguments{
\item{phy_df}{A phyloseq data frame, as generated by
\code{\link[phyloseq:psmelt]{phyloseq::psmelt()}} or \code{\link[=make_phy_df]{make_phy_df()}}}

\item{ranks}{A character vector with the taxon rank names}

\item{IDcol}{The column name of the sample IDs}

\item{rank}{The taxonomic rank to glom at}

\item{abunds}{The column name of the abundances to sum}

\item{tots}{The desired column name of the summed (glommed) abundances}
}
\description{
\code{\link[=df_glom]{df_glom()}} take totals within sample at a given taxonomic rank.
}
