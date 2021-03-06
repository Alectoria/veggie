% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/merge_cov.R
\name{merge_cov}
\alias{merge_cov}
\title{Merge cover values}
\usage{
merge_cov(dataframe, cover, plot, taxacol, taxonname = NULL, layercol,
  layernames = NULL, method = "independent")
}
\arguments{
\item{dataframe}{a data frame in long format, with plot IDs and taxa names stored in two columns.}

\item{cover}{name of column with cover values (numeric proportions in the range of 0-1).}

\item{plot}{name of column with plot IDs. Column can be of type character, integer, numeric or factor.}

\item{taxacol}{name of column with taxa names. Column must be of type character or factor.}

\item{taxonname}{name of taxon which cover values should be merged in a plot.
Default is "NULL": function merges cover values for identical taxa in a plot, across all layers they occur in.}

\item{layercol}{name of column with layer information. Column can be of type character, integer, numeric or factor.}

\item{layernames}{name(s) of layer(s) which cover values should be merged. Vector can be numeric, integer, factor or character.
Default is "NULL": function merges cover values for all layers in which a species occurs.}

\item{method}{method for merging cover values. Current options are "independent" (default) and "sum". See details below.}
}
\value{
Returns a new data frame in long format in which cover values for species within plots are merged and stored in a new column ("new.cover").
         The original columns 'cover' and 'layercol' will be replaced by new columns 'new.cov' and 'layer.new', the latter designating merged layers with "new_layer_veggie".
}
\description{
Merge cover values within vegetation plots under the assumption of independent overlap or no overlap among plant taxa. Function can be used to merge cover values for a specific taxon and/or specific layers.
}
\details{
For method="independent", this function will merge cover values under the independence assumption (Tichy & Holt 2006, Fischer 2014),
         with the degree of overlap between plants following a normal distribution. By contrast, the 'sum' method does not assume any overlap among plants and simply sums up the merged cover values.

         Unless values are supplied for layernames or taxonname, the function will merge cover values for across all layers or across identical taxa, respectively.
         By supplying values to these two arguments, the function can merge values at different levels of complexity.

         (1) In the simplest case (default options layernames=NULL, taxonname=NULL), the function will merge all cover values for identical taxa in a plot, across all layers they occur in.

         (2a) If a character value is supplied for the taxonname argument but layernames==NULL, the function will merge all cover values for the specified taxon, irrespective of the layer it occurs in.

         (2b) If values are supplied for the layersnames argument but taxonname=NULL, the function will merge all cover values of the same species for the respective layers.


         (3) The most complex scenario is when the users supply values for the layers and taxonname argument. In this case, the function will merge cover values only for the specified taxon and layers, in a plot.
}
\examples{
veg.df <- data.frame(plot.ID=paste("P",c(1,1,1,1,1, 2,2,2,2,2,2,2),sep=""),
                   species.name=c("Spec1", "Spec1", "Spec1","Spec2", "Spec3","Spec1", "Spec1", "Spec5", "Spec4", "Spec6","Spec6", "Spec6"),
                   layer=c(6,5,4,6,6, 4,5,2,3,4,5,6),
                   abund=c(0.4,0.2,0.2,0.1,0.8,0.6,0.4,0.4,0.7,0.1,0.6,0.2),
                   region=c(rep("Region1",5), rep("Region2",7)))

# Merge cover values for identical species within a plot, irrespective of the layer they occur in,
# under the assumption of independent overlap
merge_cov(veg.df, cover="abund", plot="plot.ID", taxacol="species.name", layercol="layer")

#...and under the assumption of no overlap
merge_cov(veg.df, cover="abund", plot="plot.ID", taxacol="species.name",layercol="layer",  method="sum")

# Merge cover values for identical species within a plot, for layers "4" and "5"
# (assumption of independent overlap)
merge_cov(veg.df, plot="plot.ID", taxacol="species.name",
         layercol="layer", layernames=c("4","5"), cover="abund")

# Merge cover values for taxon "Spec1" and layer "4" and "5" within a plot
# (assumption of independent overlap)
merge_cov(veg.df, plot="plot.ID", taxacol="species.name",taxonname="Spec1",
         layercol="layer", layernames=c("4", "5"), cover="abund")
}
\references{
Fischer, H.S. 2014. "On the combination of species cover values from different vegetation layers". Applied Vegetation Science 18: 169-170.

             Tichý, L. & Holt, J. 2006. "JUICE program for management, analysis and classification of ecological data." Program manual. Masaryk University Brno, Czech Republic.
}
\keyword{data}
\keyword{data,}
\keyword{manipulation.}
\keyword{vegetation}

