\name{Manual.cut}
\alias{Manual.cut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Spectral region selection }
\description{
  This function selects the spectral region to the statistical analysis. Also, Manual.cut can be used to remove the water peak.
}
\usage{
Manual.cut(coordenadas)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coordenadas}{  Data.frame with the spectral information, where the first column holds the chemical shift positions and the different spectral intensities  are in the followings columns }
}
\details{
  Interactive Display.
}
\value{
  \item{datos }{Data.frame with the spectral information}
  \item{xlim }{Internal value}
  \item{xlim2 }{Internal value}

}
\author{ Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\seealso{\link{NMRS}}
\keyword{ interface }
