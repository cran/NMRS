\name{Baseline.Correction}
\alias{Baseline.Correction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Baseline Correction }
\description{
   Interactive Baseline Correction of raw spectra based in the FTICRMS package.
}
\usage{
Baseline.Correction(coord)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coord}{ Data.frame with the spectral information, where the first column holds the chemical shift positions and the different spectral intensities  are in the followings columns }
}
\details{
  Baseline correction is a very essential step to obtain high quality NMR spectra in some cases.
 Rolling baselines can make it difficult to identify peaks, as well as introduce significant errors into any quantitative measurements. 
This function based in the FTICRMS package launches a interactive graphical display to control the individual baseline correction. It computes an estimated baseline curve for a spectrum by a method of Rocke and Xi . 

}
\value{
  \item{a3 }{Estimated Baseline}
  \item{datos }{Data.frame with the baseline corrected}
  
}
\references{FTICRMS package  \url{http://cran.r-project.org/web/packages/FTICRMS/index.html}}
\author{ Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\seealso{\link{NMRS}}
\keyword{ interface }