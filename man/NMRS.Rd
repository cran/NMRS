\name{NMRS}
\alias{NMRS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Preprocesing of NMR Spectra }
\description{
 Import.data loads directly the Bruker spectroscopy format (FID file) and applies the Fast Fourier Transform. Furthermore,  This application displays the spectrum reference and manages basic operations such as setting the chemical shift of a certain compound (TSP or DSS) to 0 ppm, zero order and first order phase corrections, baseline adjustment,...
}

\usage{
NMRS()
}
%- maybe also 'usage' for other objects documented here.
\details{
  Interactive application.
}
\value{
   \item{Spectra }{Preprocessed spectra}

}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }

\seealso{
\link{Baseline.Correction}, \link{Fid}, \link{Manual.cut}, \link{phase}

}
\keyword{ interface }
