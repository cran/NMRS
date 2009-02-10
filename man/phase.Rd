\name{phase}
\alias{phase}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Phase Correction }
\description{
  Zero order and first order phase corrections.
}
\usage{
phase(fid.out, phc0, phc1, pivot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fid.out}{ FID file}
  \item{phc0}{ Zero orden correction}
  \item{phc1}{ First orden correction }
  \item{pivot}{ pivot }
}
\details{
  The phase function has been developed to be launched with the \link{Met.FID} Interactive Display

}
\references{ R.R. Ernst, G. Bodenhausen, A. Wokaun. Principles of Nuclear Magnetic Resonance in One and Two Dimensions. Claredom Press, Oxford, 2003.}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
\seealso{\link{Met.FID}, \link{NMRS}}
 
\keyword{ interface }
