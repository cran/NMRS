\name{Fid}
\alias{Fid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Bruker FID loading}
\description{
 This function loads a Buker FID file and applies  the Fast Fourier Transform.
}
\usage{
Fid()
}
%- maybe also 'usage' for other objects documented here.
\details{
  The Fid function has been developed to be launched with the \link{Met.FID} Interactive Display
}
\value{
  \item{fidRaw }{Original Bruker FID format}
  \item{fid }{Bruker FID in complex numbers format}
  \item{SF }{SF value}
  \item{SWHz }{SWHz value}
  \item{XScaleHZ }{X scale in Hz}
  \item{ PPM }{X scale in PPM}
  \item{spectrum }{FFT of the FID file}
  \item{tt }{Internal value}
  \item{ si }{Internal value}
}
\references{ R.R. Ernst, G. Bodenhausen, A. Wokaun. Principles of Nuclear Magnetic Resonance in One and Two Dimensions. Claredom Press, Oxford, 2003.}
\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
\seealso{\link{Met.FID}, \link{NMRS}}
 
\keyword{ interface }

