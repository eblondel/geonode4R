% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GeoNodeVersion.R
\docType{class}
\name{GeoNodeVersion}
\alias{GeoNodeVersion}
\title{A GeoNode version}
\format{
\code{\link{R6Class}} object.
}
\value{
Object of \code{\link{R6Class}} for modelling a GeoNode version
}
\description{
This class allows to grab the GeoNode version.
}
\details{
GeoNode REST API - GeoNode Version
}
\examples{
\dontrun{
version <- GeoNodeVersion$new(
             url = "http://localhost:8080/GeoNode",
             user = "admin", pwd = "GeoNode"
           )
}

}
\author{
Emmanuel Blondel <emmanuel.blondel1@gmail.com>
}
\keyword{GeoNode}
\keyword{api}
\keyword{rest}
\keyword{version}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{version}}{version}

\item{\code{value}}{value}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GeoNodeVersion-new}{\code{GeoNodeVersion$new()}}
\item \href{#method-GeoNodeVersion-lowerThan}{\code{GeoNodeVersion$lowerThan()}}
\item \href{#method-GeoNodeVersion-greaterThan}{\code{GeoNodeVersion$greaterThan()}}
\item \href{#method-GeoNodeVersion-equalTo}{\code{GeoNodeVersion$equalTo()}}
\item \href{#method-GeoNodeVersion-clone}{\code{GeoNodeVersion$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoNodeVersion-new"></a>}}
\if{latex}{\out{\hypertarget{method-GeoNodeVersion-new}{}}}
\subsection{Method \code{new()}}{
Initializes an object of class \link{GeoNodeVersion}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoNodeVersion$new(url, user, pwd)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{url}}{url}

\item{\code{user}}{user}

\item{\code{pwd}}{pwd}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoNodeVersion-lowerThan"></a>}}
\if{latex}{\out{\hypertarget{method-GeoNodeVersion-lowerThan}{}}}
\subsection{Method \code{lowerThan()}}{
Compares to a version and returns TRUE if it is lower, FALSE otherwise
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoNodeVersion$lowerThan(version)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{version}}{version}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{TRUE} if lower, \code{FALSE} otherwise
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoNodeVersion-greaterThan"></a>}}
\if{latex}{\out{\hypertarget{method-GeoNodeVersion-greaterThan}{}}}
\subsection{Method \code{greaterThan()}}{
Compares to a version and returns TRUE if it is greater, FALSE otherwise
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoNodeVersion$greaterThan(version)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{version}}{version}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{TRUE} if greater, \code{FALSE} otherwise
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoNodeVersion-equalTo"></a>}}
\if{latex}{\out{\hypertarget{method-GeoNodeVersion-equalTo}{}}}
\subsection{Method \code{equalTo()}}{
Compares to a version and returns TRUE if it is equal, FALSE otherwise
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoNodeVersion$equalTo(version)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{version}}{version}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{TRUE} if equal, \code{FALSE} otherwise
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GeoNodeVersion-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GeoNodeVersion-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GeoNodeVersion$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
