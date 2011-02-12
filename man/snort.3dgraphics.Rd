\name{snort.3dgraphics}
\alias{snort.3dgraphics}
\title{3D Graphics VRML production}
\description{This code produces a 3-Dimensional Interactive VRML model to explore the generated network.}
\usage{
snort.3dgraphics(homedir, triplefile, displabels, labelcolor, edgecolor,
    vertexcolor, backgrcolor)
}
\arguments{
  \item{homedir}{e.g. /home/mark/Desktop/Eugene/Test}
  \item{triplefile}{e.g. tripfile.net}
  \item{displabels}{TRUE or FALSE}
  \item{labelcolor}{e.g. "green"}
  \item{edgecolor}{e.g. "mediumseagreen"}
  \item{vertexcolor}{e.g. "yellow"}
  \item{backgrcolor}{e.g. "black"}
}
\author{Eugene Dubossarsky and Mark Norrie}
\examples{
\dontrun{

## Example 1:
snort.3dgraphics(homedir = "/home/mark/Desktop/Eugene/Test",
triplefile = "tripfile.net",
displabels = TRUE,
labelcolor = "green",
edgecolor = "mediumseagreen",
vertexcolor = "yellow",
backgrcolor =  "black")

## Example 2:
snort.3dgraphics(homedir = "/home/mark/Desktop/Eugene/Test",
triplefile = "tripfile2.net",
displabels = TRUE,
labelcolor = "gold",
edgecolor = "mediumseagreen",
vertexcolor = "red",
backgrcolor =  "black")

}
}
\keyword{Graphic Functions}
