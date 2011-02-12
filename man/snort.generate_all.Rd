\name{snort.generate_all}
\alias{snort.generate_all}
\title{Generate All Pajek data}
\description{This code generates the Network data in Pajek format.}
\usage{
snort.generate_all(homedir, input1, input2, logfile, entityfile, triplefile)
}
\arguments{
  \item{homedir}{e.g. /home/mark/Desktop/Eugene/Test}
  \item{input1}{e.g. "sample_t&c_spec.csv"}
  \item{input2}{e.g. "sample_ccl_spec.csv"}
  \item{logfile}{e.g. "snortlog.log"}
  \item{entityfile}{e.g. "entityfile.dta"}
  \item{triplefile}{e.g. "tripfile.net"}
}
\author{Eugene Dubossarsky and Mark Norrie}
\examples{
\dontrun{

snort.generate_all(homedir = "/home/mark/Desktop/Eugene/Test",
input1 = "sample_t&c_spec.csv",
input2 = "sample_ccl_spec.csv",
logfile = "snortlog.log",
entityfile = "entityfile.dta",
triplefile = "tripfile.net")

}
}
\keyword{Data Functions}
