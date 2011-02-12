\name{snort.generate_collapsed}
\alias{snort.generate_collapsed}
\title{Generate Collapsed Network Pajek Data}
\description{This code generates the Collapsed Network data in Pajek format.}
\usage{
snort.generate_collapsed(homedir, input1, input2, logfile, entityfile, triplefile)
}
\arguments{
  \item{homedir}{e.g. /home/mark/Desktop/Eugene/Test}
  \item{input1}{e.g. "sample_t&c_spec.csv"}
  \item{input2}{e.g. "sample_ccl_spec.csv"}
  \item{logfile}{e.g. "snortlog2.log"}
  \item{entityfile}{e.g. "entityfile2.dta"}
  \item{triplefile}{e.g. "tripfile2.net"}
}
\author{Eugene Dubossarsky and Mark Norrie}
\examples{
\dontrun{

snort.generate_collapsed(homedir = "/home/mark/Desktop/Eugene/Test",
input1 = "sample_t&c_spec.csv",
input2 = "sample_ccl_spec.csv",
logfile = "snortlog2.log",
entityfile = "entityfile2.dta",
triplefile = "tripfile2.net")

}
}
\keyword{Data Functions}

