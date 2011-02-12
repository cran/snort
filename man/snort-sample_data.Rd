\name{snort-sample_data}
\docType{data}
\alias{snort-sample_data}
\title{SNORT Sample Data}
\description{These sample files can be used to test SNORT, they are in snort/extdata.}
\format{Four csv files:
\enumerate{
\item sample_ccl_spec.csv (4 fields, 3 rows)
\item sample_t&c_spec.csv (2 fields, 8 rows)
\item sample_delegates_2004.csv (12 fields, 275 rows)
\item sample_delegates_2005.csv (10 fields, 53 rows)
}
}
\details{
\emph{
\bold{File Structure
}
}

\bold{Column Cross Link Specification}

file: sample_ccl_spec.csv
\itemize{
\item Table 1
\item Column 1
\item Table 2
\item Column 2
}

\bold{Table and Column Specification}

file: sample_t&c_spec.csv
\itemize{
\item Table
\item Column
}

\bold{Base Table}

file: sample_delegates_2004.csv
\itemize{
\item ID
\item LastName
\item Title
\item FirstName
\item Organisation
\item Position
\item Suburb
\item State
\item Postcode
\item WorkPhone
\item EmailAddress
\item Industry
}

\bold{Comparison Table}

file: sample_delegates_2005.csv
\itemize{
\item TITLE
\item FIRSTNAME
\item LASTNAME
\item POSITION
\item COMPANY
\item CITY
\item STATE
\item ZIP
\item PHONE
\item EMAIL
}
}

\author{Eugene Dubossarsky and Mark Norrie}
\note{
The data files are completely synthetic.

N.B. Use these files as a template to process other data.
}
\keyword{Datasets}
