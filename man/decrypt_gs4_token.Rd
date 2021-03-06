% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{decrypt_gs4_token}
\alias{decrypt_gs4_token}
\title{Decrypts the Google token that is included in this package}
\usage{
decrypt_gs4_token(
  private_key_path = file.path(getwd(), "id_rsa"),
  gs4_token_decr_folder = getwd(),
  force = FALSE
)
}
\arguments{
\item{private_key_path}{The path to the private key. By default a key named
id_rsa in the current working directory.}

\item{gs4_token_decr_folder}{The folder where the decrypted token will be
created. By default  in the current working directory.}

\item{force}{If \code{TRUE} and a file named gs4_token_decr.json already exists in
gs4_token_decr_folder then the existing file is overwritten.}
}
\description{
If you have received a decryption key by the author, then use the function to
get access to the relevant Googlesheets. If you have received one then please
copy it to your project directory. It only need to be used once per project.
Note that this allows for the author to monitor students' progress throughout
the tutorials but the tutorials can also run without these functionality
enabled. In such a case a local file will be created to monitor progress.
}
