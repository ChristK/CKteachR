% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{make_basic_validator_gs4}
\alias{make_basic_validator_gs4}
\title{A simple validity function for use with id and password pairs stored in a
Googlesheet}
\usage{
make_basic_validator_gs4(user_credentials, instructor_secret = NULL)
}
\arguments{
\item{user_credentials}{A Googlesheet from \code{\link[googlesheets4:range_read]{googlesheets4::read_sheet()}})}

\item{instructor_secret}{A password to be used by the instructor to download
students' progress.}
}
\description{
This simple validity is inspired by \code{\link[submitr:make_basic_validator]{submitr::make_basic_validator()}}
function from the submitr package. The function is adequate for basic
authoring and testing needs. It reads login credentials stored in a
Googlesheet provided that you have received a decryption token (see
\link{decrypt_gs4_token}).
}
