# CKteachR is an R package with R tutorials for beginners.
# Copyright (C) 2020 Chris Kypridemos
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' A simple validity function for use with id and password pairs stored in a
#' Googlesheet
#'
#' This simple validity is inspired by [submitr::make_basic_validator()]
#' function from the submitr package. The function is adequate for basic
#' authoring and testing needs. It reads login credentials stored in a
#' Googlesheet provided that you have received a decryption token (see
#' [CKteachR::decrypt_gs4_token]).
#' @param user_credentials A Googlesheet from [googlesheets4::read_sheet()])
#' @param instructor_secret A password to be used by the instructor to download
#'   students' progress.
#' @export
make_basic_validator_gs4 <- function(user_credentials, instructor_secret = NULL) {
  if (is.null(instructor_secret))
    stop("Must provide an instructor secret, e.g., 'hello'")
  res <- function(user, mode, secret = "default") {
    user <- trimws(user)
    mode <- trimws(mode)
    secret <- trimws(secret)
    if (mode == "q") {
      return(TRUE)
    }
    else if (mode == "i") {
      return(user == "instructor" && secret == instructor_secret)
    }
    else if (mode == "u") {
      return(user %in% user_credentials$id)
    }
    else if (mode == "p") {
      cat("Validator in p mode with secret:", secret,
          "\n")
      ind <- which(user == user_credentials$id)
      return(user_credentials$password[ind] == secret)
    }
    FALSE
  }
  res
}


#' Decrypts the Google token that is included in this package
#'
#' If you have received a decryption key by the author, then use the function to
#' get access to the relevant Googlesheets. If you have received one then please
#' copy it to your project directory. It only need to be used once per project.
#' Note that this allows for the author to monitor students' progress throughout
#' the tutorials but the tutorials can also run without these functionality
#' enabled. In such a case a local file will be created to monitor progress.
#' @param private_key_path The path to the private key. By default a key named
#'   id_rsa in the current working directory.
#' @param gs4_token_decr_folder The folder where the decrypted token will be
#'   created. By default  in the current working directory.
#' @param force If `TRUE` and a file named gs4_token_decr.json already exists in
#'  gs4_token_decr_folder then the existing file is overwritten.
#' @export
decrypt_gs4_token <-
  function(private_key_path = file.path(getwd(), "id_rsa"),
           gs4_token_decr_folder = getwd(),
           force = FALSE) {
    on.exit(if (file.exists(tt))
      file.remove(tt))
    # set useful paths
    tt <- tempfile(fileext = ".encryptr.bin")
    gs4_token_decr_path <-
      file.path(gs4_token_decr_folder, "gs4_token_decr.json")

    # file exists logic
    if (file.exists(gs4_token_decr_path)) {
      if (force) {
        file.remove(gs4_token_decr_path)
      } else {
        message("Googlesheets token already exists. If you would like to replace it use force = TRUE")
        return()
      }
    }

    # decryption
    saveRDS(gs4_token_enc, tt)
    encryptr::decrypt_file(tt,
                           file_name = gs4_token_decr_path,
                           private_key_path = private_key_path)
    return ()
  }


#' List the tutorials that are included in CKteachR
#' @export
list_tutorials <- function() {
  list.dirs(system.file("tutorials", package = "CKteachR"), full.names = FALSE,
            recursive = FALSE)
}


#' Loads an R tutorial included in CKteachR
#'
#' This function is a wrapper for the [learnr::run_tutorial()] function from the
#' learnr package. See the documentation and vignettes of that package.
#'
#' @param tutorial_name The tutorial name. See [list_tutorials()] for a list of
#' available tutorials.
#' @param tutorial_shiny_args This pass directly to the [learnr::run_tutorial()]
#' function from the learnr package.
#' @export
load_tutorial <-
  function(tutorial_name, tutorial_shiny_args = NULL) {
    learnr::run_tutorial(name = tutorial_name,
                         package = "CKteachR",
                         shiny_args = tutorial_shiny_args)
  }




#' Sets up the progress monitoring environment
#'
#' This function sets up the progress monitoring environment. If a secret key
#' exists in the working dir but not an decrypted token, a decrypted token is
#' created. If a decrypted token exists
#' @param working_dir The path to the working directory.
#' @param tutorial_name The tutorial_name.
#' @param students_gs4_ident The Googlesheet ID with student credentials.
#' @param mark_gs4_ident The Googlesheet ID that will record students' progress.
#' @param mark_gs4_email The email linked to the Google account
#' @return A list with the necessary functions for `submitr::shiny_logic()`.
#' @export
setup_progress_monitoring <-
  function(working_dir,
           tutorial_name,
           students_gs4_ident,
           mark_gs4_ident,
           mark_gs4_email) {
    token_path <- file.path(working_dir, "gs4_token_decr.json")
    key_path <- file.path(working_dir, "gs4_token_decr.json")
    student_credential_path <- file.path(working_dir, "student_credential_path.csv")
    student_record_sheet <- file.path(working_dir,
                                      paste0(tutorial_name, "_submission.csv"))

     # if token does not exists but decryption key does, create a decrypted token
    if (!file.exists(token_path) && file.exists(key_path)) {
      decrypt_gs4_token(key_path, working_dir)
    }

    out <- list()

    if (file.exists(token_path)) {
      message("A token was found. Googlesheets is now used to record progress.")
      googlesheets4::gs4_auth(path = token_path)
      out$vfun <-
        make_basic_validator_gs4(googlesheets4::read_sheet(students_gs4_ident), instructor_pass)
      out$storage_actions <-
        submitr::record_gs4(key = mark_gs4_ident,
                            email = mark_gs4_email,
                            out$vfun)
    } else {
      message("Token was not found. A local file will be used to record progress.
              Please provide your username and password if you haven't before.")

      # create file with student credentials if not created before
      if (!file.exists(student_credential_path)) {
        id <- rstudioapi::askForPassword("Please enter your name")
        password <- rstudioapi::askForPassword("Please enter your password")
        utils::write.csv(data.frame(id = id, password = password),
                  student_credential_path, row.names = FALSE)
      }

      out$vfun <-
        submitr::make_basic_validator(student_credential_path, instructor_pass)
      out$storage_actions <- submitr::record_local(student_record_sheet)

    }
    out
  }
