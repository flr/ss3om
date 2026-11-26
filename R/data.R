# data.R - Documentation of the datasets included in ss3om
# ss3om/R/data.R

# Copyright European Union, 2015-2019; WMR, 2020.
# Author: Iago Mosqueira (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' Stock Synthesis output for the sma example model
#'
#' A list with the output of a Stock Synthesis (SS3) run, as returned by
#' `r4ss::SS_output`, including the covariance matrix (`CoVar`) and the
#' `derived_quants` table used by `ssmvln`.
#'
#' @format A list of 181 elements, as returned by `r4ss::SS_output`.
#' @name sma
#' @docType data
#' @keywords datasets
#' @examples
#' data(sma)
#' head(sma$derived_quants)
NULL

#' Stock Synthesis output for the sol example model
#'
#' A list with the output of a Stock Synthesis (SS3) run, as returned by
#' `r4ss::SS_output`, for a four-fleet model covering the years 1957 to 2023.
#'
#' @format A list of 189 elements, as returned by `r4ss::SS_output`.
#' @name sol
#' @docType data
#' @keywords datasets
#' @examples
#' data(sol)
#' head(sol$derived_quants)
NULL
