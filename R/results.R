# results.R - DESC
# /results.R

# Copyright European Union, 2015-2019; WMR, 2020.
# Author: Iago Mosqueira (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# results {{{
#' Results of FLom and FLStock objects
#'
#' Get and set the `results` attribute of `FLom` and `FLStock` objects.
#'
#' @param x An object of class [FLom][mse::FLom-class] or [FLStock][FLCore::FLStock].
#' @param ... Not used.
#' @param value A `data.frame` with the results to store.
#'
#' @return The `data.frame` stored in the `results` attribute, or the modified object for `results<-`.
#' @name results
#' @rdname results
setGeneric("results", function(x, ...) standardGeneric("results"))

#' @rdname results
setMethod("results", signature(x="FLom"),
  function(x) {
    return(attr(x, "results"))
  })

#' @rdname results
setMethod("results", signature(x="FLStock"),
  function(x) {
    return(attr(x, "results"))
  })

#' @rdname results
setGeneric("results<-", function(x, ..., value) standardGeneric("results<-"))

#' @rdname results
setReplaceMethod("results", signature(x="FLom", value="data.frame"),
  function(x, value) {
    attr(x, "results") <- value
    return(x)
  })

#' @rdname results
setReplaceMethod("results", signature(x="FLStock", value="data.frame"),
  function(x, value) {
    attr(x, "results") <- value
    return(x)
  }) # }}}
