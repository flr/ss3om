# ss3slots.R - DESC
# ss3om/R/ss3slots.R

# Copyright European Union, 2015-2019; WMR, 2020.
# Author: Iago Mosqueira (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' @rdname ss3slot
#' @aliases ss3mat30
#' @details - `ss3mat30` returns the `mat` slot.

ss3mat30 <- function(endgrowth, dmns, spawnseas, option=3) {

  # EXTRACT mat - endgrowth
  mat <- endgrowth[, .(unit, Seas, Age, Age_Mat, `Mat*Fecund`, Wt_Beg,
    Mat_F_wtatage, Mat_F_Natage, Len_Mat)]

  # maturity option 6: mat=Mat*Fecund / max(Mat*Fecund)
  if(option == 6)
    #mat[, mat:= `Mat*Fecund` / max(`Mat*Fecund`), by=.(unit, Seas)]
    mat[, mat:= `Mat*Fecund`]
  
  # maturity option 3: mat=Age_Mat
  if(option == 3)
    mat[, mat:= Age_Mat]

  # maturity option 4: mat=Mat*Fecund / Wt_Beg
  if(option == 4)
    mat[, mat:= `Mat*Fecund` / Wt_Beg]

  # maturity option 1: mat=Len_mat
  if(option == 1)
    mat[, mat:= Len_Mat]

  # DEBUG
  if(option == 2)
    mat[, mat:= Age_Mat]
  
  # DEBUG
  if(option == 5)
    mat[, mat:= 0]
  
  # DEBUG
  if(!option %in% c(3, 4, 6, 1, 2, 5))
    stop(paste0("maturity option ", option, " not covered yet, pleaase fill an issue"))

  # DELETE columns
  mat[ ,`:=`(Age_Mat = NULL, `Mat*Fecund` = NULL, Wt_Beg = NULL,
    Mat_F_wtatage = NULL, Mat_F_Natage = NULL, Len_Mat = NULL)]

  # SET mat by unit / Seas
  if(all(mat[, unique(unit)] == mat[, unique(Seas)])) {
    mat[Seas != unit, mat := 0]
  }
  # CHECK: SET mat out of Spawn_seas to 0
  # mat[!Seas %in% spawnseas, mat:=0]

  # RENAME
  names(mat) <- c("unit", "season", "age", "data")

  # TURN -1/NaN to 0
  mat[, data:=ifelse(is.nan(data), 0, data)]
  mat[, data:=ifelse(data==-1, 0, data)]

  # EXPAND by year & unit & area
  mat <- FLCore::expand(as.FLQuant(mat[, .(season, unit, age, data)],
    units=""), year=dmns$year, unit=dmns$unit, season=dmns$season,
    area=dmns$area)

  return(mat)
}

#' @rdname ss3slot
#' @aliases ss3m
#' @details - `ss3m` returns the `m` slot.

ss3m30 <- function(endgrowth, dmns, birthseas) {
  
  # EXTRACT m - biol[, Seas, BirthSeas, Age, M]
  m <- endgrowth[, .(Age, unit, Seas, M)]

  # SPLIT M across seasons
  m[, M:=M/length(dmns$season)]

  # RENAME
	names(m) <- c("age", "unit", "season", "data")
  
  # EXPAND by year, unit, season & area
  # BUG expand not filling
  m <- FLCore::expand(as.FLQuant(m[,.(season, age, data, unit)], units="m"),
    year=dmns$year, unit=dmns$unit, season=dmns$season, area=dmns$area)

  return(m)
}

#' @rdname ss3slot
#' @aliases ss3mage30
#' @param M_at_age A data frame obtained from SS_output$M_at_age.
#' @param m The `m` slot returned by `ss3m30`, used as the fallback.
#' @details - `ss3mage30` returns the `m` slot from `M_at_age` when natural
#' mortality varies over years. `endgrowth` only holds the biology of the final
#' year, so `ss3m30` copies the last year's M to every year. If M does not vary
#' over years, or `M_at_age` cannot be mapped onto the stock dimensions (e.g.
#' several growth patterns or birth seasons as units), `m` is returned unchanged.
#' SS3 reports M for the plus group as NA; the oldest age with a value is used.

ss3mage30 <- function(M_at_age, dmns, m) {

  if(is.null(M_at_age) || !is.data.frame(M_at_age) || nrow(M_at_age) == 0)
    return(m)

  ma <- data.table(M_at_age)
  ages <- dmns$age
  yrs <- as.numeric(dmns$year)

  # CHECK ages, years and units can be matched
  if(!all(ages %in% names(ma)) || !all(yrs %in% ma$Yr))
    return(m)
  if(length(dmns$unit) != length(unique(ma$Sex)))
    return(m)

  ma[, unit := dmns$unit[Sex]]
  ma <- ma[Yr %in% yrs, c("unit", "Yr", ages), with=FALSE]

  # one row per unit and year (several growth patterns not handled)
  if(anyDuplicated(ma[, .(unit, Yr)]) > 0)
    return(m)

  # MELT, ages in order (age 0 can come out as integer)
  ma[, (ages) := lapply(.SD, as.numeric), .SDcols=ages]
  ma <- data.table::melt(ma, id.vars=c("unit", "Yr"), variable.name="age",
    value.name="data")
  ma[, age := factor(as.character(age), levels=ages)]
  setorder(ma, unit, Yr, age)

  # FILL plus group (NA in M_at_age) from the previous age
  ma[, data := data.table::nafill(data, type="locf"), by=.(unit, Yr)]
  if(anyNA(ma$data))
    return(m)

  # RETURN m unchanged if M is constant over years
  vary <- ma[, .(r = diff(range(data))), by=.(unit, age)][, max(r)]
  if(vary < 1e-8)
    return(m)

  # SPLIT M across seasons, as ss3m30
  ma[, data := data / length(dmns$season)]

  # EXPAND by season & area
  ma <- ma[, .(age=as.character(age), year=as.character(Yr), unit, data)]
  grid <- CJ(season=dmns$season, area=dmns$area)
  ma <- merge(ma[, k := 1L], grid[, k := 1L], by="k",
    allow.cartesian=TRUE)[, k := NULL]
  res <- as.FLQuant(ma[, .(age, year, unit, season, area, data)], units="m")

  # SAME dimnames and order as m
  res <- res[dimnames(m)$age, dimnames(m)$year, dimnames(m)$unit,
    dimnames(m)$season, dimnames(m)$area]

  return(res)
}

#' @rdname ss3slot
#' @aliases ss3n
#' @param n A data frame obtained from SS_output$natage.
#' @details - `ss3n30` returns the `stock.n` slot.

ss3n30 <- function(n, dmns, era="TIME") {
  
  # SELECT start of season (Beg/Mid == 'B'), Era %in% era & cols
  n <- n[`Beg/Mid` == "B" & Era %in% era,
    .SD, .SDcols = c("Area", "unit", "Yr", "Seas", dmns$age)]

  # HACK 0 convert to double
  n[, `0`:=as.numeric(`0`)]

  # MELT by Sex, unit, Yr & Seas
	n <- data.table::melt(n, id.vars=c("Area", "unit", "Yr", "Seas"),
    variable.name="age")
  setorder(n, "Area", "Seas", "unit")
  
  # RENAME
  names(n) <- c("area", "unit", "year", "season", "age", "data")
  n <- as.FLQuant(n, units="1000")
  dimnames(n) <- dmns

  return(n)
}

#' @rdname ss3slot
#' @aliases ss3catch
#' @param catage A data frame obtained from SS_output$catage.
#' @param wtatage A data frame obtained from SS_output$endgrowth but subset for `birthseas` and `RetWt:_idx`.
#' @param idx The fishing fleets, as in `SS_output$fleet_ID[SS_output$IsFishFleet]`.
#' @details - `ss3catch` currently returns the `landings.n` slot, equal to `catch.n` as discards are not being parsed.

ss3catch30 <- function(catage, wtatage, dmns, birthseas, idx, col="RetWt",
  era = "TIME") {

  # FIND and SUBSET fishing fleets, TIME and BirthSeas
  catage <- catage[Fleet %in% idx & Era %in% era,]
 
  # RENAME Area and Season if only 1
  cols <- c("Seas", "Area")
  catage[, (cols) := lapply(.SD, as.character), .SDcols = cols]
  catage[, Seas := if(length(unique(Seas)) == 1) "all" else Seas]
  catage[, Area := if(length(unique(Area)) == 1) "unique" else Area]

  # CONVERT age columns to double
  catage[, (dmns$age) := lapply(.SD, as.double), .SDcols = dmns$age]

  # MELT by Sex, BirthSeas, Yr & Seas
	catage <- data.table::melt(catage, id.vars=c("Area", "Fleet", "Yr", "Seas",
    "unit"), measure.vars=dmns$age, variable.name="age")

  names(catage) <- c("area", "fleet", "year", "season", "unit", "age", "data")
  
  setorder(catage, "area", "fleet", "year", "season", "unit", "age", "data")

  # RENAME Area and Season if only 1
  cols <- c("Seas")
  wtatage[, (cols) := lapply(.SD, as.character), .SDcols = cols]
  wtatage[, Seas := if(length(unique(Seas)) == 1) "all" else Seas]

  # MELT by Sex, BirthSeas, Yr & Seas
  wtatage <- data.table::melt(wtatage, id.vars=c("Age", "unit", "Seas"),
    measure.vars=paste0(col, ":_", idx), variable.name="fleet")
  names(wtatage) <- c("age", "unit", "season", "fleet", "data")
  wtatage[,fleet:=sub(paste0(col, ":_"), "", fleet)]
  setorder(wtatage, "fleet", "season", "unit", "age")

  # FLQuants for catch per fleet
  catch <- lapply(idx, function(x) {
    catch.n <- as.FLQuant(catage[fleet %in% x,][, fleet:=NULL], units="1000")
    catch.wt <- do.call('expand',
      c(list(x=as.FLQuant(wtatage[fleet %in% x,][, fleet:=NULL], units="kg")),
      dimnames(catch.n)[c("year", "area")]))
    return(FLQuants(catch.n=catch.n, catch.wt=catch.wt))
    }
  )
  return(catch)
} 

ss3z30 <- function(zatage, dmns) {

  zaa <- zatage[Yr %in% dmns$year, -1]
  setnames(zaa, c("Sex", "Yr"), c("unit", "year"))

  # CONVERT class of last age column
  zaa[[dim(zaa)[2]]] <- as.numeric(NA)

  zatage <- data.table::melt(zaa, id.vars=c("unit", "year"),
    measure.vars=dmns$age, variable.name="age", value.name = "data")
  
  z <- as.FLQuant(zatage, units="z")
  dimnames(z) <- dmns[-4]

  z[dim(z)[1],] <- z[dim(z)[1] - 1,]

  return(z)
}

ss3wt30 <- function(endgrowth, dmns, birthseas) {
  
  # EXTRACT
  wt <- endgrowth[, list(Age, unit, Seas, Wt_Beg)]

  # RENAME
  names(wt) <- c("age", "unit", "season", "data")
  
  # EXPAND by year, unit & season
  return(FLCore::expand(as.FLQuant(wt, units="kg"),
    year=dmns$year, unit=dmns$unit, season=dmns$season, area=dmns$area))
}
