#' SSMethod.TA1.8.FC
#' \code{SSMethod.TA1.8.FC} Execute Francis CPUE weighting and return actual data frame.
#' @param summaryoutput an object generated via SS_output
#' @param standardized Should the test be performed using Unstandardized (0), Standardized (1) or both types of residuals (2)
#' @param type inputs to SSmethod.TA1.8
#' @param fleet inputs to SSmethod.TA1.8
#' @seealso \code{\link[r4ss]}

getTA1 <- function (fit, type, fleet, part = 0:2, sexes = 0:3, seas = NULL,
          method = NULL, plotit = TRUE, printit = TRUE, datonly = FALSE,
          plotadj = !datonly, maxpanel = 1000, fleetnames = NULL, label.part = TRUE,
          label.sex = TRUE, set.pars = TRUE)
{
  is.in <- function(x, y) !is.na(match(x, y))
  if (!is.in(type, c("age", "len", "size", "con"))) {
    stop("Illegal value for type (should be \"age\", \"len\", \"size\", or \"con\")")
  }
  else {
    if (sum(!is.in(sexes, c(0:3))) > 0) {
      stop("Unrecognised value for sexes")
    }
  }
  if (is.null(fleetnames)) {
    fleetnames <- fit$FleetNames
  }
  else {
    if (length(fleetnames) != fit$nfleets) {
      stop("fleetnames needs to be NULL or have length = nfleets = ",
           fit$nfleets)
    }
  }
  dbase <- fit[[paste(type, "dbase", sep = "")]]
  sel <- is.in(dbase$Fleet, fleet) & is.in(dbase$Part, part)
  if (type != "con") {
    names(dbase)[names(dbase) == "Pick_sex"] <- "Sexes"
    sel <- sel & is.in(dbase$Sexes, sexes)
  }
  if (type == "size" & !is.null(method)) {
    sel <- sel & is.in(dbase$method, method)
  }
  if (sum(sel) == 0)
    return()
  dbase <- dbase[sel, ]
  if (is.null(seas)) {
    seas <- "comb"
    if (length(unique(dbase$Seas)) > 1)
      cat("Warning: combining data from multiple seasons\n")
  }
  if (type == "size") {
    if (length(unique(dbase$units)) > 1) {
      cat("Warning: mix of units being compared:", unique(dbase$units),
          "\n")
    }
  }
  partitions <- sort(unique(dbase$Part))
  partition.labels <- c("whole", "discarded", "retained")[partitions +
                                                            1]
  partition.labels <- paste0("(", paste(partition.labels, collapse = "&"),
                             " catch)")
  indx <- paste(dbase$Fleet, dbase$Yr, if (type == "con")
    dbase$Lbin_lo
    else "", if (seas == "sep")
      dbase$Seas
    else "")
  sex.flag <- type != "con" & max(tapply(dbase$Sexes, dbase$Fleet,
                                         function(x) length(unique(x)))) > 1
  if (sex.flag) {
    indx <- paste(indx, dbase$Sexes)
  }
  method.flag <- type == "size" && length(unique(dbase$method)) >
    1
  if (method.flag) {
    indx <- paste(indx, dbase$method)
  }
  uindx <- unique(indx)
  if (length(uindx) == 1) {
    cat("Warning: only one point to plot\n")
    return()
  }
  pldat <- matrix(0, length(uindx), 10, dimnames = list(uindx,
                                                        c("Obsmn", "Obslo", "Obshi", "semn", "Expmn", "Std.res",
                                                          "ObsloAdj", "ObshiAdj", "Fleet", "Yr")))
  if (type == "con")
    pldat <- cbind(pldat, Lbin = 0)
  if (sex.flag)
    pldat <- cbind(pldat, sexes = 0)
  if (type == "size") {
    pldat <- cbind(pldat, method = 0)
    plunits <- rep(NA, nrow(pldat))
  }
  for (i in 1:length(uindx)) {
    subdbase <- dbase[indx == uindx[i], ]
    xvar <- subdbase$Bin
    pldat[i, "Obsmn"] <- sum(subdbase$Obs * xvar)/sum(subdbase$Obs)
    pldat[i, "Expmn"] <- sum(subdbase$Exp * xvar)/sum(subdbase$Exp)
    pldat[i, "semn"] <- sqrt((sum(subdbase$Exp * xvar^2)/sum(subdbase$Exp) -
                                pldat[i, "Expmn"]^2)/mean(subdbase$N))
    pldat[i, "Obslo"] <- pldat[i, "Obsmn"] - 2 * pldat[i,
                                                       "semn"]
    pldat[i, "Obshi"] <- pldat[i, "Obsmn"] + 2 * pldat[i,
                                                       "semn"]
    pldat[i, "Std.res"] <- (pldat[i, "Obsmn"] - pldat[i,
                                                      "Expmn"])/pldat[i, "semn"]
    pldat[i, "Fleet"] <- mean(subdbase$Fleet)
    pldat[i, "Yr"] <- mean(if (seas == "comb")
      subdbase$Yr
      else subdbase$Yr.S)
    if (type == "con")
      pldat[i, "Lbin"] <- mean(subdbase$Lbin_lo)
    if (sex.flag)
      pldat[i, "sexes"] <- mean(subdbase$Sexes)
    if (type == "size") {
      pldat[i, "method"] <- mean(subdbase$method)
      plunits[i] <- subdbase$units[1]
    }
  }
  Nmult <- 1/var(pldat[, "Std.res"], na.rm = TRUE)
  for (i in 1:length(uindx)) {
    pldat[i, "ObsloAdj"] <- pldat[i, "Obsmn"] - 2 * pldat[i,
                                                          "semn"]/sqrt(Nmult)
    pldat[i, "ObshiAdj"] <- pldat[i, "Obsmn"] + 2 * pldat[i,
                                                          "semn"]/sqrt(Nmult)
  }
  Nfleet <- length(unique(pldat[, "Fleet"]))
  if (plotit) {
    plindx <- if (type == "con") {
      paste(pldat[, "Fleet"], pldat[, "Yr"])
    }
    else {
      pldat[, "Fleet"]
    }
    if (sex.flag)
      plindx <- paste(plindx, pldat[, "sexes"])
    if (method.flag)
      plindx <- paste(plindx, pldat[, "method"])
    uplindx <- unique(plindx)
    Npanel <- length(uplindx)
    NpanelSet <- min(length(uplindx), maxpanel)
    Nr <- ceiling(sqrt(NpanelSet))
    Nc <- ceiling(NpanelSet/Nr)
    if (set.pars) {
      par_current <- par()
      par(mfrow = c(Nr, Nc), mar = c(2, 2, 1, 1) + 0.1,
          mgp = c(0, 0.5, 0), oma = c(1.2, 1.2, 0, 0),
          las = 1)
      par(cex = 1)
    }
    for (i in 1:Npanel) {
      subpldat <- pldat[plindx == uplindx[i], , drop = FALSE]
      x <- subpldat[, ifelse(type == "con", "Lbin", "Yr")]
      plot(x, subpldat[, "Obsmn"], pch = "-", xlim = if (length(x) >
                                                         1)
        range(x)
        else c(x - 0.5, x + 0.5), ylim = range(subpldat[,
                                                        c("Obslo", "Obshi", "ObsloAdj", "ObshiAdj", "Expmn")],
                                               finite = TRUE, na.rm = TRUE), xlab = "", ylab = "")
      segments(x, subpldat[, "Obslo"], x, subpldat[, "Obshi"],
               lwd = 3, lend = 3)
      if (plotadj) {
        arrows(x, subpldat[, "ObsloAdj"], x, subpldat[,
                                                      "ObshiAdj"], lwd = 1, length = 0.04, angle = 90,
               code = 3)
      }
      points(x, subpldat[, "Obsmn"], pch = 21, bg = "grey80")
      ord <- order(x)
      if (!datonly) {
        if (length(x) > 1) {
          lines(x[ord], subpldat[ord, "Expmn"], col = 4)
        }
        else {
          lines(c(x - 0.5, x + 0.5), rep(subpldat[, "Expmn"],
                                         2), col = 4)
        }
      }
      fl <- fleetnames[subpldat[1, "Fleet"]]
      yr <- paste(subpldat[1, "Yr"])
      lab <- if (type == "con")
        ifelse(Nfleet > 1, paste(yr, fl), yr)
      else fl
      if (sex.flag & label.sex) {
        lab <- paste(lab, ifelse(subpldat[1, "sexes"] ==
                                   0, "comb", "sex"))
      }
      if (method.flag) {
        lab <- paste(lab, "meth", subpldat[1, "method"])
      }
      if (label.part) {
        lab <- paste(lab, partition.labels)
      }
      mtext(lab, side = 3, at = mean(x))
    }
    ylab <- "Mean age"
    if (type == "len") {
      ylab <- "Mean length"
    }
    if (type == "size") {
      units <- unique(plunits[plindx %in% uplindx])
      if (length(units) == 1) {
        if (units %in% c("kg", "lb")) {
          ylab <- paste0("Mean weight (", units, ")")
        }
        if (units %in% c("cm", "in")) {
          ylab <- paste0("Mean length (", units, ")")
        }
      }
      else {
        ylab <- paste0("Mean value (", paste(units, collapse = " or "),
                       ")")
      }
    }
    mtext(ylab, side = 2, las = 0, outer = TRUE)
    mtext(ifelse(type == "con", "Length", "Year"), side = 1,
          outer = TRUE)
    if (set.pars) {
      par(mfrow = par_current$mfrow, mar = par_current$mar,
          mgp = par_current$mgp, oma = par_current$oma,
          las = par_current$las)
    }
  }
  if (!datonly) {
    tmp <- matrix(sample(pldat[, "Std.res"], 1000 * nrow(pldat),
                         replace = TRUE), nrow(pldat))
    confint <- as.vector(quantile(apply(tmp, 2, function(x) 1/var(x,
                                                                  na.rm = TRUE)), c(0.025, 0.975), na.rm = TRUE))
    Output <- c(w = Nmult, lo = confint[1], hi = confint[2])
    Outs <- paste("Francis Weights - ", type, ": ", fleetnames[fleet],
                  ": ", round(Nmult, 4), " (", round(confint[1], 4),
                  "-", round(confint[2], 4), ")", sep = "")
    if (printit) {
      print(Outs)
    }
    # return(Output)
    return(pldat)
  }
}
