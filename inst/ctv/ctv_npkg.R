if(FALSE) {
  ## get number of packages
  psy <- ctv_npkg("Psychometrics")
  
  ## smooth some fluctions (e.g., due to CRAN archivals
  ## and turn into monthly ts
  psy2 <- as.ts(aggregate(psy, zoo::as.yearmon, tail, 1))
  psy2 <- zoo::na.locf(psy2)
  
  ## visualization
  plot(psy2[, 1:2], plot.type = "single", col = c(1, 4),
    log = "y", ylim = c(10, 125), ylab = "Number of packages")
  legend("topleft", c("All", "Core"), lty = 1, col = c(1, 4), bty = "n")
}

ctv_npkg <- function(view, ctvroot = ".")
{
  ## change directory to ctvroot
  owd <- getwd()
  setwd(ctvroot)
  on.exit(setwd(owd))

  ## determine .ctv files
  if(substr(view, nchar(view) - 3L, nchar(view)) != ".ctv") view <- paste0(view, ".ctv")

  ## obtain most recent log file to extract revision numbers and corresponding dates
  system("svn update")
  svn_log <- system(paste("svn log", view), intern = TRUE)
  svn_revision_info <- strsplit(svn_log[head(which(
    svn_log == "------------------------------------------------------------------------") + 1L, -1L)],
    " | ", fixed = TRUE)
  svn_revision_number <- sapply(svn_revision_info, function(x) as.numeric(substr(x[1L], 2L, nchar(x[1L]))))
  svn_revision_date <- do.call("c", lapply(svn_revision_info, function(x) as.Date(x[3L])))

  ## extract number of packages for each revision
  npkg <- t(sapply(svn_revision_number, function(i) {
    system(paste("svn update -r", i, view))
    p <- try(get_ctv_pkglist(view)[, 2L])
    if(!inherits(p, "try-error")) return(c(length(p), sum(p), sum(!p))) else rep(NA, 3L)
  }))
  colnames(npkg) <- c("all", "core", "other")

  ## go back to head revision
  system("svn update")

  ## set up zoo object
  zoo::zoo(npkg, svn_revision_date)
}

get_ctv_pkglist <- function(file)
{
  ## read raw XML
  x <- XML::xmlTreeParse(file)
  if(XML::xmlSize(x$doc$children) > 1) warning("ctv should contain only one view")
  x <- XML::xmlChildren(x$doc$children$CRANTaskView)

  ## extraction functions
  package1 <- function(x) {
    rval <- XML::xmlAttrs(x)["priority"]
    rval <- if(!is.null(rval) && rval == "core") "core" else "normal"
    as.vector(c(XML::xmlValue(x), rval))
  }
  packagelist <- function(x) {
    rval <- t(sapply(XML::xmlChildren(x$packagelist), package1))
    colnames(rval) <- NULL
    rownames(rval) <- NULL
    rval <- data.frame(name = I(rval[,1]), core = rval[,2] == "core")
    rval[order(tolower(rval[,1])), ]
  }
  packagelist(x)
}
