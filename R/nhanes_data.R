#' 2013-2014 cycle of NHANES data
#'
#'
#' The data was extracted from The National Center for Health Statistics (NCHS)
#' in the Centers for Diseases Control and Prevention (CDC) and has been processed to a sparse matrix for memory efficiency.
#'
#'
#'
#'
#'
#' @docType data
#'
#' @usage data(nhanes2013_sbc)
#'
#' @format An object of class \code{data.frame} with 1510 obs. of 66 variables including
#' \describe{
#'     \item{HBP}{Binary response variable labeled as `1` for hypertension or high blood pressure. }
#'     \item{SEQN}{Respondent sequence number.}
#'     \item{SDDSVYR}{Data Release Number indicating survey year.}
#'     \item{SDMVPSU}{Masked Variance Pseudo-PSU, `cluster`.}
#'     \item{SDMVSTRA}{Masked Variance Pseudo-Stratum, `strata`.}
#'     \item{WTSAF2YR}{Fasting Subsample 2-year MEC Weight, `weights`.}
#' }
#' @keywords data
#' @source <https://www.cdc.gov/nchs/nhanes/about_nhanes.htm>
#' @examples
#' data(nhanes2013_sbc)
"nhanes2013_sbc"
