#' Willingness-to-Pay Calculator
#'
#' This function calculates the system-level Willingness-To-Pay.
#'
#' @param D Dataset of hospital discharges. Required variables:
#'   \itemize{
#'   \item cell: the id of the cell each observation has been allocated
#'   \item sys_id: system identifier (numeric)
#'   \item party_ind: indicator for party hospitals
#'   \item adm: the number of observations represented by the observation,
#'   = 1 for all if each observation is one admission
#'   \item weight: the designated DRG weight of admission; =1 if
#'   observations should be equally weighted.
#'   }
#'
#' @details This function calculates the system-level WTP. For use in a WTP
#'  simulation exercise of the "WTP/Q Method" described in Brand and Balan
#'  (2018).
#'  Hospital systems need to be numbered by sys_id, with distinct sys_id
#'  for all independent hospitals
#'  The functions outputs both weighted and unweighted WTP by system.
#'  For example, is it common to weight each observation by DRG weight.
#'  For only unweighted results, weight should be = 1 for all observations,
#'  then two measures will be equivalent.
#'
#' For more details see the example vignette by typing:
#' \code{vignette("semipar_example", package = "healthcare.antitrust")}
#' @importFrom stats aggregate ave
#' @export



##################################################################
# System WTP Calculator
##################################################################

# This function calculates the system-level WTP. For use in a WTP simulation exercise
# of the "WTP/Q Method".
# MAYBE should add system id to the output?

wtp_calc <- function(D) {
  D$totalweight <- D$weight*D$adm  # in case some obs are aggregated admissions

  y_cell <- aggregate(list(N_s=D$adm, wt = D$totalweight),by=list(cell=D$cell, sys_id=D$sys_id,party=D$party),sum)

  y_cell <- y_cell[order(y_cell$cell,y_cell$sys_id),]

  y_cell$N <- ave(y_cell$N_s,y_cell$cell, FUN = sum)
  y_cell$share_s <- y_cell$N_s/y_cell$N

  y_cell$wt <- ave(y_cell$wt,y_cell$cell, FUN = sum)
  y_cell$wt <- y_cell$wt/y_cell$N

  # Drop cells with only one hospital visited or do adjustment
  #y_cell <- subset(y_cell,y_cell$share_s < 1)
  y_cell$share_s[y_cell$share_s > .99] <- .99

  y_cell$WTP_s <- log(1/(1-y_cell$share_s))*y_cell$N
  y_cell$WTP_s_wt <- log(1/(1-y_cell$share_s))*y_cell$N*y_cell$wt

  y <- aggregate(list(WTP_s=y_cell$WTP_s, N_s=y_cell$N_s, WTP_s_wt=y_cell$WTP_s_wt),by=list(party=y_cell$party,sys_id=y_cell$sys_id),sum)

  return(y)
}


