#' Diversion Ratio Calculator
#'
#' Calculates hospital-level diversion ratios, once cells have been
#' defined.
#'
#' @param D Dataset of hospital discharges. Required variables:
#'   \itemize{
#'   \item cell: the id of the cell each observation has been allocated
#'   \item hosp_id: hospital identifier (numeric)
#'   \item hospital: name of hospital (string)
#'   \item sys_id: system identifier (numeric)
#'   \item system: name of system (string)
#'   \item party_ind: indicator for party hospitals
#'   \item adm: the number of observations represented by the observation,
#'   = 1 for all if each observation is one admission
#'   }
#' @param dropDegenerateCell logical; specifies how to treat cells with a
#' 100\% within-system share. If TRUE, observations in degenerate, 100\% share
#' cells with be ignored in the diversion ratio calculation. If FALSE,
#' any such individuals will be assigned to the outside option, but still
#' included in the denominator, so that the inside-option diversion will total
#' less than 100\%.
#'
#' @details The output is hospital-level diversions for party hospitals. For system
#' level diversion, let hosp_id and hospital be equal to corresponding
#' system-level identifiers. Patients are not allowed to divert to
#' within-system alternative hospitals.
#'
#' For more details see the example vignette by typing:
#' \code{vignette("semipar_example", package = "healthcare.antitrust")}
#' @export



##################################################################
# Diversion Ratio Calculator
##################################################################
# div_calc calculates hospital-level diversion ratios. (Check whether still need
# original div_calc function.) Generalized from before, so party systems need not
# be in order from 1,..,ns.

# Required inputs: cell, hosp_id, hospital, sys_id, system, party_ind, adm
# where party_ind is 1 party hospitals, zero otherwise
# and adm is the number of admissions represented by the observation. =1 for all if
# each observation is one admissions
# Note: this results in hospital level diversions for party hospitals. For system
# level diversion, let hosp_id and hospital be system-level identifiers.
# Patients are not allowed to divert to within-system alternative hospitals.

div_calc <- function(D, dropDegenerateCell = TRUE) {
  check <- unique(subset(D,select=c(hosp_id,hospital)))
  if (length(unique(check$hosp_id)) != length(check$hosp_id)) {warning('Error: hosp_id associated with multiple hospital names')}
  #if (length(unique(check$hospital)) != length(check$hospital)) {warning('Error: hospital name associated with multiple hosp_ids')}

  if (!"cell" %in% names(D)) {warning('Variable "cell" required in input dataset'); stop()}
  if (!"hosp_id" %in% names(D)) {warning('Variable "hosp_id" required in input dataset'); stop()}
  if (!"hospital" %in% names(D)) {warning('Variable "hospital" required in input dataset'); stop()}
  if (!"sys_id" %in% names(D)) {warning('Variable "sys_id" required in input dataset'); stop()}
  #if (!"system" %in% names(D)) {warning('Variable "system" required in input dataset'); stop()}
  if (!"party_ind" %in% names(D)) {warning('Variable "party_ind" required in input dataset'); stop()}
  if (!"adm" %in% names(D)) {warning('Variable "adm" required in input dataset'); stop()}


  iter <- 0
  D$party_sys_id <- D$party_ind*D$sys_id
  party_sys_list <- unique(D$party_sys_id[D$party_sys_id > 0])

  for (m in party_sys_list) {
    # Calculate cell-specific hospital diversion ratios
    y_hosp_cell = aggregate(D$adm,by=list(D$cell,D$hosp_id,D$hospital,D$party_sys_id),sum)
    names(y_hosp_cell) <- c("cell","hosp_id","hospital","party_sys_id","N_h")

    y_hosp_cell$N <- ave(y_hosp_cell$N_h,y_hosp_cell$cell, FUN = sum)
    y_hosp_cell$share_h <- y_hosp_cell$N_h/y_hosp_cell$N
    y_hosp_cell$share_m <- ave(y_hosp_cell$share_h,y_hosp_cell$cell, y_hosp_cell$party_sys_id, FUN = sum)
    y_hosp_cell$share_m[y_hosp_cell$party_sys_id != m] <- 0
    y_hosp_cell$share_m <- ave(y_hosp_cell$share_m,y_hosp_cell$cell, FUN = max)

    y_hosp_cell$share_h[y_hosp_cell$party_sys_id == m] <- 0 # set share to zero for system hospitals

    y_hosp_cell$div <- y_hosp_cell$share_h/(1-y_hosp_cell$share_m)


    # Calculate predicted hospital-cell admissions after hospital k exclusion
    system_hosp <- sort(unique(y_hosp_cell$hosp_id[y_hosp_cell$party_sys_id == m]))

    for (k in system_hosp) {
      print(paste0("Hosp Id: ", k))
      iter <- iter + 1

      y_hosp_cell$N_k <- 0
      y_hosp_cell$N_k[y_hosp_cell$hosp_id == k] <- y_hosp_cell$N_h[y_hosp_cell$hosp_id == k]
      y_hosp_cell$N_k <- ave(y_hosp_cell$N_k,y_hosp_cell$cell, FUN = max)

      y_hosp_cell$N_h_predict <- y_hosp_cell$N_h + y_hosp_cell$N_k*y_hosp_cell$div
      y_hosp_cell$N_h_predict[y_hosp_cell$party_sys_id == m] <- 0

      # Sum across cells
      y_hosp = aggregate(D$adm,by=list(D$hosp_id,D$hospital,D$party_sys_id),sum)
      names(y_hosp) <- c("hosp_id","hospital","party_sys_id","N_h")

      y_hosp$N_k <- 0
      y_hosp$N_k[y_hosp$hosp_id == k] <- y_hosp$N_h[y_hosp$hosp_id == k]
      y_hosp$N_k <- max(y_hosp$N_k)

      temp <- aggregate(y_hosp_cell$N_h_predict,by=list(y_hosp_cell$hosp_id),sum)
      names(temp) <- c("hosp_id","N_h_predict")

      y_hosp <- merge(y_hosp,temp)

      # Calculate hospital diversion ratios - two options for denom
      if (dropDegenerateCell == FALSE) {
        y_hosp$div <- (y_hosp$N_h_predict-y_hosp$N_h)/y_hosp$N_k
      }
      if (dropDegenerateCell == TRUE) {
        y_hosp$movers <- y_hosp$N_h_predict - y_hosp$N_h
        y_hosp$N_k_alt <- sum(y_hosp$movers[y_hosp$movers>0])
        y_hosp$div <- (y_hosp$N_h_predict-y_hosp$N_h)/y_hosp$N_k_alt
      }


      y_hosp$div[y_hosp$party_sys_id == m] <- NA

      # Print flag if degenerate cells
      degenlist <- y_hosp_cell$cell[is.na(y_hosp_cell$div) & y_hosp_cell$hosp_id == k]
      if (length(degenlist) > 0) {
        print("Note the following cells are degenerate:")
        print(degenlist)

        totdiv <- sum(y_hosp$div, na.rm = TRUE)
        print(paste0("Total Diversion: ",totdiv))
      }

      if (iter == 1) {out <- subset(y_hosp, select=c(hosp_id,hospital,party_sys_id,N_h))}

      #out[,paste0("div_",m,"_",k)] <- y_hosp$div
      out[,paste0("div_from_",k)] <- y_hosp$div

    }

  }

  return(out)
}


