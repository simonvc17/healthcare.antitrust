#' Allocate Observations to Cells
#'
#' Take a dataset of hospital discharges, and allocate them to cells that
#' meet a minimum threshold cell size.
#'
#' @param D Dataset of discharges. The one required variable is adm, which
#'   indicates how many admissions are represented by the observation.
#' @param S_min Minimize cell size.
#' @param layers A list of lists. Each layer is a list of the variables
#'   on which observations will be allocated to cells. The layers should
#'   be ordered in decreasing refinement, so that observations not allocated
#'   to a cell meeting the minimum size threshold can be allocated by a
#'   coaser layer.
#' @export

# Here could also go Roxygen comments with example tags (or links to
# vignettes) and description of output. With tags @examples and @return.

cell_defn <- function(D, S_min, layers) {
  cc <- c("cell","cell_type")

  # Create ID numbers for cells based on layers of decreasing refinement
  L <- length(layers)      # Number of layers

  cell_def <- lapply(layers, function(x){unique(subset(D, select = x))})


  for (l in 1:L) {
    cell_def[[l]]$cell <- cumsum((array(1,nrow(cell_def[[l]]))))
    cell_def[[l]]$cell_type <- l
  }

  # Starting with the most refined layer, merge with discharge data.
  # Use this cell defn for cells with at least `thrs' events.
  # For cells with fewer than `thrs', go on to next level.

  # First Layer
  DD <- merge(D,cell_def[[1]],by=(list1))
  tmp <- aggregate(DD$adm,by=list(DD$cell,DD$cell_type),sum)
  names(tmp) <- c(cc,"cell_tot")
  DD <- merge(DD,tmp,by=cc)
  D0 <- subset(DD,cell_tot >= thrs)
  DD <- subset(DD,cell_tot < thrs)

  # Second through L Layers
  if (L > 1) {
  for (j in 2:L) {

    cellj<-cell_def[[j]];listj<-layers[[j]]

    DD[,cbind(cc[1],cc[2],"cell_tot")] <- list(NULL)

    DD <- merge(DD,cellj,by=(listj))
    tmp <- aggregate(DD$adm,by=list(DD$cell,DD$cell_type),sum)
    names(tmp) <- c(cc,"cell_tot")
    DD <- merge(DD,tmp,by=cc)

    D0 <- rbind(D0,subset(DD,cell_tot >= thrs))

    DD <- subset(DD,cell_tot < thrs)

  }
  }
  print("Number of Excluded Observations")
  print(nrow(DD))


  #  Create a new cell ID based on combinations of 'cell' and 'cell_type'
  cell_def <- as.data.frame(unique(cbind(D0$cell,D0$cell_type)))
  names(cell_def) <- cc
  cell_def$cell1 <- cumsum((array(1,nrow(cell_def))))
  D0 <- merge(D0,cell_def,by=cc)
  D0$cell<-D0$cell1

  D0[,cbind("cell1","cell_tot")] <- list(NULL)

  # Return List of Outputs
  newList <- list(D0)
  return(newList)
}



