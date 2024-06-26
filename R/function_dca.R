#' Give a column and row name to all DCA components in a list
#'
#' This function attribute a named number from 1:size of columns or rows at each
#' matrix component.
#'
#' @param data DCA components list
#' @return A list containing all DCA components correctly named

cols_n_rows_names <- function(data) {
  g <- lapply(data, function(x) `colnames<-`(x, 1:ncol(x)))
  g <- lapply(g, function(x) `rownames<-`(x, 1:nrow(x)))
  return(g)
}

#' Runs the DCA method
#'
#' This function runs the entire Data Concentration Analysis to the desired dataset.
#' A proper explication of the methodology will be soon added do GitHub page.
#'
#' @param data Suitable dataset object
#' @param n_fd Number of indexers in first dimension
#' @param n_sd Number of indexers in second dimension
#' @param period Number of periods of the Concentration Index
#' @return A list containing all DCA components
#' @export

dca <- function(data, n_fd, n_sd, periods) {
  A <- data
  s <- n_sd
  r <- n_fd

  ###sd totals
  TSD = tibble::tibble()
  for (k in 1:s) {
    TX = c()
    for (i in 1:r) {
      j = (k-1)*i+seq(r)
      TX = A[j,]
    }
    v = as.numeric(colSums(TX))
    TSD = rbind(TSD,v)
    TSD = as.matrix(TSD)
  }

  ###fd totals
  TFD = tibble::tibble()
  for (i in 1:r) {
    TX = c()
    for (k in 1:s) {
      j = (seq(k)-1)*r+i
      TX = A[j,]
    }
    v = as.numeric(colSums(TX))
    TFD = rbind(TFD,v)
    TFD = as.matrix(TFD)
  }

  ###totals
  TOTAL <- colSums(TSD)
  TOTAL <- as.matrix(t(TOTAL))
  h <- ncol(TOTAL)

  ###fd globals percents
  GPFD <- tibble::tibble()
  for (k in 1:r) {
    for (j in 1:h) {
      GPFD[k,j] = TFD[k,j]/TOTAL[,j]
    }
  }
  GPFD <- as.matrix(GPFD)

  ###sd globals percents
  GPSD <- tibble::tibble()
  for (i in 1:s) {
    for (j in 1:h) {
      GPSD[i,j] = TSD[i,j]/TOTAL[,j]
    }
  }
  GPSD <- as.matrix(GPSD)

  ###quocient location
  LQFD <- tibble::tibble()
  LQSD <- tibble::tibble()
  LPFD <- tibble::tibble()
  LPSD <- tibble::tibble()
  GP <- tibble::tibble()
  for (k in 1:s) {
    for (i in 1:r) {
      j = (k-1)*r+i
      for (n in 1:h) {
        ifelse(A[j,n] == 0,
               c(LQFD[j,n] <- 0, LQSD[j,n] <- 0, LPFD[j,n] <- 0, LPSD[j,n] <- 0, GP[j,n] <- 0),
               c(LQFD[j,n] <- (A[j,n]/TFD[i,n])/((TSD[k,n])/(TOTAL[,n])),
                 LPFD[j,n] <- A[j,n]/TFD[i,n],
                 LPSD[j,n] <- A[j,n]/TSD[k,n],
                 LQSD[j,n] <- (A[j,n]/TSD[k,n])/(TFD[i,n]/TOTAL[,n]),
                 GP[j,n] <- A[j,n]/TOTAL[,n]))
      }
    }
  }
  LQFD <- as.matrix(LQFD)
  LQSD <- as.matrix(LQSD)
  LPFD <- as.matrix(LPFD)
  LPSD <- as.matrix(LPSD)
  GP <- as.matrix(GP)

  ###fd concentration index
  p <- h-periods+1
  MLQFD <- tibble::tibble()
  ICFD <- tibble::tibble()
  for (i in 1:r) {
    for (j in 1:p) {
      for (n in 1:periods) {
        nn=j+n-1
        for (k in 1:s) {
          l = (k-1)*r+i
          MLQFD[k,n] = LQFD[l,nn]
        }
      }
      MLQFD <- as.matrix(MLQFD)
      gc <- matrix(colMeans(MLQFD), nrow = 1)
      Z <- MLQFD - matrix(data = 1, nrow = s)%*%gc
      B <- 1/s*t(Z)%*%Z
      C <- eigen(B)$vector
      D <- eigen(B)$values
      av <- D
      ICFD[i,j] <- norm(av, type = "2")
    }
  }
  ICFD <- as.matrix(ICFD)

  ###sd concentration index
  p <- h-periods+1
  MLQSD <- tibble::tibble()
  ICSD <- tibble::tibble()
  for (k in 1:s) {
    for (j in 1:p) {
      for (n in 1:periods) {
        nn=j+n-1
        for (i in 1:r) {
          l = (k-1)*r+i
          MLQSD[i,n] = LQSD[l,nn]
        }
      }
      MLQSD <- as.matrix(MLQSD)
      gc <- matrix(colMeans(MLQSD), nrow = 1)
      Z <- MLQSD - matrix(data = 1, nrow = r)%*%gc
      B <- 1/r*t(Z)%*%Z
      C <- eigen(B)$vector
      D <- eigen(B)$values
      av <- D
      ICSD[k,j] <- norm(av, type = "2")
    }
  }
  ICSD <- as.matrix(ICSD)

  ###consolide components and return it
  dca_component <- list(TFD=TFD,
                        TSD=TSD,
                        TOTAL=TOTAL,
                        LPFD=LPFD,
                        LPSD=LPSD,
                        GPFD=GPFD,
                        GPSD=GPSD,
                        GP=GP,
                        LQFD=LQFD,
                        LQSD=LQSD,
                        ICFD=ICFD,
                        ICSD=ICSD)

  dca_component <- cols_n_rows_names(dca_component)

  return(dca_component)
}
