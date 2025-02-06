#' Give a column and row name to all DCA components in a list
#'
#' This function attribute a named number from 1:size of columns or rows ,or
#' attribute the names given by fd_names, sd_names and periods_names, at each
#' matrix component.
#'
#' @param data DCA components list
#' @param n_fd n_fd from dca() function
#' @param n_sd n_sd from dca() function
#' @param periods periods from dca() function
#' @param fd_names fd_names from dca() function
#' @param sd_names sd_names from dca() function
#' @param periods_names periods_names from dca() function
#' @return A list containing all DCA components correctly named

cols_n_rows_labels <- function(data, n_fd, n_sd, periods, fd_names, sd_names , periods_names) {
  require(dplyr)
  require(tibble)

  t_anos <- periods_names[1]:periods_names[2]
  t_anos_pos <- t_anos + (periods - 1)
  per_names <- paste0(t_anos, "-", t_anos_pos)[1:(length(t_anos) - (periods-1))]
  spec_vars <- c("TFD","TSD","TOTAL")
  comp_vars <- c("LPFD","LPSD","GP","LQFD","LQSD")
  time_vars <- c("TFD","TSD","LPFD","LPSD","GPFD","GPSD","GP","LQFD","LQSD")
  per_vars <- c("ICFD", "ICSD")

  data[c("TFD","GPFD","ICFD")] <- lapply(data[c("TFD","GPFD","ICFD")], function(x) `rownames<-`(x, 1:nrow(x)))
  data[c("TSD","GPSD","ICSD")] <- lapply(data[c("TSD","GPSD","ICSD")], function(x) `rownames<-`(x, 1:nrow(x)))
  data["TOTAL"] <- lapply(data["TOTAL"], function(x) `rownames<-`(x, 1:nrow(x)) %>%
                            `colnames<-`(1:ncol(x)) %>% as_tibble() %>%
                            add_column(total_name = "TOTAL", .before = 1))
  data[comp_vars] <- lapply(data[comp_vars], function(x) `rownames<-`(x, 1:nrow(x)))

  if(is.null(periods_names)){
    data <- lapply(data, function(x) `colnames<-`(x, 1:ncol(x)))
    data["TOTAL"] <- lapply(data["TOTAL"], function(x) `colnames<-`(x, 1:ncol(x)))
  } else{
    data[time_vars] <- lapply(data[time_vars], function(x) `colnames<-`(x, t_anos))
    data["TOTAL"] <- lapply(data["TOTAL"], function(x) `colnames<-`(x, c("total_name",t_anos)))
    data[per_vars] <- lapply(data[per_vars], function(x) `colnames<-`(x, per_names))
  }

  if(is.null(fd_names)){
    data[c("TFD","GPFD","ICFD")] <- lapply(data[c("TFD","GPFD","ICFD")], function(x) as_tibble(x) %>%
                                             add_column(fd_name = paste0("fd_name","_",1:nrow(x)), .before = 1))
  } else{
    data[c("TFD","GPFD","ICFD")] <- lapply(data[c("TFD","GPFD","ICFD")], function(x) as_tibble(x) %>%
                                             add_column(fd_name = fd_names, .before = 1))
  }

  if(is.null(sd_names)){
    data[c("TSD","GPSD","ICSD")] <- lapply(data[c("TSD","GPSD","ICSD")], function(x) as_tibble(x) %>%
                                             add_column(sd_name = paste0("sd_name","_",1:nrow(x)), .before = 1))
  } else{
    data[c("TSD","GPSD","ICSD")] <- lapply(data[c("TSD","GPSD","ICSD")], function(x) as_tibble(x) %>%
                                             add_column(sd_name = sd_names, .before = 1))
  }

  if(is.null(fd_names) & is.null(sd_names)){
    data[comp_vars] <- lapply(data[comp_vars], function(x) as_tibble(x) %>%
                                add_column(fd_name = rep(paste0("fd_name","_",1:n_fd), n_sd), .before = 1) %>%
                                add_column(sd_name = rep(paste0("sd_name","_",1:n_sd), each = n_fd), .before = 2))
  } else if(!is.null(fd_names) & is.null(sd_names)){
    data[comp_vars] <- lapply(data[comp_vars], function(x) as_tibble(x) %>%
                                add_column(fd_name = rep(fd_names, n_sd), .before = 1) %>%
                                add_column(sd_name = rep(paste0("sd_name","_",1:n_sd), each = n_fd), .before = 2))
  } else if(is.null(fd_names) & !is.null(sd_names)){
    data[comp_vars] <- lapply(data[comp_vars], function(x) as_tibble(x) %>%
                                add_column(fd_name = rep(paste0("fd_name","_",1:n_fd), n_sd), .before = 1) %>%
                                add_column(sd_name = rep(sd_names, each = n_fd), .before = 2))
  } else if(!is.null(fd_names) & !is.null(sd_names)){
    data[comp_vars] <- lapply(data[comp_vars], function(x) as_tibble(x) %>%
                                add_column(fd_name = rep(fd_names, n_sd), .before = 1) %>%
                                add_column(sd_name = rep(sd_names, each = n_fd), .before = 2))
  }

  return(data)
}

#' Runs the DCA method
#'
#' This function runs the entire Data Concentration Analysis to the desired dataset.
#' A proper explication of the methodology will be soon added do GitHub page.
#'
#' @param data Suitable dataset object
#' @param n_fd Number of indexers in first dimension
#' @param n_sd Number of indexers in second dimension
#' @param periods Number of periods of the Concentration Index
#' @param fd_names A vector containing first dimension names. Size must be the same as n_fd.
#' @param sd_names A vector containing second dimension names. Size must be the same as n_sd.
#' @param periods_names A vector containing first and last periods. Ex.: c(2002, 2021).
#' @return A list containing all DCA components
#' @export

dca <- function(data, n_fd, n_sd, periods, fd_names = NULL, sd_names = NULL, periods_names = NULL) {
  A <- data
  s <- n_sd
  r <- n_fd
  per <- periods
  fdn <- fd_names
  sdn <- sd_names
  pern <- periods_names

  cat("Aggregating FD and SD variables.\n")

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
  cat("Calculating Percentages.\n")
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
  cat("Calculating QLD.\n")
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
  cat("Calculating CI.\n")
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
  cat("Consolidating and finalizing.\n")
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

  dca_component <- cols_n_rows_labels(data = dca_component,
                                      n_fd = r,
                                      n_sd = s,
                                      periods = per,
                                      fd_names = fdn,
                                      sd_names = sdn,
                                      periods_names = pern)
  return(dca_component)
}
