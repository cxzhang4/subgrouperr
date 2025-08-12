#' Method 1 from the Ghosh and Crosby (2005) paper.
#'
#' @param D A vector of crossover differences
#' @param threshold If the largest gap is below this value, then do not form subgroups.
#' @param CAF A multiplicative factor that determines the critical value for forming subgroups.
#'
#' @return A list.
#' @export
method1 <- function(D, IDs, threshold = 0, CAF = 0.5) {
  NA_idx = is.na(D)

  D_complete = D[!NA_idx]
  IDs_complete = IDs[!NA_idx]

  # TODO: assert CAF non-negative integer
  n <- length(D)

  # radix sort is stable
  D_sorted_res <- sort(D_complete, method = "radix", index.return = TRUE)

  D_sorted = D_sorted_res$x
  D_sorted_idx_in_orig <- D_sorted_res$ix

  adjacent_gaps <- diff(D_sorted)

  max_gap <- max(adjacent_gaps)
  min_gap <- min(adjacent_gaps)

  if ((max_gap - min_gap) < threshold) {
    # Don't form subgroups
    return(rep(1L, times = n))
  }

  CAV <- CAF * (max_gap - min_gap) + min_gap

  subgroups <- list()
  current_subgroup <- 1

  i <- 2
  while (i <= n) {
    gap_to_first <- D_sorted[i] - D_sorted[current_subgroup[1]]

    tied_indices <- which(D_sorted == D_sorted[i])

    if (gap_to_first <= CAV) {
      # add to subgroup
      current_subgroup <- c(current_subgroup, tied_indices)
      i <- max(tied_indices) + 1
    } else {
      if (adjacent_gaps[i - 1] > CAV) {
        # start new subgroup
        subgroups <- append(subgroups, list(D_sorted_idx_in_orig[current_subgroup]))
        current_subgroup <- c(i)
        i <- max(tied_indices) + 1
      } else {
        # apply majority rule
        v = length(current_subgroup)

        gaps_to_current = purrr::map_dbl(current_subgroup, function(j) D_sorted[j] - D_sorted[i])

        num_small_gaps = sum(gaps_to_current <= CAV)

        if (num_small_gaps > (v %/% 2)) {
          # current is similar enough to the previous subgroups, so we add to existing subgroup
          current_subgroup = c(current_subgroup, i)
        } else {
          # start new subgroup
          subgroups = append(subgroups, list(D_sorted_idx_in_orig[current_subgroup]))
          current_subgroup = i
        }

        i = max(tied_indices) + 1
      }
    }
  }

  # Add the last subgroup
  subgroups = append(subgroups, list(D_sorted_idx_in_orig[current_subgroup]))

  subgroups_vec = rep(0, times = n)

  for (i in seq_along(subgroups)) {
    subgroups_vec[subgroups[[i]]] = i
  }

  return(list(
    subgroups_idx = as.factor(subgroups_vec),
    num_subgroups = length(subgroups),
    IDs_complete = IDs_complete,
    D_orig = D,
    subgroups_idx_sorted = sort(subgroups_vec),
    D_sorted = D_sorted,
    D_sorted_idx_in_orig = D_sorted_idx_in_orig,
    IDs_sorted = IDs_complete[D_sorted_idx_in_orig],
    adjacent_gaps = adjacent_gaps,
    CAV = CAV
  ))
}