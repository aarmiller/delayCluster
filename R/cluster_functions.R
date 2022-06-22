
#' Find clusters
#'
#' Main function to find clusters from a dataset of fitted values using k-means
#'
#' @param fit_data a dataset of fitted trend values
#' @param normalize a logical indicator of whether to normalize the model parameter
#' values (default is TRUE)
#' @param k number of clusters to find (default is k = 5)
#' @param nstart if centers is a number, how many random sets should be chosen?
#' @param iter.max the maximum number of iterations allowed.
#'
#' @export
find_clusters <- function(fit_data,normalize = TRUE, k = 5,
                          nstart = 50, iter.max = 50) {

  # prep data
  tmp <- fit_data %>% na.omit()

  if (normalize == TRUE){
    tmp[,4:ncol(tmp)] <- tmp[,4:ncol(tmp)] %>%
      mutate_all(~(.-mean(.))/sd(.))
  }

  km.res <- kmeans(tmp[,4:ncol(tmp)], k,  nstart = nstart, iter.max = iter.max)

  tmp <- tmp %>%
    mutate(cluster=km.res$cluster)

  tmp_centers <-  km.res$centers %>%
    tibble::as_tibble() %>%
    dplyr::mutate(cluster=row_number()) %>%
    dplyr::select(cluster,everything()) %>%
    dplyr::group_by(cluster) %>%
    tidyr::nest() %>%
    dplyr::mutate(center = purrr::map(data,~as.numeric(as.vector(.)))) %>%
    dplyr::select(cluster,center)


  tmp_distances <- tmp[c(1,4:ncol(tmp))] %>%
    dplyr::group_by(index,cluster) %>%
    tidyr::nest() %>%
    dplyr::mutate(v1 = map(data,~as.numeric(as.vector(.)))) %>%
    dplyr::inner_join(tmp_centers, by = "cluster") %>%
    dplyr::mutate(center_distance = purrr::map2_dbl(v1,center,euclidean)) %>%
    dplyr::select(index,cluster,v1,center,center_distance)

  return(list(clusters = tmp,
              centers = tmp_centers,
              distances = tmp_distances))

}

#' Search clusters
#'
#' Search clusters for a specific code and return the focal cluster containing the code
#'
#' @param clust_data a dataset of cluster results (generated from `find_clusters()`)
#' @param search_code a code to search clusters for
#'
#' @export
search_clusters <- function(clust_data, search_code){

  # pull out code to focus on
  focus_code <- clust_data$clusters %>%
    dplyr::filter(code==search_code)

  # pull out center of focus code
  focus_center <- focus_code[4:(ncol(focus_code))] %>%
    dplyr::group_by(cluster) %>%
    tidyr::nest() %>%
    dplyr::mutate(focus_center = purrr::map(data,~as.numeric(as.vector(.)))) %>%
    dplyr::select(-data)

  cluster_data <- focus_code %>%
    dplyr::select(cluster) %>%
    dplyr::inner_join(clust_data$clusters, by = "cluster")

  distances <- clust_data$distances %>%
    dplyr::inner_join(focus_center, by = "cluster") %>%
    dplyr::mutate(focus_distance = purrr::map2_dbl(v1,focus_center,euclidean)) %>%
    dplyr::select(cluster,index,center_distance,focus_distance) %>%
    dplyr::arrange(focus_distance) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(select(cluster_data,index:description), by = "index")

  return(list(focus_center=focus_center,
              cluster_data = cluster_data,
              distances = distances))
}

#' Search cluster terms
#'
#' Search clusters for a specific term in the codes used to cluster on
#'
#' @param clust_data a dataset of cluster results (generated from `find_clusters()`)
#' @param search_term a term to search for
#'
#' @export
search_clust_terms <- function(clust_data,search_term){

  if (length(search_term)==1) {
    clust_data$clusters %>%
      dplyr::filter(stringr::str_detect(tolower(description),tolower(search_term)))
  } else {
    out_list <- list()
    for (i in search_term){
      out_list[[i]] <-   clust_data$clusters %>%
        dplyr::filter(stringr::str_detect(tolower(description),tolower(i)))
    }
    return(out_list)
  }

}

#' Plot cluster
#'
#' Plot trends from a group of codes in a given cluster
#'
#' @param clust_data a dataset of cluster results (generated from `find_clusters()`)
#' @param search_code a code to search clusters for and identify a focal cluster
#' @param size the number of codes to include in the plot (default is 9)
#' @param code_type type of code to extract trends from
#' @param span smoothing parameter for loess fit in plot
#' @param bin_by number of days to bin counts by (default is 1)
#'
#' @export
plot_cluster_code_group <- function(clust_data,search_code,size = 9,code_type, span = 0.75,
                                    bin_by=1){

  tmp <- search_clusters(clust_data = clust_data,
                         search_code = search_code)$distances %>%
    dplyr::arrange(focus_distance) %>%
    dplyr::slice(1:size) %>%
    dplyr::select(code,description,focus_distance)


  if (bin_by>1){
    plot_data <- code_counts[[code_type]] %>%
      dplyr::mutate(days_since_index=(days_since_index %/% bin_by)*bin_by) %>%
      dplyr::group_by(days_since_index,code) %>%
      dplyr::summarise(frac=sum(n)/sum(tot_n)) %>%
      dplyr::inner_join(tmp) %>%
      dplyr::mutate(description = forcats::fct_relevel(description,tmp$description))
  } else {
    plot_data <- code_counts[[code_type]] %>%
      dplyr::inner_join(tmp) %>%
      dplyr::mutate(description = forcats::fct_relevel(description,tmp$description))
  }


  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(days_since_index,frac)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(span=span) +
    ggplot2::facet_wrap(~description, scales = "free_y") +
    ggplot2::theme_bw()
}
