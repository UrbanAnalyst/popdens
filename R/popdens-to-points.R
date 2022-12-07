#' Assign raster cell population density values to points
#'
#' @param net_sc A \pkg{silicate}-class object representing a local street
#' network, generated with \pkg{dodgr} function, `dodgr_streetnet_sc()`.
#' @param geotiff Path to 'geotiff' file containing population density
#' estimates, and including the area defined by 'net_sc'.
#' @return A \code{data.frame} containing all vertices of 'net_sc', and
#' corresponding point estimates of population density.
#' @export
pop2point <- function (net_sc, geotiff) {

    checkmate::assert_file_exists (geotiff)
    checkmate::assert_class (net_sc, c ("SC", "osmdata_sc"))

    ras <- raster::raster (geotiff)

    verts <- net_sc$vertex
    bb <- t (apply (verts [, c ("x_", "y_")], 2, range))
    ras <- raster::crop (ras, raster::extent (bb))

    nodes <- sf::st_sf (
        osm_id = verts$vertex_,
        geometry = xy_to_sfc (verts),
        crs = 4326
    )

    nodes_new <- assign_points (ras, nodes)

    return (nodes_new)
}

assign_points <- function (ras, nodes, redistribute_missing = "") {

    pd_sf <- raster::rasterToPolygons (ras) |>
        sf::st_as_sf ()

    pd_sf$id <- seq_len (nrow (pd_sf))
    pd_sf <- sf::st_transform (pd_sf, crs = sf::st_crs (nodes))
    nodes_joined <- sf::st_join (nodes, pd_sf)

    return (nodes_joined)
}


#' xy_to_sfc
#'
#' Convert matrix of xy points to sfc object
#'
#' @param xy matrix, data.frame, or tibble of points
#' @return sf::sfc representation of same
#' @export
xy_to_sfc <- function (xy) {

    x_ <- y_ <- NULL

    xy <- dplyr::select (xy, c (x_, y_)) |>
        dplyr::rename (x = x_, y = y_) |>
        as.matrix ()

    xy_sfc <- sfheaders::sfc_point (xy) |>
        sf::st_sfc (crs = 4326)

    return (xy_sfc)
}
