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

    verts_matched <- assign_points (ras, verts)

    return (verts_matched)
}

assign_points <- function (ras, verts, chunk_size = 10000) {

    ras_pts <- raster::rasterToPoints (ras)

    message ("Calculating 'geodist_min' ...")
    ras_match <- geodist::geodist_min (verts, ras_pts, measure = "cheap")
    message ("Finished calculating 'geodist_min'")

    # Then allocate density estimates equally between all verts which map onto
    # single raster density points:
    ras_match_tab <- table (ras_match)
    ras_match_counts <- ras_match_tab [match (ras_match, names (ras_match_tab))]

    nm <- colnames (ras_pts) [which (!colnames (ras_pts) %in% c ("x", "y"))]
    verts$index <- NULL
    verts [[nm]] <- ras_pts [ras_match, 3] / ras_match_counts

    return (verts)
}
