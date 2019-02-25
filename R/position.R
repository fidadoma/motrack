
#' Is the object a valid position
#'
#' The function tests an object (tibble) for several requirements:
#' \itemize{
#'   \item columns `object`, `x` and `y` are present.
#'   \item `object` column contains unique values
#'   \item no value missing in `x` and `y`
#'   \item `x` and `y` are numeric
#' }
#'
#' No restriction about data type of `object` is placed.
#'
#' @param position tibble
#'
#' @return TRUE if conditions above are met, FALSE otherwise
#' @export
#'
#' @examples
#' example_pos <-
#'   tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
#' is_valid_position(example_pos)
is_valid_position <- function(position) {
  # requirements:
  # 1) each object only once
  # 2) no missing x, y
  tmp <- position %>%
    dplyr::group_by(.data$object) %>%
    dplyr::summarise(n = dplyr::n())
  all(tmp$n == 1) &&
    all(!is.na(position$x)) &&
    all(!is.na(position$y)) &&
    is.numeric(position$x) &&
    is.numeric(position$y)
}

#' Tests inter-object distances
#'
#' The function calculated all pairwise distances and checks
#' if the are larger or equal.
#'
#' @param position tibble
#' @param min_distance Minimum distance to be present between each pair
#'
#' @return TRUE if all distances are larger than `min_distance`
#' @export
#'
#' @examples
#' example_pos <-
#'   tibble::tibble(object = 1:8, x = 1:8, y = 4 - (1:8))
#' is_distance_at_least(example_pos, 1)
#' is_distance_at_least(example_pos, 2)
is_distance_at_least <- function(position, min_distance) {
  dist_triangle <-
    position %>%
    dplyr::select(.data$x, .data$y) %>%
    as.matrix() %>%
    stats::dist()
  all(dist_triangle >= min_distance)
}

#' Default settings for position/trajectory code
#'
#' Returns a list of values used for position/trajectory generation or
#' presentation. The values refer to the "physical properties"
#' (e.g., definition of the objects or arena) or
#' to the presentation propoerties
#' (e.g., objects' colour, presence of border).
#' The list is passed as a parameter to other functions.
#'
#' Currently used properties include:
#' \describe{
#'   \item{xlim}{A vector of form `c(xmin, xmax)`.
#'   Represents x-limit of the arena square, objects are limited to this area.}
#'   \item{ylim}{A vector of form `c(ymin, ymax)`.
#'   Represents y-limit of the arena square, objects are limited to this area.}
#'   \item{min_dist}{Minimum pairwise distance between centres of objects.}
#'   \item{r}{Radius of the object. Object is considered to be a circle.}
#'   \item{arena_border}{Logical. Whether arena border should be drawn.}
#'   \item{arena_shape}{Character. "square" or "circle"}
#'   \item{fill_object}{Character. Colour code of default fill colour.}
#'   \item{fill_target}{Character. Colour code of target fill colour.}
#'   \item{border_object}{Character. Colour code of default border.}
#'   \item{border_target}{Character. Colour code of target border.}
#'   \item{show_labels}{Logical. Should we show object numbers in plots?}
#'   \item{bounce_off_square}{Logical. Should objects bounce off square arena?}
#'   \item{bounce_off_circle}{Logical. Should objects bounce off circular arena?}
#'   \item{circle_bounce_jitter}{Real. Amount of uniform angular jitter after bouncing, in radians.}
#' }
#'
#' @return list of parameters
#' @export
#'
#' @examples
#' default_settings()
default_settings <- function() {
  list(
    xlim = c(-10, 10),
    ylim = c(-10, 10),
    min_dist = 1,
    r = 0.5,
    arena_border = T,
    arena_shape = "square",
    fill_object = "gray",
    fill_target = "green",
    border_object = "black",
    border_target = "black",
    show_labels = F,
    bounce_off_square = F,
    bounce_off_others = T,
    bounce_off_circle = F,
    circle_bounce_jitter = 0
  )
}


#' Creates a custom copy of settings
#'
#' @param ... named list of properties to override default settings.
#'
#' @return Updated list of settings
#' @export
#'
#' @seealso \code{\link{default_settings}}
#'
#' @examples
#' new_settings(xlim = c(0, 10), ylim = c(0, 10))
new_settings <- function(...) {
  settings_list <- default_settings()
  dots <- list(...)
  for (key in names(dots)) {
    settings_list[[key]] <- dots[[key]]
  }
  settings_list
}

#' Random object positions
#'
#' The function expects a square arena defined by `xlim` and `ylim` settings.
#' The objects are placed randomly (uniform distribution sampling) into the arena.
#' If `check_distance` is `TRUE`, the process is repeated
#' until the minimum pairwise distance is met.
#' `border_distance` specifies, whether objects should keep some initial distance
#' from arena borders. This distance is not meant to be the bouncing distance,
#' it helps to put objects more together in the beginning.
#'
#' @param n Number of objects
#' @param settings Basic properties - namely `xlim`, `ylim`,
#' possibly `min_distance`
#' @param check_distance Logical.
#' The positions are generated until
#' minimum pairwise distance of `min_dist` is met.
#' @param border_distance Distance from arena borders.
#'
#' @return Tibble with `object`, `x` and `y` columns.
#' @export
#'
#' @seealso
#' \code{\link{default_settings}} for setting definitions,
#' \code{\link{new_settings}} for adjusting default settings,
#' \code{\link{is_distance_at_least}} for checking distances
#'
#' @examples
#' # sample positions with no other requirements
#' pos <- generate_positions_random(8, default_settings())
#' # when starting positions should be further from borders
#' pos <- generate_positions_random(8, default_settings(), border_distance = 3)
generate_positions_random <- function(
                                      n, settings, check_distance = T, border_distance = 0) {
  xlim <- settings$xlim
  ylim <- settings$ylim
  stopifnot(!is.null(xlim))
  stopifnot(!is.null(ylim))
  shapes <- c("square", "circle")
  shape <- pmatch(settings$arena_shape, shapes)
  if (is.na(shape)) {
    stop("Arena shape unknown")
  }
  while (T) {
    p <- switch(
      shapes[shape],
      square = random_coords_in_square(
        n,
        xlim[1] + border_distance, xlim[2] - border_distance,
        ylim[1] + border_distance, ylim[2] - border_distance
      ),
      circle = random_coords_in_circle(
        n,
        mean(xlim), mean(ylim),
        sum(c(diff(xlim), diff(ylim)) / 4)
      )
    )
    p <- p %>%
      dplyr::mutate(object = 1:n) %>%
      dplyr::select(.data$object, dplyr::everything())
    if (check_distance) {
      if (is_distance_at_least(p, min_distance = settings$min_distance)) {
        return(p)
      }
    } else {
      return(p)
    }
  }
}

random_coords_in_square <- function(n, xmin, xmax, ymin, ymax) {
  tibble::tibble(
    x = stats::runif(n, xmin, xmax),
    y = stats::runif(n, ymin, ymax)
  )
}

random_coords_in_circle <- function(n, xmid, ymid, radius) {
  stopifnot(radius > 0)
  res <- tibble::tibble(x = rep(NA_real_, n), y = rep(NA_real_, n))
  i <- 1
  while (i <= n) {
    xx <- stats::runif(1, -radius, radius)
    yy <- stats::runif(1, -radius, radius)
    d <- sqrt((xx^2) + (yy^2))
    if (d < radius) {
      res$x[i] <- xmid + xx
      res$y[i] <- ymid + yy
      i <- i + 1
    }
  }
  res
}

#' Plot object positions
#'
#' The function plots position tibble based on x, y values and potentially
#' extra values from settings. Position is expected to be position tibble,
#' but it may contain extra columns for graphics:
#' target (Logical vector indicating whether object is target or distractor),
#' fill (Character vector with colour names of object interiors.),
#' border (Character vector with colour names of object borders.).
#'
#' @param position tibble
#' @param settings list with basic properties
#' @param targets Which objects should be treated as targets
#'
#' @return ggplot2 figure
#' @export
#'
#' @examples
#' # sample positions with no other requirements
#' set.seed(100)
#'
#' sett <- default_settings()
#'
#' pos <- generate_positions_random(8, default_settings())
#' plot_position(pos, sett)
#' # first four objects are targets
#' plot_position(pos, sett, 1:4)
#' pos$fill <- rainbow(8)
#' plot_position(pos, sett)
#'
#' # add background image
#' sett <- new_settings(background = imager::boats)
#' plot_position(pos, sett)
#'
plot_position <- function(position,
                          settings = default_settings(),
                          targets = NULL) {
  # check extra parameters
  if (!is.null(targets)) {
    # passed in call
    position$target <- F
    position$target[targets] <- T
  } else {
    if (!"target" %in% names(position)) {
      position$target <- F
      # otherwise we have it in dataset
    }
  }
  if (!"fill" %in% names(position)) {
    position$fill <- settings$fill_object
    position$fill[position$target] <- settings$fill_target
  }
  if (!"border" %in% names(position)) {
    position$border <- settings$border_object
    position$border[position$target] <- settings$border_target
  }
  fig <-
    ggplot2::ggplot(
      position,
      ggplot2::aes_string(
        x0 = "x", y0 = "y",
        fill = "I(fill)", colour = "I(border)"
      )
    ) +
    ggforce::geom_circle(ggplot2::aes_string(r = "settings$r")) +
    ggplot2::theme(panel.background = ggplot2::element_blank()) +
    ggplot2::coord_fixed(xlim = settings$xlim, ylim = settings$ylim, expand = F) +
    NULL

  # insert the image into background
  if(!is.null(settings$background)) {
    g <- grid::rasterGrob(imager::as.cimg(settings$background), interpolate=TRUE)
    fig$layers <- c(ggplot2::annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), fig$layers)
  }

  if (settings$show_labels) {
    fig <-
      fig +
      ggplot2::geom_text(
        ggplot2::aes_string(x = "x", y = "y", label = "object"),
        colour = I("red")
      )
  }
  if (settings$arena_border) {
    fig <- fig +
      ggplot2::annotate("segment",
        x = settings$xlim[1], y = settings$ylim[1],
        xend = settings$xlim[1], yend = settings$ylim[2]
      ) +
      ggplot2::annotate("segment",
        x = settings$xlim[1], y = settings$ylim[1],
        xend = settings$xlim[2], yend = settings$ylim[1]
      ) +
      ggplot2::annotate("segment",
        x = settings$xlim[1], y = settings$ylim[2],
        xend = settings$xlim[2], yend = settings$ylim[2]
      ) +
      ggplot2::annotate("segment",
        x = settings$xlim[2], y = settings$ylim[1],
        xend = settings$xlim[2], yend = settings$ylim[2]
      )
  }
  fig
}

#' Plot object positions by adding targets to the background
#'
#' The function plots position tibble based on x, y values and potentially
#' extra values from settings. Position is expected to be position tibble,
#' but it may contain extra columns for graphics:
#' target (Logical vector indicating whether object is target or distractor),
#' fill (Character vector with colour names of object interiors.),
#' border (Character vector with colour names of object borders.).
#'
#' This function requires two parameters to be present in the settings:
#' target_img (image that will be added additively to the background)
#' background (image that will be added to the background)
#' Additionally, it is also good to overwrite the default target and distractor borders and fill
#'
#' @param position tibble
#' @param settings list with basic properties
#' @param targets Which objects should be treated as targets
#'
#' @return ggplot2 figure
#' @export
#'
#' @examples
#' # sample positions with no other requirements
#' set.seed(100)
#' bckg <- imager::boats
#' tgt <- imager::load.image(system.file("img", "Rlogo.png", package="png"))
#' settings_additive <- new_settings(background = bckg, target_img = tgt, fill_target = NA, fill_object = NA, border_object = NA, border_target = "green")
#' pos <- generate_positions_random(8, settings_additive)
#' plot_position_image(pos, settings_additive)
#' # first four objects are targets
#' plot_position(pos, default_settings(), 1:4)
#' pos$fill <- rainbow(8)
#' plot_position(pos, default_settings())
plot_position_image <- function(position,
                                settings = default_settings(),
                                targets = NULL) {
  # check extra parameters
  if (!is.null(targets)) {
    # passed in call
    position$target <- F
    position$target[targets] <- T
  } else {
    if (!"target" %in% names(position)) {
      position$target <- F
      # otherwise we have it in dataset
    }
  }

  # we are adding targets to to the noise additively

  position_pix <- convert_position_to_pixels(position, dim(noise),sum(abs(settings$ylim)))

  noise <- settings$background
  gabor <- settings$target_img
  for (i in 1:nrow(position)) {
    noise <- add_image_additive(noise, gabor, position_pix$x[i], position_pix$y[i])
  }
  noise[noise > 255] <- 255
  noise[noise < 0]   <- 0
  #if(!is.null(tim)){
  #  save(noise, file = file.path("noises",sprintf("t%.02f.RData", tim)))
  #}
  g <- grid::rasterGrob(round(imager::as.cimg(noise)), interpolate=TRUE)
  backg <- ggplot2::annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)


  if (!"fill" %in% names(position)) {
    position$fill <- settings$fill_object
    position$fill[position$target] <- settings$fill_target
  }
  if (!"border" %in% names(position)) {
    position$border <- settings$border_object
    position$border[position$target] <- settings$border_target
  }
  if(!is.null(settings$target_img)) {
    fig <-
      ggplot2::ggplot(
        position,
        ggplot2::aes_string(
          x0 = "x", y0 = "y",
          fill = "I(fill)", colour = "I(border)"
        )
      ) +
      backg +
      ggforce::geom_circle(ggplot2::aes_string(r = "settings$r")) +
      ggplot2::theme(panel.background = ggplot2::element_blank()) +
      ggplot2::coord_fixed(xlim = settings$xlim, ylim = settings$ylim, expand = F) +
      NULL
  }
  else {


    fig <-
      ggplot2::ggplot(
        position,
        ggplot2::aes_string(
          x0 = "x", y0 = "y"
        )
      ) +
      backg +
      ggforce::geom_circle(ggplot2::aes_string(r = "settings$r")) +
      ggplot2::theme(panel.background = ggplot2::element_blank()) +
      ggplot2::coord_fixed(xlim = settings$xlim, ylim = settings$ylim, expand = F) +
      NULL
  }
  if (settings$show_labels) {
    fig <-
      fig +
      ggplot2::geom_text(
        ggplot2::aes_string(x = "x", y = "y", label = "object"),
        colour = I("red")
      )
  }
  if (settings$arena_border) {
    fig <- fig +
      ggplot2::annotate("segment",
                        x = settings$xlim[1], y = settings$ylim[1],
                        xend = settings$xlim[1], yend = settings$ylim[2]
      ) +
      ggplot2::annotate("segment",
                        x = settings$xlim[1], y = settings$ylim[1],
                        xend = settings$xlim[2], yend = settings$ylim[1]
      ) +
      ggplot2::annotate("segment",
                        x = settings$xlim[1], y = settings$ylim[2],
                        xend = settings$xlim[2], yend = settings$ylim[2]
      ) +
      ggplot2::annotate("segment",
                        x = settings$xlim[2], y = settings$ylim[1],
                        xend = settings$xlim[2], yend = settings$ylim[2]
      )
  }
  fig
}

#' Add one image additively to another one at given position
#'
#' @param im background image
#' @param tgt target image that will be added to the background
#' @param x x position of the target
#' @param y y position of the target
#'
#' @return merged image array
#' @export
#'
#' @examples
#' # Add Rlogo image to boat image supllied with imager packager
#' set.seed(100)
#' bckg <- imager::boats  %>% imager::grayscale()
#' tgt <- imager::load.image(system.file("img", "Rlogo.png", package="png")) %>% imager::rm.alpha() %>% imager::grayscale() %>% imager::resize_halfXY()
#' plot(imager::as.cimg(add_image_additive(bckg, tgt, ncol(bckg)/2, nrow(bckg)/2)))
add_image_additive <- function(im, tgt, x, y) {
  im <- im %>% as.matrix(im)
  tgt <- tgt %>% as.matrix(tgt)
  stopifnot(all(dim(tgt) %% 2 == 0))

  tgt_lenx <- ncol(tgt) / 2
  tgt_leny <- nrow(tgt) / 2
  x <- x
  y <- ncol(im) - y
  im[(x-tgt_leny):(x+tgt_leny-1),(y-tgt_lenx):(y+tgt_lenx-1)] <- im[(x-tgt_leny):(x+tgt_leny-1),(y-tgt_lenx):(y+tgt_lenx-1)] + tgt
  return(im)
}

#' Converts position in degrees to pixels to have comparable length with
#'
#' @param position tibble
#' @param border_length_pix borders of arena in pixels - c(x,y)
#' @param border_length_deg length of arena in degrees
#'
#' @return tibble
#' @export
#'
#' @examples
#' settings = new_settings()
#' pos <- generate_positions_random(8, settings)
#' convert_position_to_pixels(pos, c(750,750),sum(abs(settings$xlim)))
#'
convert_position_to_pixels <- function(position, border_length_pix, border_length_deg) {
  position_pix <- position %>%
    mutate(x = round(border_length_pix[2] / 2 + border_length_pix[2] * x/border_length_deg),
           y = round(border_length_pix[1] / 2 + border_length_pix[1] * y/border_length_deg))
  return(position_pix)
}
