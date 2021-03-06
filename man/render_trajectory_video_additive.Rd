% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trajectory.R
\name{render_trajectory_video_additive}
\alias{render_trajectory_video_additive}
\title{Render video from trajectory by additive adding target images to the background}
\usage{
render_trajectory_video_additive(filename, trajectory,
  settings = default_settings(), targets = NULL,
  targets_cue_only = F, outdir = getwd())
}
\arguments{
\item{filename}{the file name of output video (e.g. "trajectory.mp4")}

\item{trajectory}{tibble with trajectory data}

\item{settings}{list with basic properties}

\item{targets}{Which objects should be treated as targets}

\item{outdir}{output directory where the video is saved}
}
\value{
No return value
}
\description{
The function repeatedly calls plot_position_additive function to generate preview
of the trajectory motion.
This function differs from render_trajectory_video as it uses creates plot positions by adding targets to background additively (which can't be done using simple ggplot logic)
Extra time is added before and after the motion sequence (2 seconds each).
Currently works only with rectangular arrays and it is not controlled that the objects are within the image. To ensure that the objects are within the borders, use larger images. This will be controlled in sctipt in the future
}
\examples{
# set background as random array
backg <- matrix(sample(255, size = 750*750, replace = T), nrow = 750,ncol = 750) \%>\% imager::as.cimg()

# set target
rlogo <- imager::load.image(system.file("img", "Rlogo.png", package="png")) \%>\% imager::rm.alpha()
alpha_rlogo <-attr(rlogo,"alpha") \%>\% imager::resize_halfXY()
rlogo <- rlogo \%>\% imager::grayscale() \%>\% imager::resize_halfXY()
rlogo <- 127 * (rlogo - mean(rlogo))
rlogo <- rlogo*alpha_rlogo

# set ffmpeg in necessary

# ani.options(ffmpeg = "/PATH/TO/ffmpeg/ffmpeg")

sett_show <- new_settings(show_labels = F, fill_target = NA, fill_object = NA, border_object = NA, border_target = "green", background = backg, target_img = rlogo)
render_trajectory_video_additive("trajectory.mp4", trajectory8c,
  sett_show,
  targets = 1:4
)
}
