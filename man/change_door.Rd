% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem Script.R
\name{change_door}
\alias{change_door}
\title{Change the door}
\usage{
change_door(stay = T, opened.door, a.pick)
}
\arguments{
\item{opened.door}{Numeric, opened goat door}

\item{a.pick}{Numeric, Player's initial selection}

\item{Stay=}{T or F, logical}
}
\value{
return a number between 1 and 3, depending if the player stay or
remain on his selection.
}
\description{
\code{change_door(stay=T, opened.door, a.pick)} generate an option for the player
to change his selection or remain on his current selection
}
\details{
The contestant is given the option to change from their initial selection to
the other door that is still closed. The function will represent the game-
playing strategy as the argument stay=TRUE or stay=FALSE
}
\examples{
opened.door <- open_goat_door( game, a.pick )
a.pick <- select_door()
change_door( stay=T, opened.door, a.pick )

}
