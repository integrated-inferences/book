
# plot levels
dag_single <- function(
  Xs = c(1/3,.1),
  Ys = c(2/3,.3),
  Ms = c(4/9,.9),
  e = .925,
  e2 = .88,
  e3 = .78,
  s = .02,
  g = .275,
  v = g/5,
  L = .05, # Text left
  R = .05,# Text right
  arrow_length = .1,
  arrcol = "darkgrey",
  cex = .75,
  textcol = "black",
  add_M = TRUE,
  func = FALSE,
  confounding = FALSE){
  par(mar = c(0,0,0,0))
  frame()

  ###############################################

  # nodes
  text(Xs[1], Xs[2], expression(paste("X = (", X[1], ", ", X[2], ")")))
  text(Ys[1], Ys[2], expression(paste("Y = (", Y[1], ", ", Y[2], ")")))
  text(Ms[1], Ms[2], expression(paste("M = (", M[1], ", ", M[2], ")")))

  # X to Y arrow
  arrows(e*Xs[1] + (1-e)*Ys[1],
           e*Xs[2] + (1-e)*Ys[2],
           e*Ys[1] + (1-e)*Xs[1],
           e*Ys[2] + (1-e)*Xs[2])


  # X to M arrow
  arrows(e2*Xs[1] + (1-e2)*Ms[1],
           e2*Xs[2] + (1-e2)*Ms[2],
           e2*Ms[1] + (1-e2)*Xs[1],
           e2*Ms[2] + (1-e2)*Xs[2])

    arrows(e2*Ms[1] + (1-e2)*Ys[1],
             e2*Ms[2] + (1-e2)*Ys[2],
             e3*Ys[1] + (1-e3)*Ms[1],
             e3*Ys[2] + (1-e3)*Ms[2])


}

dag_single()
