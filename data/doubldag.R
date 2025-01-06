
# plot levels
dag_double <- function(
  Xs = c(1/3,0),
  Ys = c(2/3,.05),
  Ms = c(4/9,.2),
  e = .95,
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
  add_alpha = FALSE,
  ls = 2.6 ){# gap to lambda

par(mar= c(0,0,0,0))
  frame()

  ###############################################

  # nodes
  z <- .25

  text(Xs[1] -z, Xs[2], expression(X[1]))
  text(Ys[1]-z, Ys[2], expression(Y[1]))
  text(Ms[1]-z, Ms[2], expression(M[1]), cex = .8)

  text(Xs[1]+z, Xs[2], expression(X[2]))
  text(Ys[1]+z, Ys[2], expression(Y[2]))
  text(Ms[1]+z, Ms[2], expression(M[2]), cex = .8)

  # X to Y arrow
  segments(e*Xs[1] + (1-e)*Ys[1] - z,
           e*Xs[2] + (1-e)*Ys[2],
           e*Ys[1] + (1-e)*Xs[1] - z,
           e*Ys[2] + (1-e)*Xs[2])

  segments(e*Xs[1] + (1-e)*Ys[1] + z,
           e*Xs[2] + (1-e)*Ys[2] ,
           e*Ys[1] + (1-e)*Xs[1]+ z,
           e*Ys[2] + (1-e)*Xs[2])

  segments(e*Ys[1] + (1-e)*Xs[1] - s - z,
           e*Ys[2] + (1-e)*Xs[2] - 2*s,
           e*Ys[1] + (1-e)*Xs[1]- z,
           e*Ys[2] + (1-e)*Xs[2])

  segments(e*Ys[1] + (1-e)*Xs[1] - s + z,
           e*Ys[2] + (1-e)*Xs[2] - 2*s,
           e*Ys[1] + (1-e)*Xs[1] + z,
           e*Ys[2] + (1-e)*Xs[2])

  segments(e*Ys[1] + (1-e)*Xs[1] - s -z,
           e*Ys[2] + (1-e)*Xs[2] + s,
           e*Ys[1] + (1-e)*Xs[1] - z,
           e*Ys[2] + (1-e)*Xs[2])

  segments(e*Ys[1] + (1-e)*Xs[1] - s + z,
           e*Ys[2] + (1-e)*Xs[2] + s,
           e*Ys[1] + (1-e)*Xs[1] + z,
           e*Ys[2] + (1-e)*Xs[2])

  # X to M arrow
  segments(e2*Xs[1] + (1-e2)*Ms[1] - z,
           e2*Xs[2] + (1-e2)*Ms[2],
           e2*Ms[1] + (1-e2)*Xs[1] - z,
           e2*Ms[2] + (1-e2)*Xs[2])

  segments(e2*Ms[1] + (1-e2)*Xs[1] - s/2 - z,
           e2*Ms[2] + (1-e2)*Xs[2] - 4*s/2,
           e2*Ms[1] + (1-e2)*Xs[1] - z,
           e2*Ms[2] + (1-e2)*Xs[2])

  segments(e2*Ms[1] + (1-e2)*Xs[1] - s/2  - z,
           e2*Ms[2] + (1-e2)*Xs[2] + 0*s/2,
           e2*Ms[1] + (1-e2)*Xs[1] - z,
           e2*Ms[2] + (1-e2)*Xs[2])


  segments(e2*Xs[1] + (1-e2)*Ms[1] + z,
           e2*Xs[2] + (1-e2)*Ms[2],
           e2*Ms[1] + (1-e2)*Xs[1] + z,
           e2*Ms[2] + (1-e2)*Xs[2])

  segments(e2*Ms[1] + (1-e2)*Xs[1] - s/2 + z,
           e2*Ms[2] + (1-e2)*Xs[2] - 4*s/2,
           e2*Ms[1] + (1-e2)*Xs[1] + z,
           e2*Ms[2] + (1-e2)*Xs[2])

  segments(e2*Ms[1] + (1-e2)*Xs[1] - s/2  + z,
           e2*Ms[2] + (1-e2)*Xs[2] + 0*s/2,
           e2*Ms[1] + (1-e2)*Xs[1] + z,
           e2*Ms[2] + (1-e2)*Xs[2])



    segments(e2*Ms[1] + (1-e2)*Ys[1] -z,
             e2*Ms[2] + (1-e2)*Ys[2],
             e3*Ys[1] + (1-e3)*Ms[1] -z,
             e3*Ys[2] + (1-e3)*Ms[2])

    segments(e3*Ys[1] + (1-e3)*Ms[1] - s/2  -z,   # from below
             e3*Ys[2] + (1-e3)*Ms[2] - 2*s/2,
             e3*Ys[1] + (1-e3)*Ms[1] -z,
             e3*Ys[2] + (1-e3)*Ms[2])

    segments(e3*Ys[1] + (1-e3)*Ms[1] - s/2  -z, # from above
             e3*Ys[2] + (1-e3)*Ms[2] + 3*s/2,
             e3*Ys[1] + (1-e3)*Ms[1] -z,
             e3*Ys[2] + (1-e3)*Ms[2])





    segments(e2*Ms[1] + (1-e2)*Ys[1] +z,
             e2*Ms[2] + (1-e2)*Ys[2],
             e3*Ys[1] + (1-e3)*Ms[1] +z,
             e3*Ys[2] + (1-e3)*Ms[2])

    segments(e3*Ys[1] + (1-e3)*Ms[1] - s/2  +z,   # from below
             e3*Ys[2] + (1-e3)*Ms[2] - 2*s/2,
             e3*Ys[1] + (1-e3)*Ms[1] +z,
             e3*Ys[2] + (1-e3)*Ms[2])

    segments(e3*Ys[1] + (1-e3)*Ms[1] - s/2  +z, # from above
             e3*Ys[2] + (1-e3)*Ms[2] + 3*s/2,
             e3*Ys[1] + (1-e3)*Ms[1] +z,
             e3*Ys[2] + (1-e3)*Ms[2])




  ###############################################

  # thetas
  text(Xs[1] - z, Xs[2]+g, expression(theta^X[1]))
  text(Ys[1] - z, Ys[2]+g, expression(theta^Y[1]))
  text(Ms[1] - z, Ms[2]+g, expression(theta^M[1]))

  text(Xs[1] + z, Xs[2]+g, expression(theta^X[2]))
  text(Ys[1] + z, Ys[2]+g, expression(theta^Y[2]))
  text(Ms[1] + z, Ms[2]+g, expression(theta^M[2]))

  # theta to X segment
  arrows(Xs[1] - z, Xs[2]+g -v, Xs[1]-z, Xs[2]+v, col = arrcol, length = arrow_length)
  arrows(Ys[1] - z, Ys[2]+g -v, Ys[1]-z, Ys[2]+v, col = arrcol, length = arrow_length)
  arrows(Ms[1]- z, Ms[2]+g -v, Ms[1]-z, Ms[2]+v, col = arrcol, length = arrow_length)

  # theta to X segment
  arrows(Xs[1] + z, Xs[2]+g -v, Xs[1]+z, Xs[2]+v, col = arrcol, length = arrow_length)
  arrows(Ys[1] + z, Ys[2]+g -v, Ys[1]+z, Ys[2]+v, col = arrcol, length = arrow_length)
  arrows(Ms[1]+ z, Ms[2]+g -v, Ms[1]+z, Ms[2]+v, col = arrcol, length = arrow_length)


  ###########################################################

  # lambdas
  ll <- .25
  text(Xs[1] -ll, Xs[2]+ls*g, expression(lambda^X))
  text(Ys[1] -ll, Ys[2]+ls*g, expression(lambda^Y))
  text(Ms[1] -ll, Ms[2]+ls*g, expression(lambda^M))

  # lambda to theta Z
  arrows(Xs[1] -ll, Xs[2]+ls*g -v/2, Xs[1] - z, Xs[2]+g+v/2, col = arrcol, length = arrow_length)
  arrows(Xs[1] -ll, Xs[2]+ls*g -v/2, Xs[1] + z, Xs[2]+g+v/2, col = arrcol, length = arrow_length)

  arrows(Ys[1] -ll, Ys[2]+ls*g -v/2, Ys[1] - z, Ys[2]+g+v/2, col = arrcol, length = arrow_length)
  arrows(Ys[1] -ll, Ys[2]+ls*g -v/2, Ys[1] + z, Ys[2]+g+v/2, col = arrcol, length = arrow_length)

  arrows(Ms[1] -ll, Ms[2]+ls*g -v/2, Ms[1] - z, Ms[2]+g+v/2, col = arrcol, length = arrow_length)
  arrows(Ms[1] -ll, Ms[2]+ls*g -v/2, Ms[1] +z , Ms[2]+g+v/2, col = arrcol, length = arrow_length)


if(add_alpha){
  # alphas
  as <- 3.5
  text(Xs[1], Xs[2]+as*g, expression(alpha^X))
  text(Ys[1], Ys[2]+as*g, expression(alpha^Y))
  text(Ms[1], Ms[2]+as*g, expression(alpha^M))


  # alpha to lambda
  arrows(Xs[1], Xs[2]+as*g -v, Xs[1], Xs[2]+ls*g+v, col = arrcol, length = arrow_length)
  arrows(Ys[1], Ys[2]+as*g -v, Ys[1], Ys[2]+ls*g+v, col = arrcol, length = arrow_length)
  arrows(Ms[1], Ms[2]+as*g -v, Ms[1], Ms[2]+ls*g+v, col = arrcol, length = arrow_length)

}
}

# dag_double(ls = 2.8)
