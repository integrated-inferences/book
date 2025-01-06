
# plot levels
dag_3d <- function(
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
  add_M = TRUE,
  func = FALSE,
  confounding = FALSE){

  frame()

  ###############################################

  # nodes
  text(Xs[1], Xs[2], "X")
  text(Ys[1], Ys[2], "Y")
  if(add_M) text(Ms[1], Ms[2], "M", cex = .8)

  # X to Y arrow
  segments(e*Xs[1] + (1-e)*Ys[1],
           e*Xs[2] + (1-e)*Ys[2],
           e*Ys[1] + (1-e)*Xs[1],
           e*Ys[2] + (1-e)*Xs[2])

  segments(e*Ys[1] + (1-e)*Xs[1] - s,
           e*Ys[2] + (1-e)*Xs[2] - 2*s,
           e*Ys[1] + (1-e)*Xs[1],
           e*Ys[2] + (1-e)*Xs[2])

  segments(e*Ys[1] + (1-e)*Xs[1] - s,
           e*Ys[2] + (1-e)*Xs[2] + s,
           e*Ys[1] + (1-e)*Xs[1],
           e*Ys[2] + (1-e)*Xs[2])


  # X to M arrow
  if(add_M){
  segments(e2*Xs[1] + (1-e2)*Ms[1],
           e2*Xs[2] + (1-e2)*Ms[2],
           e2*Ms[1] + (1-e2)*Xs[1],
           e2*Ms[2] + (1-e2)*Xs[2])

  segments(e2*Ms[1] + (1-e2)*Xs[1] - s/2,
           e2*Ms[2] + (1-e2)*Xs[2] - 4*s/2,
           e2*Ms[1] + (1-e2)*Xs[1],
           e2*Ms[2] + (1-e2)*Xs[2])

  segments(e2*Ms[1] + (1-e2)*Xs[1] - s/2,
           e2*Ms[2] + (1-e2)*Xs[2] + 0*s/2,
           e2*Ms[1] + (1-e2)*Xs[1],
           e2*Ms[2] + (1-e2)*Xs[2])
  }

  if(add_M){
    segments(e2*Ms[1] + (1-e2)*Ys[1],
             e2*Ms[2] + (1-e2)*Ys[2],
             e3*Ys[1] + (1-e3)*Ms[1],
             e3*Ys[2] + (1-e3)*Ms[2])

    segments(e3*Ys[1] + (1-e3)*Ms[1] - s/2,   # from below
             e3*Ys[2] + (1-e3)*Ms[2] - 2*s/2,
             e3*Ys[1] + (1-e3)*Ms[1],
             e3*Ys[2] + (1-e3)*Ms[2])

    segments(e3*Ys[1] + (1-e3)*Ms[1] - s/2, # from above
             e3*Ys[2] + (1-e3)*Ms[2] + 3*s/2,
             e3*Ys[1] + (1-e3)*Ms[1],
             e3*Ys[2] + (1-e3)*Ms[2])
  }


  if(confounding){
    from = .72
    run = .2
    radx = (Ys[1] - Ms[1])*1
    rady = (Ms[2] - Ys[2])*1
    cx = Ys[1]
    cy = Ms[2]
    tilt = 0
    z = seq(0,-run*2*pi, length=100)+(2*pi)*from
    points(cos(z)*radx+cx, sin(z)*rady+cy + tilt*cos(z)*radx, col="black", type="l", lty=2)
  }


  if(func){
    text(Xs[1] - L, Xs[2]+0*g, expression(paste(X=theta^X)), cex = cex, col = textcol,  pos = 2)
    # text(Ys[1] +R, Ys[2]+0*g, expression(paste(Y = theta^Y,(x))), cex = cex, col = textcol,  pos = 4)
    if(add_M){
      text(Ms[1] +R, Ms[2]+0*g,  expression(paste(M = theta^M,(x))), cex = cex*.8, col = textcol,  pos = 4)
      text(Ys[1] +R, Ys[2]+0*g, expression(paste(Y = theta^Y,"(x,m)")), cex = cex, col = textcol,  pos = 4)
    } else {
      text(Ys[1] +R, Ys[2]+0*g, expression(paste(Y = theta^Y,(x))), cex = cex, col = textcol,  pos = 4)
    }
  } else {
    text(Xs[1] - L, Xs[2]+0*g, "observed value: 0/1", cex = cex, col = textcol,  pos = 2)
    text(Ys[1] +R, Ys[2]+0*g,  "observed value: 0/1", cex = cex, col = textcol,  pos = 4)
    if(add_M)   text(Ms[1] +R/2, Ms[2]+0*g,  "observed value: 0/1", cex = cex*.8, col = textcol,  pos = 4)

  }


  ###############################################

  # thetas
  text(Xs[1], Xs[2]+g, expression(theta^X))
  text(Ys[1], Ys[2]+g, expression(theta^Y))

  if(add_M) text(Ms[1], Ms[2]+g, expression(theta^M))

  # theta to X segment
  arrows(Xs[1], Xs[2]+g -v, Xs[1], Xs[2]+v, col = arrcol, length = arrow_length)
  arrows(Ys[1], Ys[2]+g -v, Ys[1], Ys[2]+v, col = arrcol, length = arrow_length)

  if(add_M)
    arrows(Ms[1], Ms[2]+g -v, Ms[1], Ms[2]+v, col = arrcol, length = arrow_length)

  if(func){
    text(Xs[1] - L, Xs[2]+1*g, expression(paste(theta^X, "~ Categorical(", lambda^X, ")")), cex = cex, col = textcol,  pos = 2)
    text(Ys[1] +R, Ys[2]+1*g, expression(paste(theta^Y, "~ Categorical(", lambda^Y, ")")), cex = cex, col = textcol,  pos = 4)

    } else {
      text(Xs[1] - L, Xs[2]+1*g, expression(paste("nodal type (function):", f^X,(.))), cex = cex, col = textcol,  pos = 2)

    if(add_M){
      text(Ys[1] +R, Ys[2]+1*g, expression(paste("nodal type (function): ", f^Y,"(x,m)",)), cex = cex, col = textcol,  pos = 4)
    } else {
      text(Ys[1] +R, Ys[2]+1*g, expression(paste("nodal type (function): ", f^Y,(x))), cex = cex, col = textcol,  pos = 4)
    }}

  ###########################################################

  # lambdas
  text(Xs[1], Xs[2]+2*g, expression(lambda^X))
  if(!confounding) text(Ys[1], Ys[2]+2*g, expression(lambda^Y))
  if(confounding)  text(Ys[1]/2+Ms[1]/2, Ys[2]/2 + Ms[2]/2+2*g, expression(lambda^MY))
  if(add_M & !confounding) text(Ms[1], Ms[2]+2*g, expression(lambda^M))

  # theta to X segment
  arrows(Xs[1], Xs[2]+2*g -v, Xs[1], Xs[2]+g+v, col = arrcol, length = arrow_length)

  if(!confounding)arrows(Ys[1], Ys[2]+2*g -v, Ys[1], Ys[2]+g+v, col = arrcol, length = arrow_length)
  if(confounding) arrows((Ys[1]+Ms[1])/2, (Ms[2]+Ys[2])/2+2*g -v, Ms[1], Ms[2]+g+v, col = arrcol, length = arrow_length)
  if(confounding) arrows((Ys[1]+Ms[1])/2, (Ms[2]+Ys[2])/2+2*g -v, Ys[1], Ys[2]+g+v, col = arrcol, length = arrow_length)

  if(add_M) {
    if(!confounding) arrows(Ms[1], Ms[2]+2*g -v, Ms[1], Ms[2]+g+v, col = arrcol, length = arrow_length)
  }

  if(func){
    text(Xs[1] - L, Xs[2]+2*g, expression(paste(lambda^X, "~ Dirichlet(", alpha^X, ")")), cex = cex, col = textcol,  pos = 2)
    text(Ys[1] +R, Ys[2]+2*g, expression(paste(lambda^Y, "~ Dirichlet(", alpha^Y, ")")), cex = cex, col = textcol,  pos = 4)

  } else {
    text(Xs[1] - L, Xs[2]+2*g, expression(paste("Prob. of nodal type: ", lambda[0]^X, "=Pr(", theta^X, "=",theta[0]^X,")")), cex = cex, col = textcol,  pos = 2)
    if(!confounding) text(Ys[1] +R, Ys[2]+2*g, expression(paste("Prob. of nodal type: ", lambda[10]^Y, "=Pr(", theta^Y, "=",theta[10]^Y, ")")), cex = cex, col = textcol,  pos = 4)
    if(confounding) text(Ys[1] +R, Ys[2]+2*g, expression(paste("Prob. of nodal pair =Pr(", theta^Y, ",", theta^M, ")")), cex = cex, col = textcol,  pos = 4)
  }
  ###########################################################


  # alphas
  text(Xs[1], Xs[2]+3*g, expression(alpha^X))
  if(!confounding) text(Ys[1], Ys[2]+3*g, expression(alpha^Y))
  if(confounding) text(Ys[1]/2+ Ms[1]/2, Ys[2]/2+Ms[2]/2+3*g, expression(alpha^MY))
  if(add_M & !confounding) text(Ms[1], Ms[2]+3*g, expression(alpha^M))


  # alpha to lambda
  arrows(Xs[1], Xs[2]+3*g -v, Xs[1], Xs[2]+2*g+v, col = arrcol, length = arrow_length)
  if(!confounding) arrows(Ys[1], Ys[2]+3*g -v, Ys[1], Ys[2]+2*g+v, col = arrcol, length = arrow_length)
  if(confounding) arrows(Ys[1]/2+Ms[1]/2, Ys[2]/2+Ms[2]/2+3*g -v, Ys[1]/2+Ms[1]/2, Ys[2]/2+Ms[2]/2+2*g+v, col = arrcol, length = arrow_length)
  if(add_M & !confounding)
    arrows(Ms[1], Ms[2]+3*g -v, Ms[1], Ms[2]+2*g+v, col = arrcol, length = arrow_length)

  if(func){
    text(Xs[1] - L, Xs[2]+3*g, expression(paste(alpha^X, "~ InvGamma(1,1)")), cex = cex, col = textcol,  pos = 2)
    if(!confounding)     text(Ys[1] +R, Ys[2]+3*g, expression(paste(alpha^Y, "~ InvGamma(1,1)")), cex = cex, col = textcol,  pos = 4)
    if(confounding)      text(Ys[1]+R, Ys[2]+3*g, expression(paste(alpha^MY, "~ InvGamma(1,1)")), cex = cex, col = textcol,  pos = 4)

  } else {
    text(Xs[1] - L, Xs[2]+3*g, expression(paste("Prior on prob nodal type ", lambda^X)), cex = cex, col = textcol,  pos = 2)
    if(!confounding) text(Ys[1] +R, Ys[2]+3*g, expression(paste("Prior on prob nodal type ", lambda^Y)), cex = cex, col = textcol,  pos = 4)
    if(confounding) text(Ys[1] +R, Ys[2]+3*g, expression(paste("Prior on prob nodal pair")), cex = cex, col = textcol,  pos = 4)
  }

  if(add_M & !confounding) {
       if(func){
      text(Ms[1] +R*.5, Ms[2]+3*g, expression(paste(alpha^M, "~ InvGamma(1,1)")), cex = cex*.8, col = textcol,  pos = 4)
       } else {
      text(Ms[1] +R*.5, Ms[2]+3*g, expression(paste("Prior on prob nodal type ", lambda^M)), cex = cex*.8, col = textcol,  pos = 4)
    }}
}

# dag_3d()
# dag_3d(add_M = FALSE)
# dag_3d(func = TRUE)

# dag_3d(confounding = TRUE)
