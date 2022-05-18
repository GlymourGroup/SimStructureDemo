list(
  C = list(
    parents=c(),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.05
      )
    )
  ),
  V = list(
    parents=c("C"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.05,
        C=0.2
      )
    )
  ),
  M = list(
    parents=c("V"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.05,
        V=0.6
      )
    )
  ),
  Y = list(
    parents=c("C","M"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.05,
        C=0.2,
        M=0.5
      )
    )
  )
)
