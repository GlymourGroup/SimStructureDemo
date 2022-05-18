list(
  C = list(
    parents=c(),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1
      )
    )
  ),
  C2 = list(
    parents=c(),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        C=0.1,
        U=0.1
      )
    )
  ),
  V = list(
    parents=c("C","C2"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1,
        C=0.2,
        C2=0.1
      )
    )
  ),
  M = list(
    parents=c("V"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1,
        V=0.5
      )
    )
  ),
  Y = list(
    parents=c("C","C2","M"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1,
        C=0.1,
        C2=0.05,
        M=0.3
      )
    )
  )
)
