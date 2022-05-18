list(
  V = list(
    parents=c(),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1
      )
    )
  ),
  M = list(
    parents=c("V","M2"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1,
        V=0.4,
        M2=0.3
      )
    )
  ),
  M2 = list(
    parents=c("V"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1,
        V=0.3
      )
    )
  ),
  Y = list(
    parents=c("M","M2"),
    simFn="simLinear",
    simOptions = list(
      coefs=list(
        U=0.1,
        M=0.36,
        M2=0.36
      ),
      ixnCoefs=list(
        "M*M2"=list(
          size=0.1,
          var1="M",
          var2="M2"
        )
      )
    )
  )
)
