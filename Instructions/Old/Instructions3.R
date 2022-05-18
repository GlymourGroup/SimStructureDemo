list(
  truth=list(
    n=100000
  ),
  measurement=list(
    "measM"=list(
      measFn = "measurement1",
      options=list(
        cohorts=list(
          "younger"=list(
            obs=c("V","M"),
            n=1000
          ),
          "older"=list(
            obs=c("M","Y"),
            n=1000
          )
        )
      )
    ),
    "measCM"=list(
      measFn = "measurement1",
      options=list(
        cohorts=list(
          "younger"=list(
            obs=c("V","C","M"),
            n=1000
          ),
          "older"=list(
            obs=c("M","C","Y"),
            n=1000
          )
        )
      )
    )
  ),
  combining=list(
    "probMatch"=list(
      combFn="probMatch",
      options=list(
        nMatches=1
      )
    )
  ),
  analysis=list(
    "V"=list(
      analysisFn="V",
      options=list()
    ),
    "VC"=list(
      analysisFn="VC",
      options=list()
    )
  ),
  dags = list(
    "dag_earthworm"=list(
      measurement=list(
        "measM" = c("V")
      ),
      combining = c("probMatch")
    ),
    "dag_turtle"=list(
      measurement=list(
        "measM" = c("V")
      ),
      combining = c("probMatch")
    ),
    "dag_bat"=list(
      measurement=list(
        "measM" = c("V")
      ),
      combining = c("probMatch")
    ),
    "dag_dromedary"=list(
      measurement= list(
        "measM"=c("V"),
        "measCM"=c("V","VC")
      ),
      combining = c("probMatch")
    )
  ),
  effect = list(outcomes="Y",
                do="V")
)
