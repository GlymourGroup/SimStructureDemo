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
    ),
    "poolAndImpute"=list(
      combFn="poolAndImpute",
      options=list()
    )
  ),
  analysis=list(
    "V"=list(
      analysisFn="V",
      options=list()
    ),
    "VM"=list(
      analysisFn="VM",
      options=list()
      ),
    "VC"=list(
      analysisFn="VC",
      options=list()
    ),
    "VMC"=list(
      analysisFn="VMC",
      options=list()
    )
  ),
  dags = list(
    "dag_earthworm"=list(
      measurement=list(
        "measM" = c("V","VM")
      ),
      combining = c("probMatch","poolAndImpute")
    ),
    "dag_turtle"=list(
      measurement=list(
        "measM" = c("V","VM")
      ),
      combining = c("probMatch","poolAndImpute")
    ),
    "dag_bat"=list(
      measurement=list(
        "measM" = c("V","VM")
      ),
      combining = c("probMatch","poolAndImpute")
    ),
    "dag_dromedary"=list(
      measurement= list(
        "measM"=c("V","VM"),
        "measCM"=c("V","VM","VC","VMC")
      ),
      combining = c("probMatch","poolAndImpute")
    ),
    "dag_bactrian"=list(
      measurement= list(
        "measM"=c("V","VM"),
        "measCM"=c("V","VM","VC","VMC")
      ),
      combining = c("probMatch","poolAndImpute")
    )
  ),
  effect = list(outcomes="Y",
                do="V")
)
