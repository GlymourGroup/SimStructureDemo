list(
  truth=list(
    n=100000
  ),
  measurement=list(
    "measCM_both"=list(
      measFn = "measurement1",
      options=list(
        cohorts=list(
          "younger"=list(
            obs=c("V","M","C"),
            n=1000
          ),
          "older"=list(
            obs=c("C","M","Y"),
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
  matchQuality=list(
    "matchQualityFn_conf"=list(
      qualFn="matchQualityFn_conf",
      options=list(
        testCalipers=(0.1)^(-10:10),
        qualMetric="euclidian"
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
    "dag_dromedary_strongconf"=list(
      measurement=list(
        "measCM_both" = c("V",
                    "VC")
      ),
      combining = c("probMatch"),
      matchQuality = list(
        "matchQualityFn_conf"=list(
            qualFn="matchQualityFn_conf",
            options= list(
              calipers=c(0.1,0.01,0.001,0.0001,0.00001)
            )
          )
        )
    )
  ),
  effect = list(outcomes="Y",
                do="V")
)
