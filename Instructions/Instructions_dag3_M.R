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
    "matchQualityFn_noConf"=list(
      qualFn="matchQualityFn_noConf",
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
    )
  ),
  dags = list(
    "dag_bat"=list(
      measurement=list(
        "measM" = c("V")
      ),
      combining = c("probMatch"),
      matchQuality = list(
        "matchQualityFn_noConf"=list(
            qualFn="matchQualityFn_noConf",
            options= list(
              calipers=c(0.1,0.01,0.001,0.0001,0.00001)
            )
          )
        )
    ),
    "dag_bat_ixn"=list(
      measurement=list(
        "measM" = c("V")
      ),
      combining = c("probMatch"),
      matchQuality = list(
        "matchQualityFn_noConf"=list(
          qualFn="matchQualityFn_noConf",
          options= list(
            calipers=c(0.1,0.01,0.001,0.0001,0.00001)
          )
        )
      )
    ),
    "dag_bird"=list(
      measurement=list(
        "measM" = c("V")
      ),
      combining = c("probMatch"),
      matchQuality = list(
        "matchQualityFn_noConf"=list(
          qualFn="matchQualityFn_noConf",
          options= list(
            calipers=c(0.1,0.01,0.001,0.0001,0.00001)
          )
        )
      )
    ),
    "dag_bird_ixn"=list(
      measurement=list(
        "measM" = c("V")
      ),
      combining = c("probMatch"),
      matchQuality = list(
        "matchQualityFn_noConf"=list(
          qualFn="matchQualityFn_noConf",
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
