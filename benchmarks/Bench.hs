import Vinyl.Fields()
import Criterion.Main

main = defaultMain [
  bgroup "Vinyl.Fields"
    [ bench "1" $ nf   length [1..1000::Int]
    , bench "2" $ whnf length [1..1000::Int]
    ]
  ]

