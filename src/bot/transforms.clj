(ns bot.transforms
  (:require [midje.sweet :refer :all]
            [uncomplicate.fluokitten.core :refer [fmap! fold bind op bind! fmap]]
            [uncomplicate.neanderthal.internal
             [api :as api :refer [Blas VectorMath]]]
            [uncomplicate.neanderthal
             [native :refer [dge]]
             [math :refer [sin cos]]]))


(defn rotation [yaw pitch roll]
  (letfn [(rotate-x [angle]
            (dge 4 4 [1     0             0         0
                      0 (cos angle) (- (sin angle)) 0
                      0 (sin angle) (cos angle)     0
                      0     0             0         1] {:layout :row}))
          (rotate-y [angle]
            (dge 4 4 [(cos angle)     0 (sin angle) 0
                      0               1     0       0
                      (- (sin angle)) 0 (cos angle) 0
                      0               0     0       1] {:layout :row}))
          (rotate-z [angle]
            (dge 4 4 [(cos angle) (- (sin angle)) 0 0
                      (sin angle) (cos angle)     0 0
                      0               0           1 0
                      0               0           0 1] {:layout :row}))]
    (mm (rotate-x yaw) (rotate-y pitch) (rotate-z roll))))


(defn translation [x y z]
  (dge 4 4 [1 0 0 x
            0 1 0 y
            0 0 1 z
            0 0 0 1] {:layout :row}))
