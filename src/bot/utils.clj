(ns bot.utils
  (:require #_[uncomplicate.neanderthal.internal
             [api :as api :refer [Blas VectorMath]]]
            [uncomplicate.neanderthal
             #_[block :refer [buffer]]
             [core :refer [copy! transfer! mrows ncols submatrix view-vctr matrix? scal mm]]
             [native :refer [dge]]
             [math :refer [sin cos]]])
  (:import clojure.lang.ExceptionInfo))


(defn hstack
  ([m] m)
  ([m1 m2] 
   {:pre [(= (mrows m1) (mrows m2))]}
   (let [ret (dge (mrows m1) (+ (ncols m1) (ncols m2)))]
     (copy! m1 (submatrix ret 0 0 (mrows m1) (ncols m1)))
     (copy! m2 (submatrix ret 0 (ncols m1) (mrows m2) (ncols m2)))
     ret))
  ([m1 m2 & ms]
   (reduce hstack (hstack m1 m2) ms)))


(defn vstack
  ([m] m)
  ([m1 m2]
   {:pre [(= (ncols m1) (ncols m2))]}
   (let [ret (dge (+ (mrows m1) (mrows m2)) (ncols m1))]
     (copy! m1 (submatrix ret 0 0 (mrows m1) (ncols m1)))
     (copy! m2 (submatrix ret (mrows m1) 0 (mrows m2) (ncols m2)))
     ret))
  ([m1 m2 & ms]
   (reduce vstack (vstack m1 m2) ms)))


(defn reshape [mat m n]
  {:pre [(= (* m n) (* (mrows mat) (ncols mat)))]}
  (let [ret (dge m n)]
    (transfer! (view-vctr mat) ret)
    ret))


(defn mul
  ([a b]
   (cond
     (and (number? a) (number? b)) (* a b)
     (and (number? a) (matrix? b)) (scal a b)
     (and (matrix? a) (number? b)) (scal b a)
     (and (matrix? a) (matrix? b)) (mm a b)
     :else
     (throw (Exception. "unsupported type combination. Add it!")))))
