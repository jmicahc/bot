(ns demo_neanderthal.core
  (:require [midje.sweet :refer :all]
            [uncomplicate.fluokitten.core :refer [fmap! fold bind op bind! fmap]]
            [uncomplicate.neanderthal.internal
             [api :as api :refer [Blas VectorMath]]]
            [uncomplicate.neanderthal
             [block :refer [buffer]]
             [core :refer :all]
             [native :refer :all]
             [math :refer [sin cos]]])
  (:import [java.nio ByteBuffer ByteOrder]
           [uncomplicate.neanderthal.internal.api Matrix RealNativeMatrix]
           clojure.lang.ExceptionInfo))


(defn rotation [yaw pitch roll]
  (letfn [(rotate-x [angle]
            (dge 4 4 [1     0             0         0
                      0 (cos angle) (- (sin angle)) 0
                      0 (sin angle) (cos angle)     0
                      0     0             0         1]))
          (rotate-y [angle]
            (dge 4 4 [(cos angle)     0 (sin angle) 0
                      0               1     0       0
                      (- (sin angle)) 0 (cos angle) 0
                      0               0     0       1]))
          (rotate-z [angle]
            (dge 4 4 [(cos angle) (- (sin angle)) 0 0
                      (sin angle) (cos angle)     0 0
                      0               0           1 0
                      0               0           0 1]))]
    (mm (rotate-x yaw) (rotate-y pitch) (rotate-z roll))))


(defn translation [x y z]
  (dge 4 4 [1 0 0 0
            0 1 0 0
            0 0 1 0
            x y z 1]))


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
     (and (matrix? a) (number? b)) (scal b a))))


(def v (dv [1 2 3]))


(mul (dge 4 4 (repeat 1.0)) 10)


(scal 2 (dge 4 4 (repeat 1)))



(def m (dge 10 10))


(defn transform [points tf]
  (let [ret (dge (mrows points) 4 (repeat 1))]
    (copy! points (submatrix ret 0 0 (mrows points) 3))
    (mm ret tf)))


(defn test-vctr-transfer []
  (let [x0 (dv 4)
        x1 (dv [1 2 3 0])
        x2 (dv [22 33 3 0])
        y0 (dv 4)
        y1 (dv [22 33])
        y2 (dv [22 33 3 0])
        m1 (dge 2 4)]
    #_(= (transfer! (float-array [1 2 3]) x0) x1)
    #_(transfer! (float-array [1 2 3]) x0)
    (transfer! x0 x1)
    #_(facts
     "Vector transfer tests."
     (transfer! (float-array [1 2 3]) x0) => x1
     (transfer! (double-array [1 2 3]) x0) => x1
     (transfer! (int-array [1 2 3 0 44]) x0) => x1
     (transfer! (long-array [1 2 3]) x0) => x1
     (seq (transfer! x1 (float-array 2))) => (seq (float-array [1 2]))
     (seq (transfer! x1 (double-array 2))) => (seq (double-array [1 2]))
     (transfer! y1 x0) => x2
     (transfer! x2 y0) => y2)))



(defmulti patch :op)

(cond value
  (:key value)  (assoc value :new-key new-value)
  (:key value)  (assoc value :new-kwy new-value))


(reduce + ())


(reduce + (range 100))


(defmethod patch "remove"
  ([state {:keys [path]}]
    (update-in state path dissoc)))


(defmethod patch "replace"
  ([state {:keys [path value]}]
    (assoc-in state path value))) 


(defmethod patch "move"
  ([state {:keys [from path]}]
    (let [value (get-in state from)]
      (-> state
          (update-in from dissoc)
          (assoc-in path value)))))

(defmethod patch "copy"
  ([state {:keys [from path]}]
    (let [value (get-in state)]
      (assoc-in state from value))))


(defmethod patch "test"
  ([state {:keys [path value]}]
    (= (get-in state path) value)))


(defn migrate [state patches]
  (reduce patch state patches))

(reduce + (range 10))

(facts
 "We create a few double vectors using different input methods."
 (dv 10) => (dv (repeat 10 0))
 (dv (dv (repeat 10 0))) => (dv (repeat 10 0)))


(facts
 "And here is how you create double general dense matrices."
 (dge 2 3) => (dge 2 3 (repeat 6 0))
 (dge 3 2 [1 2 3 4 5 6]) => (dge 3 2 '(1 2 3 4 5 6))
 (dge 2 3 (dge 2 3 (repeat 6 0))) => (dge 2 3 (repeat 6 0)))


(facts
  "Miscelaneous matrix functions."
  (let [a (dge 20 30 (range 600))]
    (= a (dge 20 30 (range 600))) => true
    (identical? a (dge 20 30 (range 600))) => false
    (entry a 15 16) => 335.0
    (mrows a) => 20
    (ncols a) => 30
    (trans (trans a)) => a
    (col a 2) => (dv (range 40 60))
    (row a 3) => (dv (range 3 600 20))
    (matrix? a) => true
    (matrix? (row a 4)) => false
    (zero a) => (dge 20 30)
    (submatrix a 11 12 2 3) => (dge 2 3 [251 252 271 272 291 292])
    (ax 2.0 (col a 0)) => (dv (range 0 40 2))
    a => (dge 20 30 (range 600))
    (scal! 2.0 (row a 3)) => (row a 3)
    a =not=> (dge 20 30 (range 600))))

(let [primitive-inc (fn ^double [^double x] (inc x))
      primitive-add2 (fn ^double [^double x ^double y] (+ x y))
      primitive-add3 (fn ^double [^double x ^double y ^double z] (+ x y z))
      primitive-multiply (fn ^double [^double x ^double y] (* x y))
      a (dge 2 3 (range 6))
      b (dge 2 3 (range 0 60 10))
      c (dge 2 3 (range 0 600 100))]
  (fact
   "You can change individual entries of any structure with fmap!. You can also
accumulate values with fold, or fold the entries."
   (fmap! primitive-inc a) => (dge 2 3 (range 1 7))
   a => (dge 2 3 (range 1 7))
   (fmap! primitive-add3 a b c) => (dge 2 3 [1 112 223 334 445 556])
   a => (dge 2 3 [1 112 223 334 445 556])
   (fold (fn ^double [^double x ^double y] (+ y x)) 0.0 b c) => 1650.0
   (fold c) => 1500.0
   (fold primitive-multiply 1.0 a) => 2.06397368128E12))
