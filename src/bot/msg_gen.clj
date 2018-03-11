(ns bot.msg-gen
  {:doc "
ROS message generator.
"}
  (:require [primitive-math :as p]))


(def field-types #{"bool" "int8" "uint8" "int16" "uint16" "int32" "unit32"
                   "int64" "uint62" "float32" "float64" "string" "time" "duration"})



(def ctype->java
  {"bool" java.lang.Boolean
   "int8" java.lang.Byte
   "uint8" java.lang.Byte
   "int16" java.lang.Short
   "uint16" java.lang.Short
   "int32" java.lang.Integer
   "unit32" java.lang.Integer
   "int64" java.lang.Long
   "uint62" java.lang.Long
   "float32" java.lang.Float
   "float64" java.lang.Floati
   "string" java.lang.String})

(defprotocol IROSMessage
  (serialize [this])
  (deserialize [this])
  (slots [this]))


(defrecord Header [^java.lang.Float x ^java.lang.Float y]
  IROSMessage
  (serialize [this])
  (deserialize [this s])
  (slots [this] (keys this)))



(defmulti stringify type)
(defmethod stringify Message [x]
  (s/join "\n" (map stringify x)))
(defmethod stringify Iterable [x])
(defmethod stringify java.lang.Boolean [x])
(defmethod stringify java.lang.Byte [x])





(defrecord x [^double x ^double y]
  )

