(ns the-seasoned-clojurer.preface)


(def atom?
  (complement coll?))


(defn Y
  [le]
  ((fn [f]
     (f f))
   (fn [f]
     (le (fn [x]
           ((f f) x))))))
