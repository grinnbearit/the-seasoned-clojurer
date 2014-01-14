(ns absconding-with-the-jewels)


(defn deep
  [m]
  (if (zero? m)
    :pizza
    (conj () (deep (dec m)))))


(defn six-layers
  [p]
  (->> p
       (conj ())
       (conj ())
       (conj ())
       (conj ())
       (conj ())
       (conj ())))


(defn four-layers
  [p]
  (->> p
       (conj ())
       (conj ())
       (conj ())
       (conj ())))


;;; no true continuations in Clojure either so stuck for now
