(ns the-seasoned-clojurer.welcome-back-to-the-show)


(defn member?
  [a lat]
  (cond
   (empty? lat) false
   :else (or (= (first lat) a)
             (recur a (rest lat)))))


(declare two-in-a-row?)

;;; corecursion
(defn is-first?
  [a lat]
  (cond
   (empty? lat) false
   :else (= a (first lat))))


(defn two-in-a-row?
  [lat]
  (cond
   (empty? lat) false
   :else (or (is-first? (first lat) (rest lat))
             (recur (rest lat)))))


(defn is-first-b?
  [a lat]
  (cond
   (empty? lat) false
   :else (or (= a (first lat))
             (two-in-a-row? lat))))


(defn two-in-a-row?
  [lat]
  (cond
   (empty? lat) false
   :else (is-first-b? (first lat) (rest lat))))


(defn two-in-a-row-b?
  [preceeding lat]
  (cond
   (empty? lat) false
   :else (or (= preceeding (first lat))
             (recur (first lat) (rest lat)))))


(defn two-in-a-row?
  [lat]
  (cond
   (empty? lat) false
   :else (two-in-a-row-b? (first lat) (rest lat))))


(defn sum-of-prefixes-b
  [sonssf tup]
  (cond
   (empty? tup) ()
   :else (conj (sum-of-prefixes-b (+ sonssf (first tup)) (rest tup))
               (+ sonssf (first tup)))))


(defn sum-of-prefixes
  [tup]
  (cond
   (empty? tup) ()
   :else (sum-of-prefixes-b 0 tup)))


(defn pick
  [n lat]
  (cond
   (= 1 n) (first lat)
   :else (recur (dec n) (rest lat))))


(defn scramble-b
  [tup rev-pre]
  (cond
   (empty? tup) ()
   :else (conj (scramble-b (rest tup) (conj rev-pre (first tup)))
               (pick (first tup) (conj rev-pre (first tup))))))


(defn scramble
  [tup]
  (cond
   (empty? tup) ()
   :else (scramble-b tup ())))
