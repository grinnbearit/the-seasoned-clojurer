(ns the-seasoned-clojurer.take-cover
  (:use [the-seasoned-clojurer.welcome-back-to-the-show :only [pick]]))


(defn Y
  [f]
  ((fn [g] (g g))
   (fn [h]
     (f (fn [x] ((h h) x))))))


(defn multirember
  [a lat]
  ((Y (fn [mr]
        (fn [lat]
          (cond
           (empty? lat) ()
           (= a (first lat)) (mr (rest lat))
           :else (conj (mr (rest lat)) (first lat))))))
   lat))


(def length
  (Y (fn [len]
       (fn [l]
         (cond
          (empty? l) 0
          :else (inc (len (rest l))))))))


(defn multirember                       ; clojure doesn't require letrec since anonymous functions can be
  [a lat]                               ; named, this name exists only within the scope of their body
  ((fn mr [lat]
     (cond
      (empty? lat) ()
      (= a (first lat)) (mr (rest lat))
      :else (conj (mr (rest lat)) (first lat))))
   lat))


(defn multirember                       ; in case the name is needed outside the body but not globally
  [a lat]                               ; letfn is useful
  (letfn [(mr [lat]
            (cond
             (empty? lat) ()
             (= a (first lat)) (mr (rest lat))
             :else (conj (mr (rest lat)) (first lat))))]
    (mr lat)))


(defn rember-f
  [test?]
  (fn [a l]
    (cond
     (empty? l) ()
     (test? (first l) a) (rest l)
     :else (conj ((rember-f test?) a (rest l))
                 (first l)))))


(def rember-= (rember-f =))


(defn multirember-f
  [test?]
  (fn [a l]
    (cond
     (empty? l) ()
     (test? (first l) a) ((multirember-f test?) a (rest l))
     :else (conj ((rember-f test?) a (rest l)) (first l)))))


(defn multirember-f
  [test?]
  (fn m-f [a l]
    (cond
     (empty? l) ()
     (test? (first l) a) m-f
     :else (conj (m-f a (rest l)) (first l)))))


(defn member?
  [a lat]
  (cond
   (empty? lat) false
   (= (first lat) a) true
   :else (recur a (rest lat))))


(defn member?                           ; recur always jumps to the lexically closest function, in
  [a lat]                               ; this case the anonymous inner fn and so no name is needed
  ((fn [lat]
     (cond
      (empty? lat) false
      (= (first lat) a) true
      :else (recur (rest lat))))
   lat))


(defn union
  [set1 set2]
  (cond
   (empty? set1) set2
   (member? (first set1) set2) (recur (rest set1) set2)
   :else (conj (union (rest set1) set2) (first set1))))


(defn union
  [set1 set2]
  (letfn [(U [set]
            (cond
             (empty? set) set2
             (member? (first set) set2) (recur (rest set))
             :else (conj (U (rest set)) (first set))))]
    (U set1)))


(defn union
  [set1 set2]
  (letfn [(M? [a lat]
            (cond
             (empty? lat) false
             (= (first lat) a) true
             :else (recur a (rest lat))))

          (U [set]
            (cond
             (empty? set) set2
             (M? (first set) set2) (recur (rest set))
             :else (conj (U (rest set)) (first set))))]
    (U set1)))


(defn union
  [set1 set2]
  (letfn [(M? [a lat]
            (letfn [(N? [lat]
                      (cond
                       (empty? lat) false
                       (= (first lat) a) true
                       :else (recur (rest lat))))]
              (N? lat)))

          (U [set]
            (cond
             (empty? set) set2
             (M? (first set) set2) (recur (rest set))
             :else (conj (U (rest set)) (first set))))]
    (U set1)))


(defn two-in-a-row?
  [lat]
  (letfn [(W? [preceeding lat]
            (cond
             (empty? lat) false
             :else (or (= preceeding (first lat))
                       (recur (first lat) (rest lat)))))]
    (cond
     (empty? lat) false
     :else (W? (first lat) (rest lat)))))


(defn sum-of-prefixes
  [tup]
  (letfn [(S [sss tup]
            (cond
             (empty? tup) ()
             :else (conj (S (+ (first tup) sss) (rest tup))
                         (+ (first tup) sss))))]
    (S 0 tup)))


(defn scramble
  [tup]
  (letfn [(P [tup rp]
            (cond
             (empty? tup) ()
             :else (conj (P (rest tup) (conj rp (first tup)))
                         (pick (first tup) (conj rp (first tup))))))]
    (P tup ())))
