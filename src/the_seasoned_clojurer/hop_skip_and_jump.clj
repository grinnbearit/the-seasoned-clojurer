(ns the-seasoned-clojurer.hop-skip-and-jump
  (:use [the-seasoned-clojurer.take-cover :only [member?]]))


(defn intersect
  [set1 set2]
  (cond (empty? set1)
        ()

        (member? (first set1) set2)
        (conj (intersect (rest set1) set2) (first set1))

        :else
        (recur (rest set1) set2)))


(defn intersect
  [set1 set2]
  (letfn [(I [set]
            (cond (empty? set)
                  ()

                  (member? (first set) set2)
                  (conj (I (rest set)) (first set))

                  :else
                  (recur (rest set))))]
    (I set1)))


(defn intersectall
  [lset]
  (cond (empty? lset)
        ()

        :else
        (intersect (first lset) (intersectall (rest lset)))))


(defn intersectall
  [lset]
  (letfn [(A [lset]
            (cond (empty? (rest lset))
                  (first lset)

                  :else
                  (intersect (first lset) (A (rest lset)))))]

    (cond (empty? lset)
          ()

          :else
          (A lset))))


;;; Clojure doesn't provide a native letcc or callcc, using
;;; java exceptions as a replacement


(defmacro letcc
  [name & body]
  `(letfn [(~name [arg#]
             (throw (ex-info (str '~name) {:name '~name :value arg#})))]
     (try ~@body
          (catch clojure.lang.ExceptionInfo e#
            (if (= '~name (:name (ex-data e#)))
              (:value (ex-data e#))
              (throw e#))))))


(defn intersectall
  [lset]
  (letcc hop
   (letfn [(A [lset]
             (cond (empty? (first lset))
                   (hop ())

                   (empty? (rest lset))
                   (first lset)

                   :else
                   (intersect (first lset) (A (rest lset)))))]

     (cond (empty? lset)
           ()

           :else
           (A lset)))))


(defn intersect
  [set1 set2]
  (letfn [(I [set]
            (cond (empty? set)
                  ()

                  (member? (first set) set2)
                  (conj (I (rest set)) (first set))

                  :else
                  (recur (rest set))))]
    (cond (empty? set2)
          ()

          :else
          (I set1))))


(defn intersectall
  [lset]
  (letcc hop
   (letfn [(I [s1 s2]
             (letfn [(J [s1]
                       (cond (empty? s1)
                             ()

                             (member? (first s1) s2)
                             (conj (J (rest s1)) (first s1))

                             :else
                             (recur (rest s1))))]
               (cond (empty? s2)
                     (hop ())

                     :else
                     (J s1))))

           (A [lset]
             (cond (empty? (first lset))
                   (hop ())

                   (empty? (rest lset))
                   (first lset)

                   :else
                   (I (first lset) (A (rest lset)))))]

     (cond (empty? lset)
           ()

           :else
           (A lset)))))


(defn rember
  [a lat]
  (letfn [(R [lat]
            (cond (empty? lat)
                  ()

                  (= a (first lat))
                  (rest lat)

                  :else
                  (conj (R (rest lat) (first lat)))))]
    (R lat)))


(defn rember-beyond-lat
  [a lat]
  (letfn [(R [lat]
            (cond (or (empty? lat)
                      (= a (first lat)))
                  ()

                  :else
                  (conj (R (rest lat)) (first lat))))]

    (R lat)))


(defn rember-upto-last
  [a lat]
  (letcc skip
   (letfn [(R [lat]
             (cond (empty? lat)
                   ()

                   (= a (first lat))
                   (skip (R (rest lat)))

                   :else
                   (conj (R (rest lat)) (first lat))))]
     (R lat))))
