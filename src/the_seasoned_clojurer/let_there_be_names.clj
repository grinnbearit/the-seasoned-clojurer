(ns the-seasoned-clojurer.let-there-be-names
  (:use [the-seasoned-clojurer.preface :only [atom?]]
        [the-seasoned-clojurer.welcome-back-to-the-show :only [pick]]
        [the-seasoned-clojurer.hop-skip-and-jump :only [letcc]]))


(defn leftmost
  [l]
  (cond (atom? (first l))
        (first l)

        :else
        (recur (first l))))


(defn leftmost
  [l]
  (cond (empty? l)
        ()

        (atom? (first l))
        (first l)

        :else
        (recur (first l))))


(defn leftmost
  [l]
  (cond (empty? l)
        ()

        (atom? (first l))
        (first l)

        (atom? (leftmost (first l)))
        (recur (first l))

        :else
        (recur (rest l))))


(defn leftmost
  [l]
  (cond (empty? l)
        ()

        (atom? (first l))
        (first l)

        :else
        (let [a (leftmost (first l))]   ; Clojure has its own variant of let, with fewer parens as usual
          (cond (atom? a)
                a

                :else
                (recur (rest l))))))


(defn rember1*
  [a l]
  (cond (empty? l)
        ()

        (atom? (first l))
        (cond (= a (first l))
              (rest l)

              :else
              (conj (rember1* a (rest l)) (first l)))

        (= (rember1* a (first l)) (first l))
        (conj (rember1* a (rest l)) (first l))

        :else
        (conj (rest l) (rember1* a (first l)))))


(defn rember1*
  [a l]
  (letfn [(R [l]
            (cond (empty? l)
                  ()

                  (atom? (first l))
                  (cond (= a (first l))
                        (rest l)

                        :else
                        (conj (R (rest l)) (first l)))

                  (= (R (first l)) (first l))
                  (conj (R (rest l)) (first l))

                  :else
                  (conj (rest l) (R (first l)))))]
    (R l)))


(defn rember1*
  [a l]
  (letfn [(R [l]
            (cond (empty? l)
                  ()

                  (atom? (first l))
                  (cond (= a (first l))
                        (rest l)

                        :else
                        (conj (R (rest l)) (first l)))

                  :else
                  (let [av (R (first l))]
                    (cond (= av (first l))
                          (conj (R (rest l)) (first l))

                          :else
                          (conj (rest l) av)))))]
    (R l)))


(defn depth*
  [l]
  (cond (empty? l)
        1

        (atom? (first l))
        (recur (rest l))


        (> (depth* (rest l))
           (inc (depth* (first l))))
        (depth* (rest l))

        :else
        (inc (depth* (first l)))))


(defn depth*
  [l]
  (cond (empty? l)
        1

        (atom? (first l))
        (recur (rest l))


        :else
        (let [a (inc (depth* (first l)))
              d (depth* (rest l))]
          (cond (> d a)
                d

                :else
                a))))


(defn depth*
  [l]
  (cond (empty? l)
        1

        (atom? (first l))
        (recur (rest l))

        :else
        (let [a (inc (depth* (first l)))
              d (depth* (rest l))]
          (if (> d a) d a))))


(defn depth*
  [l]
  (cond (empty? l)
        1

        (atom? (first l))
        (recur (rest l))

        :else
        (max (inc (depth* (first l)))
             (depth* (rest l)))))


(defn scramble
  [tup]
  (letfn [(P [tup rp]
            (cond
             (empty? tup) ()

             :else (let [rp (conj rp (first tup))]
                     (conj (P (rest tup) rp)
                           (pick (first tup) rp)))))]
    (P tup ())))


(defn lm
  [l out]
  (cond (empty? l)
        ()

        (atom? (first l))
        (out (first l))

        :else
        (do (lm (first l) out)          ; `do` works just as well as `let` with no bindings
            (lm (rest l) out))))


(defn leftmost
  [l]
  (letcc skip
         (lm l skip)))


(defn leftmost
  [l]
  (letcc skip
         (letfn [(lm [l]
                   (cond (empty? l)
                         ()

                         (atom? (first l))
                         (skip (first l))

                         :else
                         (do (lm (first l))
                             (lm (rest l)))))]

           (lm l))))


(defn rm
  [a l oh]
  (cond (empty? l)
        (oh :no)

        (atom? (first l))
        (if (= a (first l))
          (rest l)
          (conj (rm a (rest l) oh) (first l)))

        :else
        (let [new-car (letcc oh (rm a (first l) oh))]
          (if (atom? new-car)
            (conj (rm a (rest l) oh) (first l))
            (conj (rest l) new-car)))))


(defn rember1*
  [a l]
  (let [new-l (letcc oh (rm a l oh))]
    (if (atom? new-l)
      l
      new-l)))


;;; clojure exposes java exceptions with the keyword try
;;; using trycc as a workaround

(defmacro trycc
  [name alpha beta]
  `(letcc success#
          (letcc ~name (success# ~alpha))
          ~beta))


(defn rember1*
  [a l]
  (trycc oh (rm a l oh) l))


(defn rm
  [a l oh]
  (cond (empty? l)
        (oh :no)

        (atom? (first l))
        (if (= a (first l))
          (rest l)
          (conj (rm a (rest l) oh) (first l)))

        :else
        (trycc oh2
               (conj (rest l) (rm a (first l) oh2))
               (conj (rm a (rest l) oh) (first l)))))
