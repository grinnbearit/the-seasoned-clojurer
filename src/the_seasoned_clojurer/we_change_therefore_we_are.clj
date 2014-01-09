(ns the-seasoned-clojurer.we-change-therefore-we-are
  (:refer-clojure :exclude [find])
  (:use [the-seasoned-clojurer.preface :only [atom?]]
        [the-seasoned-clojurer.hop-skip-and-jump :only [letcc]]
        [the-seasoned-clojurer.ready-set-bang :only [find]]))


(def deepM
  (let [Rs (atom ())
        Ns (atom ())]
    (letfn [(D [m]
              (if (zero? m)
                :pizza
                (conj () (D (dec m)))))]
      (fn [n]
        (let [exists (find n Ns Rs)]
          (if (atom? exists)
            (let [result (D n)]
              (swap! Rs conj result)
              (swap! Ns conj n)
              result)
            exists))))))


(def deepM
  (let [Rs (atom ())
        Ns (atom ())]
    (letfn [(D [m]
              (if (zero? m)
                :pizza
                (conj () (deepM (dec m)))))]
      (fn [n]
        (let [exists (find n Ns Rs)]
          (if (atom? exists)
            (let [result (D n)]
              (swap! Rs conj result)
              (swap! Ns conj n)
              result)
            exists))))))


(def deepM
  (let [Rs (atom ())
        Ns (atom ())
        D (fn [m]
            (if (zero? m)
              :pizza
              (conj () (deepM (dec m)))))]
    (fn [n]
      (let [exists (find n Ns Rs)]
        (if (atom? exists)
          (let [result (D n)]
            (swap! Rs conj result)
            (swap! Ns conj n)
            result)
          exists)))))


(def deepM
  (let [Rs (atom ())
        Ns (atom ())]
    (fn [n]
      (let [exists (find n Ns Rs)]
        (if (atom? exists)
          (let [result (#(if (zero? %) :pizza (conj () (deepM (dec %)))) n)]
            (swap! Rs conj result)
            (swap! Ns conj n)
            result)
          exists)))))


(def deepM
  (let [Rs (atom ())
        Ns (atom ())]
    (fn [n]
      (let [exists (find n Ns Rs)]
        (if (atom? exists)
          (let [result (if (zero? n) :pizza (conj () (deepM (dec n))))]
            (swap! Rs conj result)
            (swap! Ns conj n)
            result)
          exists)))))


(def conjC
  (let [N (atom 0)]
    (fn [x y]
      (swap! N inc)
      (conj x y))))


(defn deep
  [m]
  (if (zero? m)
    :pizza
    (conjC () (deep (dec m)))))


(deep 5)
;;; => (((((:pizza)))))


(def counter (atom nil))


(def conjC
  (let [N (atom 0)]
    (reset! counter (fn [] @N))
    (fn [x y]
      (swap! N inc)
      (conj x y))))


(deep 5)
;;; => (((((:pizza)))))

(deep 7)
;;; => (((((((:pizza)))))))


(defn supercounter
  [f]
  (letfn [(S [n]
            (if (zero? n)
              (f n)
              (do (f n)
                  (recur (dec n)))))]
    (S 1000)
    (@counter)))


(supercounter deep)
;;; => 500512


(def set-counter (atom nil))


(def conjC
  (let [N (atom 0)]
    (reset! counter (fn [] @N))
    (reset! set-counter (fn [x] (reset! N x)))
    (fn [x y]
      (swap! N inc)
      (conj x y))))


(@set-counter 0)
;;; => 0

(supercounter deep)
;;; => 500500


(def deepM
  (let [Rs (atom ())
        Ns (atom ())]
    (fn [n]
      (let [exists (find n Ns Rs)]
        (if (atom? exists)
          (let [result (if (zero? n) :pizza (conjC () (deepM (dec n))))]
            (swap! Rs conj result)
            (swap! Ns conj n)
            result)
          exists)))))


(@set-counter 0)
;;; => 0

(deepM 5)
;;; => (((((:pizza)))))

(deepM 7)
;;; => (((((((:pizza)))))))

(supercounter deepM)
;;; => 1000


(defn rember1*C
  [a l]
  (letfn [(R [l oh]
            (cond (empty? l)
                  (oh :no)

                  (atom? (first l))
                  (if (= a (first l))
                    (rest l)
                    (conjC (R (rest l) oh)))

                  :else
                  (let [new-f (letcc oh (R (first l) oh))]
                    (if (atom? new-f)
                      (conjC (R (rest l) oh) (first l))
                      (conjC (rest l) new-f)))))]
    (let [new-l (letcc oh (R l oh))]
      (if (atom? new-l)
        l
        new-l))))


(@set-counter 0)
;;; => 0

(rember1*C :noodles '((:food) :more (:food)))
;;; => ((:food) :more (:food))


(defn rember1*
  [a l]
  (letfn [(R [l]
            (cond (empty? l)
                  ()

                  (atom? (first l))
                  (if (= a (first l))
                    (rest l)
                    (conjC (R (rest l)) (first l)))

                  :else
                  (let [av (R (first l))]
                    (if (= av (first l))
                      (conjC (R (rest l)) av)
                      (conjC (rest l) av)))))]
    (R l)))


(@set-counter 0)
;;; => 0

(conjC (conjC (conjC () (conjC () :food))
              :more)
       (conjC () :food))
;;; => ((:food) :more (:food))

(@set-counter 0)
;;; => 0

(rember1*C :noodles '((:food) :more (:food)))
;;; => ((:food) :more (:food))
