(ns the-seasoned-clojurer.we-change-therefore-we-are-the-same
  (:refer-clojure :exclude [long])
  (:use [the-seasoned-clojurer.hop-skip-and-jump :only [letcc]]))


(defn kons
  [x koll]
  (atom [x koll]))


(defn kdr
  [koll]
  (@koll 1))


(defn kar
  [koll]
  (@koll 0))


(defn ->seq
  [koll]
  (loop [acc [] k koll]
    (if (nil? k)
      acc
      (recur (conj acc (kar k)) (kdr k)))))


(defn set-kdr
  [koll tail]
  (swap! koll (fn [[H T]] [H tail])))


(def kounter (atom nil))
(def set-kounter (atom nil))


(def konsC
  (let [N (atom 0)]
    (reset! kounter (fn [] @N))
    (reset! set-kounter (fn [x] (reset! N x)))
    (fn [x y]
      (swap! N inc)
      (kons x y))))


(defn lots
  [m]
  (if (zero? m)
    nil
    (kons :egg (lots (dec m)))))


(defn lenkth
  [l]
  (if (empty? @l)
    0
    (inc (lenkth (kdr l)))))


(defn add-at-end
  [l]
  (if (nil? (kdr l))
    (konsC (kar l) (kons :egg nil))
    (konsC (kar l) (add-at-end (kdr l)))))


(->seq (add-at-end (lots 3)))
;;; => [:egg :egg :egg :egg]


(defn add-at-end-too
  [l]
  (letfn [(A [ls]
            (if (nil? (kdr ls))
              (set-kdr ls (kons :egg nil))
              (recur (kdr ls))))]
    (A l)
    l))


(@set-kounter 0)
;;; => 0

(->seq (add-at-end-too (lots 3)))
;;; => [:egg :egg :egg :egg]


(defn kons
  [kar kdr]
  (fn [selector]
    (selector kar kdr)))


(defn kar
  [c]
  (c (fn [a d] a)))


(defn kdr
  [c]
  (c (fn [a d] d)))


(defn bons
  [kar]
  (let [kdr (atom ())]
    (fn [selector]
      (selector (fn [x] (reset! kdr x))
                kar
                @kdr))))


(defn kar
  [c]
  (c (fn [s a d] a)))


(defn kdr
  [c]
  (c (fn [s a d] d)))


(defn set-kdr
  [c x]
  ((c (fn [s a d] s)) x))


(defn kons
  [a d]
  (let [c (bons a)]
    (set-kdr c d)
    c))


(def dozen (lots 12))

(def bakers-dozen (add-at-end dozen))

(def bakers-dozen-too (add-at-end-too dozen))

(def bakers-dozen-again (add-at-end dozen))


(defn eklist?
  [ls1 ls2]
  (cond (nil? ls1)
        (nil? ls2)

        (nil? ls2)
        false

        :else
        (and (= (kar ls1) (kar ls2))
             (recur (kdr ls1) (kdr ls2)))))


(defn same?
  [c1 c2]
  (let [t1 (kdr c1)
        t2 (kdr c2)]
    (set-kdr c1 1)
    (set-kdr c2 2)
    (let [v (= (kdr c1) (kdr c2))]
      (set-kdr c1 t1)
      (set-kdr c2 t2)
      v)))


(defn last-kons
  [ls]
  (if (nil? (kdr ls))
    ls
    (recur (kdr ls))))


(def long (lots 12))


(set-kdr (last-kons long) long)
;; => #fn


(defn finite-lenkth
  [p]
  (letcc infinite
         (letfn [(sl [x]
                   (kdr x))

                 (qk [x]
                   (kdr (kdr x)))

                 (C [p q]
                   (cond (same? p q)
                         (infinite nil)

                         (nil? q)
                         0

                         (nil? (kdr q))
                         1

                         :else
                         (+ (C (sl p) (qk q)) 2)))]
           (if (nil? p)
             0
             (inc (C p (kdr p)))))))
