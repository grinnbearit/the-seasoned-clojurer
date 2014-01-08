(ns the-seasoned-clojurer.the-difference-between-men-and-boys
  (:refer-clojure :exclude [find])
  (:use [the-seasoned-clojurer.preface :only [atom? Y]]
        [the-seasoned-clojurer.take-cover :only [member?]]))


;;; Shared mutable state in Clojure is tightly controlled, access is through
;;; one of the 3 reference types atoms, refs and agents
(def x (atom nil))


;;; reset! can be used like set! in scheme but only on atoms
(reset! x :skins)


;;; @x refers to the value of x and not x, the atom
(defn gourmet
  [food]
  (conj () @x food))

(gourmet :onion)
;;; => (:onion :skins)

(reset! x :rings)

(gourmet :onion)
;;; => (:onion :rings)


(defn gourmand
  [food]
  (reset! x food)
  (conj () @x food))


(gourmand :potato)
;;; => (:potato :potato)

(gourmand :rice)
;;; => (:rice :rice)


(defn dinerR
  [food]
  (reset! x food)
  (conj () food :milkshake))


(dinerR :onion)
;;; => (:milkshake :onion)

(dinerR :pecanpie)
;;; => (:milkshake :pecanpie)

(gourmand :onion)
;;; => (:onion :onion)


(defn omnivore
  [food]
  (let [x (atom nil)]
    (reset! x food)
    (conj () @x food)))


(def gobbler
  (let [x (atom :minestrone)]
    (fn [food]
      (reset! x food)
      (conj () @x food))))


(def food (atom nil))


(defn glutton
  [x]
  (reset! food x)
  (conj () x :more x :more))


(glutton :garlic)
;;; => (:more :garlic :more :garlic)


(defn chez-nous
  []
  (let [a @food]
    (reset! food @x)
    (reset! x a)))


(glutton :garlic)
;;; => (:more :garlic :more :garlic)

(gourmand :potato)
;;; => (:potato :potato)

(chez-nous)


(defn sweet-tooth
  [food]
  (conj () :cake food))


;;; naming it lst to avoid shadowing `last`
(def lst (atom nil))


(defn sweet-toothL
  [food]
  (reset! lst food)
  (conj () :cake food))


(sweet-toothL :chocolate)
;;; => (:chocolate :cake)

(sweet-toothL :fruit)
;;; => (:fruit :cake)

(sweet-toothL :cheese)
;;; => (:cheese :cake)

(sweet-toothL :carrot)
;;; => (:carrot :cake)


(defn deep
  [m]
  (if (zero? m)
    :pizza
    (conj () (deep (dec m)))))


(def Ns (atom ()))


(defn deepR
  [n]
  (swap! Ns conj n)                     ; swap! is an atomic operation to do a compare and swap instead of writing a possibly stale value
  (deep n))


(def Rs (atom ()))


(defn deepR
  [n]
  (let [result (deep n)]
    (swap! Rs conj result)
    (swap! Ns conj n)
    result))


(deepR 3)
;;; => (((:pizza)))

(deepR 5)
;;; => (((((:pizza)))))

(deepR 3)
;;; => (((:pizza)))


(defn find
  [n Ns Rs]
  (letfn [(A [ns rs]
            (if (= n (first ns))
              (first rs)
              (recur (rest ns) (rest rs))))]

    (A @Ns @Rs)))


(defn deepM
  [n]
  (if (member? n @Ns)
    (find n Ns Rs)
    (deepR n)))

(swap! Ns rest)
(swap! Rs rest)


(defn deepM
  [n]
  (if (member? n @Ns)
    (find n Ns Rs)
    (let [result (deep n)]
      (swap! Rs conj result)
      (swap! Ns conj n)
      result)))


(deepM 6)
;;; => ((((((:pizza))))))


(defn deep
  [m]
  (if (zero? m)
    :pizza
    (conj () (deepM (dec m)))))


(deepM 9)
;;; => (((((((((:pizza)))))))))


(def deepM
  (let [Rs (atom ())
        Ns (atom ())]
    (fn [n]
      (if (member? n @Ns)
        (find n Ns Rs)
        (let [result (deep n)]
          (swap! Rs conj result)
          (swap! Ns conj n)
          result)))))

(deepM 16)
;;; => ((((((((((((((((:pizza))))))))))))))))


(defn find
  [n Ns Rs]
  (letfn [(A [ns rs]
            (cond (empty? ns)
                  :f

                  (= n (first ns))
                  (first rs)

                  :else
                  (recur (rest ns) (rest rs))))]

    (A @Ns @Rs)))


(def deepM
  (let [Rs (atom ())
        Ns (atom ())]
    (fn [n]
      (let [exists (find n Ns Rs)]
        (if (atom? exists)
          (let [result (deep n)]
            (swap! Rs conj result)
            (swap! Ns conj n)
            result)
          exists)))))


(defn length
  [l]
  (if (empty? l)
    0
    (inc (length (rest l)))))


(def length
  (let [h (atom (constantly 0))]
    (reset! h (fn [l]
                (if (empty? l) 0 (inc (@h (rest l))))))
    @h))


(defn L
  [length]
  (fn [l]
    (if (empty? l)
      0
      (inc (length (rest l))))))


(def length
  (let [h (atom (constantly 0))]
    (reset! h (L #(@h %)))
    @h))


(defn Y!
  [L]
  (let [h (atom (constantly ()))]
    (reset! h (L #(@h %)))
    @h))


;;; Since Clojure differentiates betweens values and refs, something like `letrec` is not permitted
;;; `h` would refer to its original value instead of the newly set one

;;; this version of letrec is a workaround which creates atoms instead of vars

(defmacro letrec
  [recspecs & body]
  `(let ~(vec (mapcat (fn [[name value]]
                        `[~name (atom nil)
                          ~'_ (reset! ~name ~value)])
                      (partition 2 recspecs)))
     ~@body))


(defn Y-bang
  [f]
  (letrec
   [h (f #(@h %))]
   @h))


(def length (Y! L))


(defn D
  [depth*]
  (fn [s]
    (cond (empty? s)
          1

          (atom? (first s))
          (depth* (rest s))

          :else
          (max (inc (depth* (first s)))
               (depth* (rest s))))))


(def depth* (Y! D))


(def biz
  (let [x (atom 0)]
    (fn [f]
      (swap! x inc)
      (fn [a]
        (if (= a x) 0 (f a))))))


;;; unlike scheme or common lisp, clojure on the jvm has very constrained tail recursion
;;; in this case both `Y` and `Y!` cause a stack overflow
