(ns the-seasoned-clojurer.the-difference-between-men-and-boys
  (:use [the-seasoned-clojurer.preface :only [atom?]]
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
