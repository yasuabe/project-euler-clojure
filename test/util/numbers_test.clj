(ns util.numbers-test
  (:require [clojure.test :refer :all]
            [util.numbers :refer :all]))

(require '[clojure.test.check.generators :as gen])
(require '[clojure.test.check.properties :as prop])
(require '[clojure.test.check.clojure-test :refer [defspec]])

(def divisor-remainder-pair
  (gen/bind
   (gen/fmap inc gen/nat)
   (fn [d]
     (gen/fmap
      (fn [r] [d r])
      (gen/large-integer* {:min 0 :max (- d 1)})))))

(defspec div-mod-works-for-any-nat
  (prop/for-all
   [[q [d r]] (gen/tuple gen/nat divisor-remainder-pair)]
   (let [n (+ (* q d) r)]
     (= (div-mod n d) [q r]))))

(defspec squared-isqrt-does-not-exceeds-given-value
  (prop/for-all
   [n gen/nat]
   (let [r (isqrt n) s (+ r 1) pow (fn [m] (* m m))]
     (and (<= (pow r) n) (< n (pow s))))))

