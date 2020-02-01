(ns pe.003)

(require '[util.primes :refer [primes]])
(require '[util.numbers :refer [div-mod isqrt]])

(defn pr003
  [target]
  (loop [[p & ps] primes
         n target]
    (if (>= p (isqrt n)) n
        (let [[q r] (div-mod n p)]
          (if (= 0 r) (recur primes q) (recur ps n))))))

; ---- $ lein run -m pe.003
(defn -main [] (println (pr003 600851475143)))
