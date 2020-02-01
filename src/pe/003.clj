(ns pe.003)

(require '[util.primes :refer [primes]])

(defn pr003
  [target]
  (loop [[p & ps] primes
         n target]
    (if (>= p (int (Math/sqrt n))) n+1 ; TODO optimize terminal condition later
        (let [r (rem n p) q (quot n p)] ; TODO duplication 
          (if (= 0 r) (recur primes q) (recur ps n))))))

; ---- $ lein run -m pe.003
(defn -main [] (println (pr003 600851475143)))
