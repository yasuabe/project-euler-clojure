(ns pe.002)

(def fib
  (let [fib-step (fn [[a b]] [b (+ a b)])]
    (map first (iterate fib-step [1 2]))))

(defn solve
  [n]
  (reduce + (filter even? (take-while (partial > n) fib))))

; --- $ lein run -m pe.002
(defn -main [] (println (solve 4000000)))

