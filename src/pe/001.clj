(ns pe.001)

; ------ using brute force
(defn divisible?
  [n]
  (let [f (fn [d] (= 0 (rem n d)))]
    (or (f 5) (f 3)))) 

(defn solve
  [n]
  (reduce + (filter divisible? (range 1 n))))


; ------ using math
(defn mult-sum
  [n d]
  (let [m (quot n d)]
    (* (quot (* m (+ m 1)) 2) d))) 

(defn solve2
  [n]
  (let [f (partial mult-sum (- n 1))]
    (+ (f 3) (f 5) (- (f 15)))))

; ------- lein run -m pe.001
(defn -main [] (println (solve2 1000)))

