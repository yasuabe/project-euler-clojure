(ns util.primes)

(require '[clojure.core.match :refer [match]])

(defrecord Tree [ns phs])

(defn tree ([ns phs] (->Tree ns phs)))

(defn join-trees
  [t1 t2]
  (let [ns1 (:ns t1) ns2 (:ns t2)]
    (if (< (first ns1) (first ns2))
      (tree ns1 (cons t2 (:phs t1)))
      (tree ns2 (cons t1 (:phs t2))))))

(defn merge-ph
  [h1 h2]
  (match [h1 h2]
         [h :empty] h
         [:empty h] h
         :else
         (let [ns1 (:ns h1) ns2 (:ns h2)]
           (if (< (first ns1) (first ns2))
             (tree ns1 (cons h2 (:phs h1)))
             (tree ns2 (cons h1 (:phs h2)))))))

(defn ph-to-string
  [ph]
  (if (= ph :empty) "empty"
      (format "{:ns %s, :phs %s}"
              (apply list (take 3 (:ns ph)))
              (list (map ph-to-string (:phs ph))))))

(defn merge-all
  [phs]
  (if (empty? phs) :empty
      (let [ph1 (first phs) phr (rest phs)]
        (if (empty? phr) ph1
            (merge-ph (merge-ph ph1 (first phr)) (merge-all (rest phr)))))))

(defn merge-pair
  [phs]
  (if (empty? phs) :empty
      (let [h (first phs)
            t (rest phs)]
        (if (empty? t) h
            (let [h2 (first t) t2 (rest t)]
              (merge-ph (merge-ph h h2) (merge-pair t2)))))))

(defn spin
  [start wheel]
  (letfn [(ss [] (lazy-cat wheel (ss)))]
    (reductions + (cons start (ss)))))

(def candidates (lazy-cat '(2 3) (spin 5 '(2 4))))

(defn composites
  [ns]
  (letfn [(loop [ms] (lazy-seq (cons (* (first ns) (first ms)) (loop (rest ms)))))]
    (loop ns)))

(defn enqueue
  [ns q]
  (merge-ph (tree ns nil) q))

(defn sieve
  [ns ph]
  (let [[n & nt] ns]
    (if (= ph :empty)
      (lazy-seq (cons n (sieve nt (enqueue (composites ns) :empty))))
      (let [{[m & ms] :ns qs :phs} ph]
        (cond
          (< n m) (lazy-seq (cons n (sieve nt (enqueue (composites ns) ph))))
          (> n m) (sieve ns (enqueue ms (merge-all qs)))
          :else   (sieve nt (enqueue ms (merge-all qs))))))))

(def primes (sieve candidates :empty))



