(ns util.primes
  (:require [clojure.core.match :refer [match]]))

; PairingHeap = :empty | Tree ns phs
(defrecord Tree [ns phs])

(defn tree ([ns phs] (->Tree ns phs)))

(defn head [tree] (first (:ns tree)))

(defn small? [tree1 tree2] (< (head tree1) (head tree2)))

(defn attach [t1 t2] (tree (:ns t1) (cons t2 (:phs t1))))

(defn merge-ph
  [h1 h2]
  (match [h1 h2]
         [h :empty] h
         [:empty h] h
         :else (if (small? h1 h2) (attach h1 h2) (attach h2 h1))))

(defn merge-all
  [phs]
  (if (empty? phs) :empty
      (let [[ph1 & phr] phs]
        (if (empty? phr) ph1
            (merge-ph (merge-ph ph1 (first phr)) (merge-all (rest phr)))))))

(defn ph-to-string
  [ph]
  (if (= ph :empty) "empty"
      (format "{:ns %s, :phs %s}"
              (apply list (take 3 (:ns ph)))
              (list (map ph-to-string (:phs ph))))))

; functions below generate lazy prime sequence using paring heap.

(defn spin
  [start wheel]
  (letfn [(ss [] (lazy-cat wheel (ss)))]
    (reductions + (cons start (ss)))))

(def candidates (lazy-cat '(2 3) (spin 5 '(2 4))))

(defn composites
  [ns]
  (letfn [(loop [ms]
            (lazy-seq (cons (* (first ns) (first ms)) (loop (rest ms)))))]
    (loop ns)))

(defn enqueue [ns q] (merge-ph (tree ns nil) q))

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

