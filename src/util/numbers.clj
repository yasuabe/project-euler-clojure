(ns util.numbers)

(defn div-mod [n d] [(quot n d) (rem n d)])

(defn isqrt [n] (int (Math/sqrt n)))

