(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
      (first a-seq)
      (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond (empty? seq1)
          (empty? seq2)
        (empty? seq2)
          false
        (not (= (first seq1) (first seq2)))
          false
        :else
          (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n 0
         a-seq a-seq]
    (cond (empty? a-seq)
            nil
          (pred (first a-seq))
            n
          :else
            (recur (inc n) (rest a-seq)))))


(defn avg [a-seq]
  (loop [sum 0
         n 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum n)
      (recur (+ sum (first a-seq)) (inc n) (rest a-seq)))))

(defn parity [a-seq]
  (loop [result #{}
         a-seq a-seq]
    (if (empty? a-seq)
      result
      (recur (if (contains? result (first a-seq))
               (disj result (first a-seq))
               (conj result (first a-seq)))
               (rest a-seq)))))

(defn fast-fibo [n]
  (loop [fib-n-1 0
         fib-n 1
         n n]
    (cond (zero? n)
            0
          (= n 1)
            fib-n
          :else
            (recur fib-n (+ fib-n-1 fib-n) (dec n)))))


(defn cut-at-repetition [a-seq]
  (loop [result '[]
         a-seq a-seq]
    (cond (empty? a-seq)
            result
          (= (first result) (first a-seq))
            result
          :else
            (recur (conj result (first a-seq)) (rest a-seq)))))

