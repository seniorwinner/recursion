(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? coll)
    nil
    (if (singleton? coll)
      (first coll)
      (my-last (rest coll)))))

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (if (singleton? a-seq)
      (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (let [f (first a-seq)
        r (rest  a-seq)]
    (if (empty? r)
      f
      (seq-max f (longest-sequence r)))))


(defn my-filter [pred? a-seq]
  (let [f (first a-seq)
        r (rest  a-seq)]
    (if (empty? a-seq)
      []
      (let [fr (pred? f)
            rr (my-filter pred? r)]
        (if fr
          (cons f rr)
          rr)))))

;;
(defn only-numbers? [coll]
  (cond
    (empty? coll)
    true
    (number? (first coll))
      (only-numbers? (rest coll))
    :else
      false))
;;

(defn sequence-contains? [elem a-seq]
  (let [f (first a-seq)
        r (rest  a-seq)]
    (cond
      (= elem f) true
      (empty? r)  false
      :else       (sequence-contains? elem r))))

(defn my-take-while [pred? a-seq]
  (let [f (first a-seq)
        r (rest  a-seq)]
    (cond
      (or (empty? a-seq) (not (pred? f))) []
      :else (cons f (my-take-while pred? r)))))

(defn my-drop-while [pred? a-seq]
  (let [f (first a-seq)
        r (rest  a-seq)]
    (cond
      (empty? a-seq) []
      (pred? f)  (my-drop-while pred? r)
      :else      a-seq)))

;;
(defn first-in [val seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) 0
    (= (first seq-1) val) 1
    (= (first seq-2) val) 2
    :else (first-in val (rest seq-1) (rest seq-2))))
;;

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))    true
    (not (= (first a-seq) (first b-seq))) false
    :else (seq= (rest a-seq) (rest b-seq))))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    []
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

;;
(defn indexed [a-seq]
  (let [indexes (range 0 (count a-seq))]
    (map vector indexes a-seq)))

(defn consecutives [a-seq]
  (map vector a-seq (rest a-seq)))

(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))
;;

(defn power [n k]
  (if (zero? k)
    1
    (* n (power  n (dec k)))))

(defn fib [n]
  (cond
    (== 0 n) 0
    (== 1 n) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times)
    []
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (>= 0 up-to)
    []
    (cons up-to (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))

(defn rotations [a-seq]
  (if (empty? a-seq)
    '()
    (reverse (map concat (rest (tails a-seq)) (rest (inits a-seq))))))

;;
(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
  (count-elem-helper 0 elem coll))
;;

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [f (first a-seq)
          r (rest  a-seq)]
      (my-frequencies-helper
        (if (contains? freqs f)
          (update freqs f inc)
          (assoc freqs f 1))
        r))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (let [f (first a-map)
        r (rest  a-map)]
    (if (empty? f)
      []
      (let [[k v] f]
        (concat (repeat v k) (un-frequencies r))))))

(defn my-take [n coll]
  (if (or (>= 0 n) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (if (or (>= 0 n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll))))


(defn halve [a-seq]
  (let [ll (count a-seq)
        hl (int (/ ll 2))]
    [(my-take hl a-seq) (my-drop hl a-seq)]))

(defn seq-merge [a-seq b-seq]
  (let [a (first a-seq)
        b (first b-seq)]
    (cond
      (empty? a-seq) b-seq
      (empty? b-seq) a-seq
      (<= a b)       (cons a (seq-merge (rest a-seq) b-seq))
      :else          (cons b (seq-merge  a-seq (rest b-seq))))))

(defn merge-sort [a-seq]
  (let [la (count a-seq)]
    (cond
      (<= la 1) a-seq
      :else (let [[l r] (halve a-seq)]
              (seq-merge (merge-sort l) (merge-sort r))))))

(defn monotomic? [a-seq]
  (cond
    (empty? a-seq)   false
    (apply >= a-seq) true
    (apply <= a-seq) true
    :else            false))

(defn map-monotomics [secs]
  (map (fn [x] [(monotomic? x) x]) secs))

(defn get-max-monotomic [msecs]
  (get (last (my-take-while (fn [x] (get x 0)) msecs)) 1))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq)
    '()
    (let [x (get-max-monotomic
              (map-monotomics (rest (inits a-seq))))
          lx (count x)
          r (my-drop lx a-seq)]
      (if (empty? r)
        [x]
        (cons x (split-into-monotonics r))))))

(defn get-at [a-seq pos]
  (let [d (inc pos)
        la (my-take d a-seq)
        l (my-take pos la)
        a (last la)
        r (my-drop d a-seq)]
    [a (concat l r)]))

(defn get-all-splits [a-seq]
  (let [l (count a-seq)]
    (cond
      (>= 1 l) (seq a-seq)
      :else (map (fn [x] (get-at a-seq x)) (range l)))))

(defn add-to-start [x b]
  (vec (map
         (fn [vv] (concat (seq [x]) vv))
         b)))

(defn merge-sets [v]
  (vec (apply concat v)))

(defn permutations-helper [a-seq]
  (let [l (count a-seq)]
    (cond
      (>= 1 l) [(seq a-seq)]
      :else (merge-sets (map
              (fn [[x1 x2]]
                (add-to-start x1 (permutations-helper x2)))
              (get-all-splits a-seq))))))

(defn permutations [a-set]
  (let [l (count a-set)]
    (cond
      (== 0 l) '(())
      :else (seq (permutations-helper (seq a-set))))))

(defn powerset-helper [a-seq skip]
  (let [l (count a-seq)
        next-skip (inc skip)]
    (cond
      (or (>= 1 l) (>= skip l)) [(set a-seq)]
      :else (merge-sets
              [(powerset-helper (second (get-at a-seq skip)) skip)
               (powerset-helper a-seq next-skip)]))))

(defn powerset [a-set]
  (let [l (count a-set)
        es #{}]
    (cond
      (== 0 l) #{es}
      :else (set (conj (powerset-helper (seq a-set) 0) es)))))

