
;; [[file:~/programming/timetabling/timetabling.org::*code%20-%20encoding%20cardinality%20constraints][code\ -\ encoding\ cardinality\ constraints:1]]

(ns timetabling.cardinality
  (:require  [timetabling.logic :refer :all]
             [clojure.set :refer [union]]))


;;not used because at-least-k and exactly-k aren't simple adaptions of
;;the formula
#_(defn at-most-k 
  "returns cnf form of the constraint that at most k of the variables
  are true see http://www.carstensinz.de/papers/CP-2005.pdf"
  [xs k]
  (let [n (count xs) ;;convert from 1-based indexing to 0-based
        s (into [] (for [i (range n)]
                     (into [] (for [j (range k)]
                                (symbol (str "s" i j))))))]
    ((comp flatten concat)
     [#{(gen-var (nth xs 0) false) (gen-var (get-in s [0 0]))}]
     (for [j (range 1 k)]
       #{(gen-var (get-in s [0 j]) false)})
     (for [i (range 1 (dec n))]
       (concat
        [#{(gen-var (nth xs i) false)
           (gen-var (get-in s [i 0]))}
         #{(gen-var (get-in s [(dec i) 0]) false)
           (gen-var (get-in s [i 0]))}]
         (for [j (range 1 k)]
           [#{(gen-var (nth xs i) false)
              (gen-var (get-in s [(dec i) (dec j)] false))
              (gen-var (get-in s [i j]))}
            #{(gen-var (get-in s [(dec i) j]) false)
              (gen-var (get-in s [i j]))}])
         [#{(gen-var (nth xs i) false)
             (gen-var (get-in s [(dec i) (dec k)]) false)}]))
     [#{(gen-var (nth xs (dec n)) false)
        (gen-var (get-in s [(- n 2) (dec k)]) false)}])))


(defn generate-unique-vars
  ([n] (generate-unique-vars n "guv"))
  ([n prfx] (generate-unique-vars n prfx true));;initially set to true?
  ([n prfx b]
    (repeatedly n (comp #(gen-var % b) 
                        (partial gensym prfx)))))


(def base-case-clauses
  {:at-most (fn [a b cs]
              [#{(negate a) (negate b) (second cs)}
               #{(negate a) (first cs)}
               #{(negate b) (first cs)}])
   :at-least (fn [a b cs]
               [#{a b (negate (first cs))}
                #{a (negate (second cs))}
                #{b (negate (second cs))}])
   :between (fn [a b cs]
              [#{(negate a) (negate b) (second cs)}
               #{(negate a) (first cs)}
               #{(negate b) (first cs)}
               #{a b (negate (first cs))}
               #{a (negate (second cs))}
               #{b (negate (second cs))}])})
;;(dec n) auf n erhöhen und in h-merge entsprechend anpassen
;;s-merge gleichbehalten und damit hoffentlich letzten bug fixen......
(def recursive-case-clauses
  {:at-most (fn [n cs ds es]
              (for [i (range n)]
              [#{(negate (nth ds (inc i)))
                 (negate (nth es i))
                 (nth cs (+ 2 (* 2 i)))}
               #{(negate (nth ds (inc i)))
                 (nth cs (inc (* 2 i)))}
               #{(negate (nth es i))
                 (nth cs (inc (* 2 i)))}])) ;;kann sein dass 1 und 0 based indexing
   ;;hier probleme macht ....
   ;;erstmal alles mit 2*i auf 2*i + 1 erhöht
   :at-least (fn [n cs ds es]
               (for [i (range n)]
                 [#{(nth ds (inc i))
                    (negate (nth cs (+ 2 (* 2 i))))}
                  #{(nth es i)
                    (negate (nth cs (+ 2 (* 2 i))))}
                  #{(nth ds (inc i))
                    (nth es i)
                    (negate (nth cs (inc (* 2 i))))}]))
   :between (fn [n cs ds es]
              (for [i (range n)]
              [#{(negate (nth ds (inc i)))
                 (negate (nth es i))
                 (nth cs (+ 2 (* 2 i)))}
               #{(negate (nth ds (inc i)))
                 (nth cs (inc (* 2 i)))}
               #{(negate (nth es i))
                 (nth cs (inc (* 2 i)))}
               #{(nth ds (inc i))
                 (negate (nth cs (+ 2 (* 2 i))))}
               #{(nth es i)
                 (negate (nth cs (+ 2 (* 2 i))))}
               #{(nth ds (inc i))
                 (nth es i)
                 (negate (nth cs (inc (* 2 i))))}]))})

(defn split-even-odd [s]
  [(map (partial nth s) (range 1 (count s) 2))
   (map (partial nth s) (range 0 (count s) 2))])


(defn h-merge-single [type a b]
  (let [cs (generate-unique-vars 2 "c")]
    [cs ((type base-case-clauses) a b cs)]))


(defn h-merge [type as bs]
  (if (= (count as) 1)
    (do (assert (= (count bs) 1))
        (h-merge-single type (first as) (first bs)))
    (let [n (* 2 (count as))
          cs (generate-unique-vars n "c")
          [as-even as-odd] (split-even-odd as)
          [bs-even bs-odd] (split-even-odd bs)
          [ds s-odd] (h-merge type as-odd bs-odd)
          [es s-even] (h-merge type as-even bs-even)]
      [(concat [(first ds)] (rest (butlast cs)) [(last es)])
       (->> ((type recursive-case-clauses) (dec (count as)) cs ds es)
            (apply concat s-odd s-even))])))


(defn h-sort-length-2 [type [a b]]
  (h-merge type [a] [b]))

(defn h-sort [type as]
  (if (= (count as) 2)
    (h-sort-length-2 type as)
    (let [n (quot (count as) 2)
          [ds s-d] (h-sort type (take n as))
          [d's s-d'] (h-sort type (drop n as))
          [cs s-m] (h-merge type ds d's)]
      [cs (concat s-d s-d' s-m)])))



(defn s-merge-single [type a b]
  (let [cs (generate-unique-vars 2 "c")]
    [cs ((type base-case-clauses) a b cs)]))

(defn s-merge
  "as and bs are sequences of vars of length n
   c is sequence of vars of length n+1
   returns set of clauses
   see http://www.cs.upc.edu/~erodri/webpage/papers/sat09-card.pdf"
  [type as bs]
  (if (= (count as) 1)
    (do (assert (= (count bs) 1))
        (s-merge-single type (first as) (first bs)))
    (let [cs (generate-unique-vars (inc (count as)) "c")
          [as-even as-odd] (split-even-odd as)
          [bs-even bs-odd] (split-even-odd bs)
          [ds s-odd] (s-merge type as-odd bs-odd)
          [es s-even] (s-merge type as-even bs-even)]
      [(into [] (cons (first ds) (rest cs)))
       (->> ((type recursive-case-clauses)
             (quot (count as) 2)
             cs ds es)            
            (apply concat s-odd s-even))])))


(defn card
  "k-cardinality network Sequence A of length n = m*k with k power of
  two returns [c s] c sequence of length k and S set of clauses"
  [type as k]
  (if (= (count as) k)
    (h-sort type as)
    (let [[ds s-d] (card type (take k as) k)
          [d's s-d'] (card type (drop k as) k)
          [cs s-m] (s-merge type ds d's)]
      [(butlast cs)
       (concat s-d s-d' s-m)])))

(defn next-power-of-2 [p]
  (if (== p 1) 2
      (->> (/ (Math/log p) (Math/log 2))
           double
           Math/ceil
           (Math/pow 2)
           int)))
;;TODO sind noch bugs drin zum beispiel
;;(get-assignment (at-least (map (comp gen-var #(str "a" %)) (range 5)) 3))
;; das funktioniert jetzt mit der Einführung der Zeile da unten
;; dafür aber das nicht mehr nur wenn die zeile wieder gelöscht wird, mist
;;(count (filter (fn [[n v]] (and (<= (count (str n)) 4) v)) (get-assignment (at-least (map (comp gen-var #(str "a" %)) (range 5)) 2))))x
(defn at-least-buggy [as p]
  (cond
    (== p 0) []
    (== p (count as)) (mapv (fn [s] #{s}) as)
    :else 
    (let [n (count as)
          k (next-power-of-2 p)
          r (rem n k)
          additional-vars
          (if (= 0 r) [] (generate-unique-vars (- k r) "a" false))
          as (concat as additional-vars)
          [cs s] (card :at-least as k)]
      (concat s [#{(nth cs (dec p))}]))))



;;TODO auch bei at-most stimmt noch etwas wirklich nicht
;;(get-assignment (concat (at-most (map (comp gen-var #(str "a" %)) (range 5)) 3) (map (comp (fn [s] #{s}) gen-var #(str "a" %)) (range 4))))
;;das oben funktioniert nicht, weil dort der branch mit at-least genommen wird, at-most selbst scheint gut zu funktionieren
(defn at-most [as p]
  (cond
    (== p 0) (mapv (comp (fn [s] #{s}) negate) as)
    (== p (count as)) []
    #_#_(> p (/ (count as) 2)) (at-least
                            (map negate as)
                            (- (count as) p))
    :else
    (let [n (count as)
          k (next-power-of-2 p)
          k (if (= k p) (* 2 k) k)
          r (rem n k)
          additional-vars
          (if (= 0 r) [] (generate-unique-vars (- k r) "a" false)) ;??
          as (concat as additional-vars)
          [cs s] (card :at-most as k)]
      (concat s [#{(negate (nth cs p))}]))))

(defn at-least
  "cheating with at-most here :)"
  [as p]
  (at-most (map negate as) (- (count as) p)))

(defn at-most-h-sort [as p]
 (let [[cs s] (h-sort :at-most as)]
    (concat s [#{(negate (nth cs p))}])))


(defn in-between [as p q]
  (let [n (count as)
        k (next-power-of-2 q)
        k (if (= k q) (* 2 k) k)
        r (rem n k)
        additional-vars
        (if (= 0 r) [] (generate-unique-vars (- k r) "a" false)) ;??
        as (concat as additional-vars)
        [cs s] (card :between as k)]
    (concat s [#{(nth cs (dec p))} #{(negate (nth cs q))}])))


(defn exactly-buggy [as k]
  (in-between as k k))

(defn exactly [as k]
  (concat (at-most as k) (at-least as k)))

;; code\ -\ encoding\ cardinality\ constraints:1 ends here
