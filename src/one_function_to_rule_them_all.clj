(ns one-function-to-rule-them-all)

;COPY THIS TO REPL
(use 'one-function-to-rule-them-all)
(require 'one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat '() a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [str1 str2] (str str1 " " str2)) a-seq)))

(defn my-interpose [sep a-seq]
  (if (empty? a-seq)
    '()
    (rest (reduce (fn [coll elem] (conj coll sep elem)) [] a-seq))))

(defn my-count [a-seq]
  (reduce (fn [a pointless] (inc a)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [coll elem] (conj coll elem)) '() a-seq))

(defn min-max-element [a-seq]
  (let [minmax
        (fn [coll elem] [(min elem (first coll)) (max elem (last coll))])
        ]
    (reduce minmax [(first a-seq) (first a-seq)] a-seq)))


(defn min-max-element2 [a-seq]
  (let [lo (reduce min a-seq)
        hi (reduce max a-seq)]
    [lo hi]))

(defn insert [sorted-seq n]
  (loop [head-seq [] tail-seq sorted-seq]
    (if(or (nil? (first tail-seq)) (< n (first tail-seq)))
      (concat head-seq (conj tail-seq n))
      (recur (conj head-seq (first tail-seq)) (rest tail-seq)))))


(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn toggle [a-set elem]
  (if(contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (concat (reduce toggle #{} a-seq)))

(defn minus
  ([x]    (- x))
  ([x y]  (- x y)))

(defn count-params [& params]
  (reduce (fn [x y] (inc x)) 0 params))

(defn my-* [& params]
  (reduce * 1 params))

(defn pred-and
  ([] (fn[elem] true))
  ;if no params are given to pred-and return function that allways return true
  ([pred]
    ;this constructor can be used by passing only one predicate to pred-and
    ;it is allso used by last constructor in reduce located below.
    ;in reduces usage parameter 'pred' is actually the method 'twoInOne'
    (fn [elem] (pred elem)))
  ([pred1 pred2 & preds]
    (let [twoInOne (fn [elem] (and (pred1 elem) (pred2 elem)))]
    (reduce pred-and twoInOne preds)))
  ;reduce first round (and (pred1 elem) (pred2 elem)
  ;second round  ...  (and ('pred-and' result above) (pred3 elem))
  ;third round   ...  (and ('pred-and' result above) (pred4 elem))...
  ;...n:th round ...  (and ('pred-and' result above) (predn elem))
  )



;usage:
;(filter (pred-and) [1 0 -2])                                       ;=> (1 0 -2)
;(filter (pred-and pos? odd?) [1 2 -4 0 6 7 -3])                    ;=> (1 7)
;(filter (pred-and number? integer? pos? even?) [1 0 -2 :a 7 "a" 2]);=> (0 2)



(defn my-map [f a-seq]
  [:-])