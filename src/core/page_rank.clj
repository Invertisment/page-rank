(ns core.page-rank)

(defn abs [n] (max n (- n)))

(defn euclidean-distance [li-a li-b]
  (Math/sqrt (reduce
               +
               (map
                 (fn [a b]
                   (let [diff (- b a)] (* diff diff)))
                 li-a
                 li-b))))

(defn manhattan-distance [li-a li-b]
  (first (sort
           (comp - compare) ; least of all items
           (map
             #(Math/abs (- %1 %2))
             li-a
             li-b))))

(defn weights-seq-iteration
  ([relations weights zeros damping-factor-fn]
   (damping-factor-fn
     (reduce
       (fn [result relation]
         (let [{:keys [from to change]} relation]
           (update result to #(+ % (* change (get weights from))))))
       zeros
       relations))))

(defn apply-damping-factor-85
  [mappings]
  (into
    {}
    (map
      (fn [[k v]]
        [k (+
            (/
             (- 1 0.85)
             (count mappings))
            (* 0.85 v))])
      mappings)))

(defn produce-zeros [weights]
  (into {} (zipmap
             (keys weights)
             (repeat 0))))

(defn find-weights-seq [relations weights zeros df-fn distance-fn]
  (let
    [weights-next (weights-seq-iteration
                    relations
                    weights
                    zeros
                    df-fn)]
    (if
      (distance-fn
        (vals weights)
        (vals weights-next))
      (list weights-next)
      (lazy-seq (cons
                  weights
                  (find-weights-seq
                    relations
                    weights-next
                    weights-next
                    df-fn
                    distance-fn))))))
