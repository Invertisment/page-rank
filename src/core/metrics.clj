(ns core.metrics)

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

