(ns core.page-rank)

(defn count-weights [ngbs my-weight]
  (map
    (fn [title]
      [title (/ my-weight (count ngbs))])
    ngbs))

(defn merge-weights [wgts]
  (reduce
    #(merge-with + %1 %2)
    {}
    (map
      #(into {} [%])
      wgts)))

