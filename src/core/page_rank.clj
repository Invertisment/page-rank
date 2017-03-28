(ns core.page-rank)

(defn count-weights-single [ngbs]
  (into {} (map
             (fn [title]
               [title (/ 1 (count ngbs))])
             ngbs)))

(defn count-weights [func sites]
  (map
    (fn [[title adjacent]]
      {:from title
       :hrefs (set (func adjacent))})
    sites))

(defn flatten-weights [nested-weights]
  (mapcat
    (fn [mapping]
      (let [{:keys [from hrefs]} mapping]
        (map
          (fn [[to change]]
            {:from from
             :to to
             :change change})
          hrefs)))
    nested-weights))

(defn list-sites [site-map weights]
  "produces site string list from links and site-map keys"
  (zipmap
    (concat
      (keys site-map)
      ((comp
         (partial apply concat)
         (partial map vec)
         vals)
       site-map))
    weights))

(defn merge-weights [wgts]
  (reduce
    #(merge-with + %1 %2)
    {}
    (map
      #(into {} [%])
      wgts)))

(defn weights-iteration
  ([flat-weights sites-apriori]
   (weights-iteration flat-weights sites-apriori sites-apriori))
  ([flat-weights sites-apriori initial-weights-value]
   #_(println flat-weights sites-apriori)
   (println sites-apriori)
   #_(println initial-weights-value)
   (reduce
     (fn [result weight]
       (let [{:keys [from to change]} weight]
         #_(println from to change)
         #_(println sites-apriori result)
         #_(println (get sites-apriori from))
         (update result to #(+ % (* change (get sites-apriori from))))))
     #_(zipmap
         (keys sites-apriori)
         (repeat 0))
     initial-weights-value
     flat-weights))
  )

(defn find-weights [sites init-weight-coll]
  (let [flat-weights (flatten-weights
                       (count-weights count-weights-single sites))
        #_all-sites-prepended #_(zipmap
                    (keys (list-sites sites init-weight-coll))
                    (repeat 0))
        #_all-sites #_(weights-iteration flat-weights sites-apriori)]
    (println flat-weights)
    (loop [sites-apriori
           (weights-iteration
             flat-weights
             (list-sites sites init-weight-coll)
             (zipmap
               (keys (list-sites sites init-weight-coll))
               (repeat 0)))
           counter 0]
      (let [sites-post-apriori (weights-iteration flat-weights sites-apriori)]
        (println "counter:" counter)
        (if
          (or
            (> counter 5000)
            (= sites-apriori sites-post-apriori))
          sites-apriori
          (recur
            sites-post-apriori
            (inc counter)))))))

