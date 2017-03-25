(ns core.page-rank-spec
  (:require [speclj.core :refer :all]
            [core.page-rank :as a]))

(describe
  "should get neighbour weight"
  (letfn [(weight=
            ([in out]
             (should=
               out
               (a/count-weights in 1)))
            ([in my-weight out]
             (should=
               out
               (a/count-weights in my-weight))))]
    (it "nop"
      (weight= {} []))
    (it "should find count-weights [count 1 my-weight 1]"
      (weight=
        #{"mif.vu.lt"}
        [["mif.vu.lt" 1]]))
    (it "should find count-weights [count 2 my-weight 1]"
      (weight=
        #{"mif.vu.lt" "google.com"}
        [["mif.vu.lt" 1/2]
         ["google.com" 1/2]]))
    (it "should find count-weights [count 3 my-weight 1]"
      (weight=
        #{"mif.vu.lt" "google.com" "onemillionetherpage.com"}
        [["mif.vu.lt" 1/3]
         ["google.com" 1/3]
         ["onemillionetherpage.com" 1/3]]))
    (it "should find count-weights [count 2 my-weight rand]"
      (let [factor (rand)
            div (/ factor 2)]
        (weight=
          #{"mif.vu.lt" "google.com"}
          factor
          [["mif.vu.lt" div]
           ["google.com" div]])))))

(describe
  "should merge neighbours"
  (letfn [(merge-weights=
            [in out]
            (should=
              out
              (a/merge-weights in)))]
    (it "nop"
      (merge-weights= {} {}))
    (it "should sum two neighbours"
      (merge-weights=
        [["google.com" 1]
         ["google.com" 2]]
        {"google.com" 3}))
    (it "should sum multi"
      (merge-weights=
        [["google.com" 2]
         ["mif.vu.lt" 15]
         ["onemillionetherpage.com" 1]
         ["google.com" 1]
         ["onemillionetherpage.com" 3]
         ["mif.vu.lt" 25]]
        {"google.com" 3
         "mif.vu.lt" 40
         "onemillionetherpage.com" 4}))))

