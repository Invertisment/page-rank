(ns core.page-rank-spec
  (:require [speclj.core :refer :all]
            [core.page-rank :as a]))

(describe
  "should get neighbour weight"
  (letfn [(weight=
            [in out]
            (should=
              out
              (a/count-weights-single in)))]
    (list
      (it "nop"
          (weight= {} {}))
      (it "should find count-weights [count 1 my-weight 1]"
          (weight=
            #{"mif.vu.lt"}
            {"mif.vu.lt" 1}))
      (it "should find count-weights [count 2 my-weight 1]"
          (weight=
            #{"mif.vu.lt" "google.com"}
            {"mif.vu.lt" 1/2
             "google.com" 1/2}))
      (it "should find count-weights [count 3 my-weight 1]"
          (weight=
            #{"mif.vu.lt" "google.com" "onemillionetherpage.com"}
            {"mif.vu.lt" 1/3
             "google.com" 1/3
             "onemillionetherpage.com" 1/3})))))

(describe
  "should produce paths with local weight"
  (letfn [(weight=
            ([in out]
             (should=
               out
               (a/count-weights
                 #(map clojure.string/capitalize %)
                 in))))]
    (list
      (it "nop"
          (weight= {} ()))
      (it "should return mutated children [1]"
          (weight=
            {"mif.vu.lt" #{"aaa"}}
            (list {:from "mif.vu.lt"
                   :hrefs #{"Aaa"}})))
      (it "should return mutated children [2]"
          (weight=
            {"mif.vu.lt"
             #{"ccc" "BBB"}}
            (list {:from "mif.vu.lt"
                   :hrefs #{"Ccc" "Bbb"}}))))))

(describe
  "should produce map of paths with local weight"
  (letfn [(weight=
            ([in out]
             (should=
               out
               (a/flatten-weights in))))]
    (list
      (it "nop"
          (weight= () ()))
      #_(it "should return mutated children [1]"
            (weight=
              (list {:from "mif.vu.lt"
                     :hrefs {"google.com" 1/2
                             "ask.com" 1/3}})
              (list {:from "mif.vu.lt"
                     :to "google.com"
                     :change 1/2}
                    {:from "mif.vu.lt"
                     :to "ask.com"
                     :change 1/3}))))))

(describe
  "should produce list of all sites"
  (letfn [(sites=
            ([in out]
             (should=
               out
               (a/list-sites in (range)))))]
    (list
      (it "nop"
          (sites= {} {}))
      (it "should list keys [1]"
          (sites=
            {"mif.vu.lt" #{}
             "aaa.com" #{}}
            {"mif.vu.lt" 0
             "aaa.com" 1}))
      (it "should list mixed [2]"
          (sites=
            {"mif.vu.lt" #{"google.com" "bit.ly"}}
            {"mif.vu.lt" 0
             "google.com" 2
             "bit.ly" 1})))))

(describe
  "should merge neighbours"
  (letfn [(merge-weights=
            [in out]
            (should=
              out
              (a/merge-weights in)))]
    (list
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
             "onemillionetherpage.com" 4})))))

(describe
  "should find weights"
  (letfn [(merge-weights=
            [flat-weights sites-apriori sites-initial-value out]
            (should=
              out
              (a/weights-iteration flat-weights sites-apriori sites-initial-value)))]
    (list
      (it "nop"
          (merge-weights= () {} {} {}))
      (it "(B C D) -> A"
          (merge-weights=
            (list
              {:from "B", :to "A", :change 1}
              {:from "C", :to "A", :change 1}
              {:from "D", :to "A", :change 1})
            {"A" 1/4
             "B" 1/4
             "C" 1/4
             "D" 1/4}
            {"A" 0
             "B" 0
             "C" 0
             "D" 0}
            {"A" 3/4
             "B" 0
             "C" 0
             "D" 0}))
      (it "(B -> A C) (C -> A) (D -> A B C)"
          (merge-weights=
            (list
              {:from "B" :to "C" :change 1/2}
              {:from "B" :to "A" :change 1/2}
              {:from "C" :to "A" :change 1}
              {:from "D" :to "A" :change 1/3}
              {:from "D" :to "B" :change 1/3}
              {:from "D" :to "C" :change 1/3})

            {"A" 0.25
             "B" 0.25
             "C" 0.25
             "D" 0.25}
            {"A" 0
             "B" 0
             "C" 0
             "D" 0}
            {"A" 0.4583333333333333
             "B" 0.08333333333333333
             "C" 0.20833333333333331
             "D" 0}))
      (it "(B -> A C) (C -> A) (D -> A B C) with fractions"
          (merge-weights=
            (list
              {:from "B" :to "C" :change 1/2}
              {:from "B" :to "A" :change 1/2}
              {:from "C" :to "A" :change 1}
              {:from "D" :to "A" :change 1/3}
              {:from "D" :to "B" :change 1/3}
              {:from "D" :to "C" :change 1/3})

            {"A" 1/4
             "B" 1/4
             "C" 1/4
             "D" 1/4}
            {"A" 0
             "B" 0
             "C" 0
             "D" 0}

            {"A" 11/24
             "B" 1/12
             "C" 5/24
             "D" 0})))))

(describe
    "should find weights"
    (letfn [(merge-weights=
              [init-seq in out]
              (should=
                out
                (a/find-weights in init-seq)))]
      (list
        (it "nop"
            (merge-weights= (range) {} {}))
        (it "(B C D) -> A"
            (merge-weights=
              (repeat 1)
              {"B" #{"A"}
               "C" #{"A"}
               "D" #{"A"}}
              {"A" 3
               "B" 0
               "C" 0
               "D" 0}))
        (it "(B -> A C) (C -> A) (D -> A B C)"
              (merge-weights=
                (repeat 1/4)
                {"B" #{"A" "C"}
                 "C" #{"A"}
                 "D" #{"A" "B" "C"}}
                {"A" 0
                 "B" 0
                 "C" 0
                 "D" 0}
                )))))

