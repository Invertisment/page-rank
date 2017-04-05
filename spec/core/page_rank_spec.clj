(ns core.page-rank-spec
  (:require [speclj.core :refer :all]
            [core.page-rank :as a]))

(describe
    "should find distance"
    (letfn [(distance=
              [in1 in2 out]
              (should=
                out
                (a/euclidean-distance in1 in2)))]
      (list
        (it "[0] [1] -> 1"
            (distance=
              [1]
              [0]
              1.0))
        (it "[5] [4] -> 1"
            (distance=
              [5]
              [4]
              1.0))
        (it "[5 4] [4 5] -> (sqrt 2)"
            (distance=
              [5 4]
              [4 5]
              (Math/sqrt 2)))
        (it "[4 5 13 2] [2 7 8 9] -> (sqrt 82)"
            (distance=
              [4 5 13 2]
              [2 7 8 9]
              (Math/sqrt 82))))))

(describe
  "should transform weights using damping factor"
  (letfn [(find-weights=
            [relations weights zeros damping-factor-fn out]
            (should=
              out
              (a/weights-seq-iteration
                relations
                weights
                zeros
                damping-factor-fn)))]
    (list
      (it "nop"
          (find-weights= {} {} {} identity {}))
      (it "(B C D) -> A"
          (find-weights=
            (list
              {:from "B" :to "A" :change 1}
              {:from "C" :to "A" :change 1}
              {:from "D" :to "A" :change 1})
            {"A" 1
             "B" 1
             "C" 1
             "D" 1}
            {"A" 0
             "B" 0
             "C" 0
             "D" 0}
            identity
            {"A" 3
             "B" 0
             "C" 0
             "D" 0}))
      (it "(B C D) -> A"
            (find-weights=
              (list
                {:from "B" :to "A" :change 1}
                {:from "C" :to "A" :change 1}
                {:from "D" :to "A" :change 1})
              {"A" 1
               "B" 1
               "C" 1
               "D" 1}
              {"A" 0
               "B" 0
               "C" 0
               "D" 0}
              (fn [mappings]
                (into
                  {}
                  (map (fn [[k v]]
                         [k (* v v)]) mappings)))
              {"A" 9
               "B" 0
               "C" 0
               "D" 0}))

      (it "(B -> A C) (C -> A) (D -> A B C) df"
            (find-weights=
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
              a/apply-damping-factor-85
              {"A" 0.42708333333333326,
               "B" 0.10833333333333334,
               "C" 0.21458333333333332,
               "D" 0.037500000000000006})))))

(describe
    "should find weights seq"
    (letfn [(find-weights=
              [relations weights zeros df-fn out]
              (should=
                out
                (take
                  (count out)
                  (a/find-weights-seq
                    relations
                    weights
                    zeros
                    df-fn
                    =))))]
      (list
        (it "nop"
            (find-weights= {} {} {} identity '({})))
        (it "(B C D) -> A"
            (find-weights=
              (list
                {:from "B" :to "C" :change 1/2})
              {"A" 0.25
               "B" 0.25
               "C" 0.25
               "D" 0.25}
              {"A" 0
               "B" 0
               "C" 0
               "D" 0}
              identity
              (list {"A" 0.25
                     "B" 0.25
                     "C" 0.25
                     "D" 0.25}
                    {"A" 0
                     "B" 0
                     "C" (/ 0.25 2)
                     "D" 0})))
        (it "(B -> A C) (C -> A) (D -> A B C)"
            (find-weights=
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
              a/apply-damping-factor-85
              (list
                {"A" 0.25
                 "B" 0.25
                 "C" 0.25
                 "D" 0.25}
                {"A" 0.42708333333333326,
                 "B" 0.10833333333333334,
                 "C" 0.21458333333333332,
                 "D" 0.037500000000000006}))))))

(describe
    "should find weights seq"
    (letfn [(find-weights=
              [relations weights zeros df-fn out]
              (should=
                out
                (take
                  (count out)
                  (a/find-weights-seq
                    relations
                    weights
                    zeros
                    df-fn
                    =))))]
      (list
        (it "nop"
            (find-weights= {} {} {} identity '({})))
        (it "(B C D) -> A"
            (find-weights=
              (list
                {:from "B" :to "C" :change 1/2})
              {"A" 0.25
               "B" 0.25
               "C" 0.25
               "D" 0.25}
              {"A" 0
               "B" 0
               "C" 0
               "D" 0}
              identity
              (list {"A" 0.25
                     "B" 0.25
                     "C" 0.25
                     "D" 0.25}
                    {"A" 0
                     "B" 0
                     "C" (/ 0.25 2)
                     "D" 0})))
        (it "(B -> A C) (C -> A) (D -> A B C)"
            (find-weights=
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
              a/apply-damping-factor-85
              (list
                {"A" 0.25
                 "B" 0.25
                 "C" 0.25
                 "D" 0.25}
                {"A" 0.42708333333333326,
                 "B" 0.10833333333333334,
                 "C" 0.21458333333333332,
                 "D" 0.037500000000000006}))))))

(describe
    "should converge"
    (letfn [(find-weights=
              [convergence-fn expected-count]
              (should=
                expected-count
                (count
                  (a/find-weights-seq
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
                    a/apply-damping-factor-85
                    convergence-fn))))]
      (list
        (it "= -> 269"
            (find-weights=
              =
              269))
        (it "euclidean [0.1] -> 45"
            (find-weights=
              (fn [a b]
                (identity
                 (let [distance (a/euclidean-distance a b)]
                   #_(println distance)
                   (> 0.1 distance))))
              45))
        (it "manhattan [0.001] -> 84"
            (find-weights=
              (fn [a b]
                (> 0.001 (a/manhattan-distance a b)))
              84)))))
