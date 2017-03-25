(ns core.page-rank-spec
  (:require [speclj.core :refer :all]
            [core.page-rank :as a]))

(describe
  "should get neighbours"
  (it "should get one neighbour from map"
      (should=
        (a/smth)
        3)))

