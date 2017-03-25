(defproject tho "0.1.0-SNAPSHOT"
  :min-lein-version "2.5.0"
  :description "FIXME: write description"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :profiles {

             :dev [{:dependencies [[speclj "3.3.2"]
                                   [peridot "0.4.4"]]
                    :plugins [[speclj "3.3.2"]]}]}
  :test-paths ["spec"])
