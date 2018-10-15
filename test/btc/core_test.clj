(ns btc.core-test
  (:require [clojure.test :refer :all]
            [btc.core :refer :all]))

(deftest test-magic
  (testing "FIXME, I fail."
    (is (= (:magic-no (read-block "../data/blk00000.dat" 293))
           3652501241))))
